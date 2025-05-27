(ns c51cc.preprocessor-v2
  "Рефакторированный препроцессор C51 с использованием Transducers и Protocols.
   
   Архитектурные принципы:
   - Каждая директива препроцессора реализует Protocol
   - Обработка строк через композицию transducers
   - Функциональный подход без мутабельного состояния
   - Высокая тестируемость и расширяемость"
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]
            [clojure.spec.alpha :as s]))

;; ============================================================================
;; СПЕЦИФИКАЦИИ ДЛЯ ВАЛИДАЦИИ ДАННЫХ (C89 СОВМЕСТИМЫЕ)
;; ============================================================================

(s/def ::macro-name string?)
(s/def ::macro-body string?)
;; В C89 поддерживаются только простые макросы (строковые значения)
(s/def ::simple-macro string?)
;; C89 поддерживает ТОЛЬКО простые макросы вида: #define NAME [value]
(s/def ::macro-definition ::simple-macro)

(s/def ::defines (s/map-of ::macro-name ::macro-definition))
(s/def ::include-stack (s/coll-of string? :kind vector?))
(s/def ::include-paths (s/coll-of string?))
(s/def ::line-number pos-int?)
(s/def ::current-file string?)
(s/def ::errors (s/coll-of map?))

(s/def ::preprocessor-state
  (s/keys :req-un [::defines ::include-stack ::include-paths 
                   ::line-number ::current-file ::errors]))

;; ============================================================================
;; ПРОТОКОЛЫ ДЛЯ ДИРЕКТИВ ПРЕПРОЦЕССОРА
;; ============================================================================

(defprotocol PreprocessorDirective
  "Протокол для всех директив препроцессора.
   Каждая директива должна уметь:
   - Распознавать свой синтаксис
   - Обрабатывать строку с директивой
   - Возвращать результат обработки"
  (matches? [this line]
    "Проверяет, соответствует ли строка данной директиве")
  (process [this line state]
    "Обрабатывает строку с директивой, возвращает [result new-state]"))

(defprotocol ConditionalDirective
  "Протокол для условных директив (#if, #ifdef, #ifndef, #else, #endif)"
  (evaluate-condition [this condition state]
    "Вычисляет условие для условной директивы"))

(defprotocol MacroProcessor
  "Протокол для работы с макросами"
  (expand-macro [this macro-name args state]
    "Расширяет макрос с заданными аргументами")
  (define-macro [this name definition state]
    "Определяет новый макрос")
  (undefine-macro [this name state]
    "Удаляет определение макроса"))

(defprotocol FileProcessor
  "Протокол для работы с файловой системой при обработке директив #include.
   
   Архитектурное обоснование:
   - Абстракция файловых операций для тестируемости
   - Возможность мокирования файловой системы в тестах
   - Централизованная обработка ошибок чтения файлов
   - Поддержка различных стратегий поиска файлов"
  (resolve-include-path [this filename include-paths current-file]
    "Разрешает путь к включаемому файлу с учетом путей поиска")
  (read-include-file [this resolved-path]
    "Читает содержимое включаемого файла")
  (file-exists? [this path]
    "Проверяет существование файла")
  (get-canonical-path [this path]
    "Возвращает канонический путь файла для предотвращения циклических включений"))

;; ============================================================================
;; БАЗОВЫЕ ТИПЫ ДАННЫХ
;; ============================================================================

(defrecord PreprocessorState [defines include-stack include-paths 
                              line-number current-file errors
                              conditional-stack])

(defrecord ProcessingResult [content state])

(defrecord DirectiveMatch [directive-type params])

;; ============================================================================
;; КОНСТАНТЫ И ПРЕДОПРЕДЕЛЕННЫЕ ЗНАЧЕНИЯ
;; ============================================================================

(def ^:const MAX_INCLUDE_DEPTH 50)
(def ^:const MAX_MACRO_EXPANSION_ITERATIONS 10)

;; Регулярные выражения для директив (оптимизированные)
(def directive-patterns
  {:include    #"^\s*#\s*include\s*[<\"]([^>\"]+)[>\"]"
   :define     #"^\s*#\s*define\s+(\w+)(?:\s+(.*))?$"
   :undef      #"^\s*#\s*undef\s+(\w+)"
   :ifdef      #"^\s*#\s*ifdef\s+(\w+)"
   :ifndef     #"^\s*#\s*ifndef\s+(\w+)"
   :if         #"^\s*#\s*if\s+(.+)"
   :elif       #"^\s*#\s*elif\s+(.+)"
   :else       #"^\s*#\s*else"
   :endif      #"^\s*#\s*endif"
   :pragma     #"^\s*#\s*pragma\s+(.+)"
   :error      #"^\s*#\s*error\s*(.*)"
   :warning    #"^\s*#\s*warning\s*(.*)"
   :line       #"^\s*#\s*line\s+(\d+)(?:\s+\"([^\"]+)\")?"})

;; Предопределенные макросы согласно стандарту C89
(def predefined-macros
  {"__LINE__"     :line-number
   "__FILE__"     :current-file
   "__DATE__"     (str "\"" (.format (java.text.SimpleDateFormat. "MMM dd yyyy") (java.util.Date.)) "\"")
   "__TIME__"     (str "\"" (.format (java.text.SimpleDateFormat. "HH:mm:ss") (java.util.Date.)) "\"")
   "__STDC__"     "1"
   "__C51__"      "1"})

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ПАРСИНГА
;; ============================================================================

(defn parse-macro-definition
  "Парсит определение простого макроса из строки #define согласно стандарту C89.
   
   C89 поддерживает ТОЛЬКО простые макросы:
   - #define MAX_SIZE 100
   - #define DEBUG
   - #define VERSION \"1.0.0\"
   
   Возвращает [name definition] где definition может быть пустой строкой"
  [line]
  (when (nil? line)
    (throw (ex-info "Строка определения макроса не может быть nil" 
                   {:type :invalid-input :line line})))
  
  (let [trimmed (str/trim line)
        space-pos (.indexOf trimmed " ")]
    (if (<= space-pos 0)
      [trimmed ""]  ; Макрос без значения: #define DEBUG
      [(subs trimmed 0 space-pos) (str/trim (subs trimmed space-pos))])))

(defn expand-macros-in-text
  "Расширяет макросы в тексте с оптимизацией производительности"
  [text defines state]
  (let [all-macros (merge predefined-macros defines)]
    (if (empty? all-macros)
      text
      (loop [result text
             iterations 0]
        (if (>= iterations MAX_MACRO_EXPANSION_ITERATIONS)
          result
          (let [new-result (reduce-kv
                           (fn [acc macro-name macro-def]
                             (let [pattern (re-pattern (str "\\b" (java.util.regex.Pattern/quote macro-name) "\\b"))
                                   replacement (cond
                                               (= macro-name "__LINE__") (str (:line-number state))
                                               (= macro-name "__FILE__") (str "\"" (:current-file state) "\"")
                                               (contains? predefined-macros macro-name) (get predefined-macros macro-name)
                                               (string? macro-def) macro-def
                                               :else (str macro-def))]
                               (str/replace acc pattern replacement)))
                           result
                           all-macros)]
            (if (= new-result result)
              result  ; Больше нет изменений
              (recur new-result (inc iterations)))))))))

(defn create-initial-state
  "Создает начальное состояние препроцессора с валидацией"
  [& {:keys [defines include-paths current-file]
      :or {defines {}
           include-paths ["." "include" "lib"]
           current-file "input"}}]
  (let [state (->PreprocessorState 
               defines [] include-paths 1 current-file [] [])]
    (when-not (s/valid? ::preprocessor-state state)
      (throw (ex-info "Некорректное начальное состояние препроцессора"
                     {:spec-explain (s/explain-data ::preprocessor-state state)})))
    state))

(defn add-error
  "Добавляет ошибку в состояние препроцессора (чистая функция)"
  [state error-message & [error-data]]
  (let [error-info (merge {:message error-message
                          :line (:line-number state)
                          :file (:current-file state)
                          :timestamp (System/currentTimeMillis)}
                         error-data)]
    (update state :errors conj error-info)))

(defn should-process-line?
  "Определяет, должна ли строка обрабатываться на основе условной компиляции"
  [state]
  (or (empty? (:conditional-stack state))
      (every? :active (:conditional-stack state))))

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ РАБОТЫ С ВКЛЮЧЕНИЯМИ
;; ============================================================================

(defn check-include-depth
  "Проверяет глубину включений для предотвращения переполнения стека.
   
   Теоретическое обоснование:
   - Защита от бесконечной рекурсии при циклических включениях
   - Соответствие ограничениям компиляторов C (обычно 15-200 уровней)
   - Предотвращение исчерпания памяти стека"
  [state]
  (< (count (:include-stack state)) MAX_INCLUDE_DEPTH))

(defn parse-include-directive
  "Парсит директиву #include и определяет тип включения.
   
   Поддерживаемые форматы согласно стандарту C89:
   - #include <filename>  - системные заголовки
   - #include \"filename\" - пользовательские заголовки
   
   Возвращает {:type :system/:user :filename string} или nil при ошибке"
  [line]
  (when-let [[_ filename] (re-find (:include directive-patterns) line)]
    (cond
      ;; Определяем тип по первому символу в захваченной группе
      (str/starts-with? line "#include <") {:type :system :filename filename}
      (str/starts-with? line "#include \"") {:type :user :filename filename}
      :else nil)))

;; Реализация FileProcessor по умолчанию
(defrecord DefaultFileProcessor []
  FileProcessor
  (resolve-include-path [_ filename include-paths current-file]
    (let [;; Для пользовательских заголовков сначала ищем относительно текущего файла
          current-dir (when current-file
                       (let [file (io/file current-file)]
                         (when (.exists file)
                           (.getParent file))))
          search-paths (if current-dir
                        (cons current-dir include-paths)
                        include-paths)]
      (loop [paths search-paths]
        (when (seq paths)
          (let [candidate-path (str (first paths) "/" filename)
                file (io/file candidate-path)]
            (if (.exists file)
              (.getAbsolutePath file)
              (recur (rest paths))))))))
  
  (read-include-file [_ resolved-path]
    (try
      (slurp resolved-path)
      (catch Exception e
        (throw (ex-info (str "Ошибка чтения файла: " (.getMessage e))
                       {:file resolved-path :cause e})))))
  
  (file-exists? [_ path]
    (.exists (io/file path)))
  
  (get-canonical-path [_ path]
    (try
      (.getCanonicalPath (io/file path))
      (catch Exception _
        ;; Если не удается получить канонический путь, возвращаем исходный
        path))))

;; ============================================================================
;; ФУНКЦИИ ДЛЯ ОБРАБОТКИ ВКЛЮЧЕНИЙ
;; ============================================================================

(defn expand-includes
  "Рекурсивно разворачивает все директивы #include в коде.
   
   Архитектурное обоснование:
   - Предварительная обработка включений перед основным transducer
   - Рекурсивная обработка с защитой от переполнения стека
   - Циклические включения предотвращаются через include guards (стандарт C89)
   
   Возвращает {:content string :state state :errors [errors]}"
  ([code] (expand-includes code (create-initial-state) (->DefaultFileProcessor)))
  ([code state] (expand-includes code state (->DefaultFileProcessor)))
  ([code state file-processor]
   (let [lines (str/split-lines code)
         result (atom {:content [] :state state :errors []})]
     
     (doseq [line lines]
       (cond
         ;; Обрабатываем директиву #include
         (parse-include-directive line)
         (let [{:keys [filename type]} (parse-include-directive line)
               current-state (:state @result)]
           
           ;; Проверяем глубину включений
           (if-not (check-include-depth current-state)
             (swap! result update :errors conj 
                    {:message (str "Превышена максимальная глубина включений (" MAX_INCLUDE_DEPTH ")")
                     :filename filename 
                     :depth (count (:include-stack current-state))})
             
             ;; Разрешаем путь к файлу
             (let [resolved-path (resolve-include-path file-processor 
                                                      filename 
                                                      (:include-paths current-state)
                                                      (:current-file current-state))]
               (if-not resolved-path
                 ;; Файл не найден
                 (swap! result update :errors conj
                        {:message (str "Файл не найден: " filename)
                         :filename filename 
                         :type type
                         :search-paths (:include-paths current-state)})
                 
                 ;; Читаем файл и добавляем его содержимое
                 (try
                   (let [file-content (read-include-file file-processor resolved-path)
                         ;; Обновляем состояние для обработки включаемого файла
                         new-state (-> current-state
                                      (update :include-stack conj (:current-file current-state))
                                      (assoc :current-file resolved-path))
                         
                         ;; Рекурсивно обрабатываем включаемый файл
                         nested-result (expand-includes file-content new-state file-processor)
                         
                         ;; Восстанавливаем состояние, но сохраняем макросы
                         restored-state (-> new-state
                                          (assoc :defines (:defines (:state nested-result)))
                                          (update :include-stack pop)
                                          (assoc :current-file (:current-file current-state)))]
                     
                     ;; Добавляем содержимое включаемого файла
                     (swap! result 
                            (fn [r] 
                              (-> r
                                  (update :content concat (str/split-lines (:content nested-result)))
                                  (assoc :state restored-state)
                                  (update :errors concat (:errors nested-result))))))
                   
                   (catch Exception e
                     (swap! result update :errors conj
                            {:message (str "Ошибка при чтении файла: " (.getMessage e))
                             :filename filename 
                             :resolved-path resolved-path
                             :exception-type (type e)})))))))
         
         ;; Обрабатываем директиву #define
         (re-find (:define directive-patterns) line)
         (let [[_ macro-name definition] (re-find (:define directive-patterns) line)
               current-state (:state @result)
               parse-string (if definition 
                             (str macro-name " " definition)
                             macro-name)
               [parsed-name parsed-def] (parse-macro-definition parse-string)]
           (swap! result 
                  (fn [r]
                    (-> r
                        (update :content conj line)
                        (assoc :state (assoc-in current-state [:defines parsed-name] parsed-def))))))
         
         ;; Обычная строка - добавляем как есть
         :else
         (swap! result update :content conj line)))
     
     ;; Возвращаем результат
     (update @result :content #(str/join "\n" %)))))

;; Директива #include (упрощенная версия для совместимости)
(defrecord IncludeDirective [file-processor]
  PreprocessorDirective
  (matches? [_ line]
    (re-find (:include directive-patterns) line))
  
  (process [_ line state]
    ;; В новой архитектуре #include обрабатывается на этапе expand-includes
    ;; Эта реализация нужна только для совместимости с transducer
    [nil state]))

;; ============================================================================
;; TRANSDUCERS ДЛЯ ОБРАБОТКИ ТЕКСТА
;; ============================================================================

(defn remove-comments-xf
  "Transducer для удаления комментариев из строк кода"
  []
  (map (fn [line]
         (-> line
             ;; Удаляем однострочные комментарии
             (str/replace #"//.*$" "")
             ;; Удаляем многострочные комментарии (упрощенная версия)
             (str/replace #"/\*.*?\*/" "")))))

(defn normalize-whitespace-xf
  "Transducer для нормализации пробельных символов"
  []
  (map (fn [line]
         (-> line
             (str/replace #"[ \t]+" " ")
             str/trim))))

(defn filter-empty-lines-xf
  "Transducer для фильтрации пустых строк"
  []
  (filter (complement str/blank?)))

(defn add-line-numbers-xf
  "Transducer для добавления номеров строк"
  []
  (map-indexed (fn [idx line] {:line-number (inc idx) :content line})))

;; ============================================================================
;; РЕАЛИЗАЦИИ ДИРЕКТИВ ПРЕПРОЦЕССОРА
;; ============================================================================

;; Директива #define
(defrecord DefineDirective []
  PreprocessorDirective
  (matches? [_ line]
    (re-find (:define directive-patterns) line))
  
  (process [_ line state]
    (if (should-process-line? state)
      (let [[_ macro-name definition] (re-find (:define directive-patterns) line)]
        (if macro-name
          (let [;; Формируем строку для парсинга: "NAME" или "NAME value"
                parse-string (if definition 
                              (str macro-name " " definition)
                              macro-name)
                [parsed-name parsed-def] (parse-macro-definition parse-string)]
            [nil (assoc-in state [:defines parsed-name] parsed-def)])
          ;; Если не удалось распарсить имя макроса
          [nil (add-error state "Некорректный синтаксис директивы #define")]))
      [nil state]))
  
  MacroProcessor
  (define-macro [_ name definition state]
    (assoc-in state [:defines name] definition)))

;; Директива #undef  
(defrecord UndefDirective []
  PreprocessorDirective
  (matches? [_ line]
    (re-find (:undef directive-patterns) line))
  
  (process [_ line state]
    (if (should-process-line? state)
      (let [[_ macro-name] (re-find (:undef directive-patterns) line)
            new-state (update state :defines dissoc macro-name)]
        [nil new-state])
      [nil state]))
  
  MacroProcessor
  (undefine-macro [_ name state]
    (update state :defines dissoc name)))

;; Директива #ifdef
(defrecord IfdefDirective []
  PreprocessorDirective
  (matches? [_ line]
    (re-find (:ifdef directive-patterns) line))
  
  (process [_ line state]
    (let [[_ macro-name] (re-find (:ifdef directive-patterns) line)
          condition-result (contains? (:defines state) macro-name)
          new-state (update state :conditional-stack 
                           conj {:type :ifdef 
                                :condition condition-result 
                                :active condition-result})]
      [nil new-state]))
  
  ConditionalDirective
  (evaluate-condition [_ condition state]
    (contains? (:defines state) condition)))

;; Директива #ifndef
(defrecord IfndefDirective []
  PreprocessorDirective
  (matches? [_ line]
    (re-find (:ifndef directive-patterns) line))
  
  (process [_ line state]
    (let [[_ macro-name] (re-find (:ifndef directive-patterns) line)
          condition-result (not (contains? (:defines state) macro-name))
          new-state (update state :conditional-stack 
                           conj {:type :ifndef 
                                :condition condition-result 
                                :active condition-result})]
      [nil new-state]))
  
  ConditionalDirective
  (evaluate-condition [_ condition state]
    (not (contains? (:defines state) condition))))

;; Директива #else
(defrecord ElseDirective []
  PreprocessorDirective
  (matches? [_ line]
    (re-find (:else directive-patterns) line))
  
  (process [_ line state]
    (if (empty? (:conditional-stack state))
      [nil (add-error state "Директива #else без соответствующего #if")]
      (let [current-block (peek (:conditional-stack state))
            new-block (assoc current-block 
                            :active (not (:condition current-block)) 
                            :in-else true)
            new-state (assoc state :conditional-stack 
                            (conj (pop (:conditional-stack state)) new-block))]
        [nil new-state]))))

;; Директива #endif
(defrecord EndifDirective []
  PreprocessorDirective
  (matches? [_ line]
    (re-find (:endif directive-patterns) line))
  
  (process [_ line state]
    (if (empty? (:conditional-stack state))
      [nil (add-error state "Директива #endif без соответствующего #if")]
      [nil (update state :conditional-stack pop)])))

;; ============================================================================
;; РЕЕСТР ДИРЕКТИВ
;; ============================================================================

(def directive-registry
  "Реестр всех доступных директив препроцессора"
  [(->DefineDirective)
   (->UndefDirective)
   (->IfdefDirective)
   (->IfndefDirective)
   (->ElseDirective)
   (->EndifDirective)
   (->IncludeDirective (->DefaultFileProcessor))])

;; ============================================================================
;; ОСНОВНОЙ TRANSDUCER ДЛЯ ОБРАБОТКИ ПРЕПРОЦЕССОРА
;; ============================================================================

(defn preprocessor-xf
  "Основной transducer для обработки препроцессора"
  [initial-state]
  (fn [rf]
    (let [state (volatile! initial-state)]
      (fn
        ([] (rf))
        ([result] (rf result))
        ([result line]
         (let [current-state @state
               ;; Ищем подходящую директиву
               matching-directive (first (filter #(matches? % line) directive-registry))]
           (if matching-directive
             ;; Обрабатываем директиву
             (let [[processed-line new-state] (process matching-directive line current-state)]
               (vreset! state new-state)
               (if processed-line
                 (rf result processed-line)
                 result))  ; Пропускаем строку, если processed-line = nil
             ;; Обычная строка кода
             (if (should-process-line? current-state)
               (let [expanded-line (expand-macros-in-text line (:defines current-state) current-state)
                     new-state (update current-state :line-number inc)]
                 (vreset! state new-state)
                 (rf result expanded-line))
               ;; Пропускаем строку из-за условной компиляции
               (do
                 (vswap! state update :line-number inc)
                 result)))))))))

;; ============================================================================
;; ПУБЛИЧНЫЙ API
;; ============================================================================

(defn preprocess-v2
  "Новая версия препроцессора с использованием transducers и protocols"
  ([code] (preprocess-v2 code {}))
  ([code options]
   ;; Валидация входных параметров
   (when-not (string? code)
     (throw (ex-info "Код должен быть строкой" {:code-type (type code)})))
   
   (when-not (map? options)
     (throw (ex-info "Опции должны быть map" {:options-type (type options)})))
   
   (try
     (let [initial-state (create-initial-state 
                         :defines (get options :defines {})
                         :include-paths (get options :include-paths ["." "include" "lib"])
                         :current-file (get options :current-file "input"))
           
           ;; ЭТАП 1: Предварительная обработка включений
           include-result (expand-includes code initial-state)
           expanded-code (:content include-result)
           state-after-includes (-> initial-state
                                   (assoc :defines (merge (:defines initial-state) 
                                                         (:defines (:state include-result))))
                                   (assoc :errors (concat (:errors initial-state) 
                                                         (:errors (:state include-result)))))
           include-errors (:errors include-result)
           
           ;; ЭТАП 2: Основная обработка через transducers
           lines (str/split-lines expanded-code)
           final-state (volatile! state-after-includes)
           
           ;; Композиция transducers для обработки (без IncludeDirective)
           processing-xf (comp
                         (remove-comments-xf)
                         (normalize-whitespace-xf)
                         (map (fn [line]
                                (let [current-state @final-state
                                      ;; Ищем подходящую директиву (кроме #include)
                                      matching-directive (first (filter #(and (matches? % line)
                                                                              (not (instance? IncludeDirective %))) 
                                                                        directive-registry))]
                                  (if matching-directive
                                    ;; Обрабатываем директиву
                                    (let [[processed-line new-state] (process matching-directive line current-state)]
                                      (vreset! final-state new-state)
                                      processed-line)  ; может быть nil
                                    ;; Обычная строка кода
                                    (if (should-process-line? current-state)
                                      (let [expanded-line (expand-macros-in-text line (:defines current-state) current-state)
                                            new-state (update current-state :line-number inc)]
                                        (vreset! final-state new-state)
                                        expanded-line)
                                      ;; Пропускаем строку из-за условной компиляции
                                      (do
                                        (vswap! final-state update :line-number inc)
                                        nil))))))
                         (filter some?))  ; Убираем nil строки
           
           processed-lines (into [] processing-xf lines)
           final-state-value @final-state
           all-errors (concat include-errors (:errors final-state-value))]
       
       {:result (str/join "\n" processed-lines)
        :errors all-errors
        :success (empty? all-errors)
        :final-state final-state-value})
     
     (catch Exception e
       {:result ""
        :errors [{:message (str "Критическая ошибка препроцессора: " (.getMessage e))
                 :type :critical
                 :exception-type (type e)}]
        :success false}))))

(defn migrate-to-v2
  "Помощник для миграции с старого API на новый.
   Показывает, как изменить код для использования нового API."
  [code options]
  (println "=== МИГРАЦИЯ НА ПРЕПРОЦЕССОР V2 ===")
  (println "Старый код:")
  (println "  (preprocess code options)")
  (println "")
  (println "Новый код:")
  (println "  (let [result (preprocess-v2 code options)]")
  (println "    (if (:success result)")
  (println "      (:result result)")
  (println "      (handle-errors (:errors result))))")
  (println "")
  (preprocess-v2 code options)) 