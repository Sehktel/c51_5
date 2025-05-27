(ns c51cc.preprocessor
  (:require [clojure.string :as str]
            [clojure.java.io :as io]
            [clojure.set :as set]))

;; Состояние препроцессора
(def ^:dynamic *preprocessor-state* 
  {:defines {}           ;; Определенные макросы
   :include-stack []     ;; Стек включаемых файлов для предотвращения циклических включений
   :include-guards #{}   ;; Защитные макросы для предотвращения повторного включения
   :include-paths ["." "include" "lib"]  ;; Пути поиска заголовочных файлов
   :conditional-stack [] ;; Стек условной компиляции (#if, #ifdef, etc.)
   :line-number 1        ;; Текущий номер строки
   :current-file ""      ;; Текущий обрабатываемый файл
   :errors []            ;; Собранные ошибки препроцессора
   })

(declare process-line)

(defn add-error
  "Добавляет ошибку в состояние препроцессора"
  [state error-message & [error-data]]
  (let [error-info (merge {:message error-message
                          :line (:line-number state)
                          :file (:current-file state)}
                         error-data)]
    (update state :errors conj error-info)))

;; Регулярные выражения для директив препроцессора
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

;; Предопределенные макросы согласно C89
(def predefined-macros
  {"__LINE__"     :line-number
   "__FILE__"     :current-file
   "__DATE__"     (str "\"" (.format (java.text.SimpleDateFormat. "MMM dd yyyy") (java.util.Date.)) "\"")
   "__TIME__"     (str "\"" (.format (java.text.SimpleDateFormat. "HH:mm:ss") (java.util.Date.)) "\"")
   "__STDC__"     "1"
   "__C51__"      "1"})

(defn remove-comments 
  "Удаляет комментарии из кода согласно стандарту C89.
   Поддерживает:
   - Однострочные комментарии (//) - расширение, не входящее в C89
   - Многострочные комментарии (/* */)
   
   Возвращает код без комментариев с сохранением номеров строк"
  [code]
  (-> code
      ;; Удаляем многострочные комментарии, заменяя их пробелами для сохранения позиций
      (str/replace #"/\*(?s).*?\*/" 
                   (fn [match] 
                     (str/join (repeat (count match) \space))))
      ;; Удаляем однострочные комментарии (расширение для удобства)
      (str/replace #"//.*?(?:\r\n|\r|\n|$)" "\n")))

(defn normalize-whitespace 
  "Нормализует пробельные символы, сохраняя переносы строк для корректной работы директив"
  [code]
  (-> code
      ;; Заменяем последовательности пробелов и табов на один пробел
      (str/replace #"[ \t]+" " ")
      ;; Убираем пробелы в начале и конце строк
      (str/replace #"(?m)^[ \t]+|[ \t]+$" "")))

(defn expand-macros-in-text
  "Расширяет все макросы в тексте, включая предопределенные и рекурсивное расширение"
  [text defines state]
  (let [all-macros (merge predefined-macros defines)
        ;; Функция для расширения одного макроса
        expand-single (fn [text macro-name macro-def]
                       (let [pattern (re-pattern (str "\\b" macro-name "\\b"))
                             replacement (cond
                                          ;; Предопределенные макросы
                                          (= macro-name "__LINE__") (str (:line-number state))
                                          (= macro-name "__FILE__") (str "\"" (:current-file state) "\"")
                                          (contains? predefined-macros macro-name) (get predefined-macros macro-name)
                                          ;; Обычные макросы
                                          (string? macro-def) macro-def
                                          :else (str macro-def))]
                         (str/replace text pattern replacement)))]
    ;; Применяем расширение макросов несколько раз для рекурсивного расширения
    (loop [result text
           iterations 0
           max-iterations 10] ;; Предотвращаем бесконечные циклы
      (if (>= iterations max-iterations)
        result
        (let [new-result (reduce (fn [acc [macro-name macro-def]]
                                  (expand-single acc macro-name macro-def))
                                result
                                all-macros)]
          (if (= new-result result)
            ;; Больше нет изменений
            result
            ;; Продолжаем расширение
            (recur new-result (inc iterations) max-iterations)))))))

(defn parse-macro-definition
  "Парсит определение макроса из строки #define"
  [definition]
  (let [trimmed (str/trim definition)]
    (if (str/includes? trimmed "(")
      ;; Функциональный макрос
      (let [paren-pos (.indexOf trimmed "(")
            macro-name (subs trimmed 0 paren-pos)
            rest-part (subs trimmed paren-pos)
            close-paren (.indexOf rest-part ")")]
        (if (> close-paren 0)
          (let [params-str (subs rest-part 1 close-paren)
                params (if (str/blank? params-str) 
                         [] 
                         (map str/trim (str/split params-str #",")))
                body (str/trim (subs rest-part (inc close-paren)))]
            [macro-name {:params params :body body}])
          [macro-name ""]))
      ;; Простой макрос - исправляем логику парсинга
      (let [space-pos (.indexOf trimmed " ")]
        (if (> space-pos 0)
          [(subs trimmed 0 space-pos) (str/trim (subs trimmed space-pos))]
          [trimmed ""])))))

(defn evaluate-condition
  "Вычисляет условие для директив #if, #ifdef, #ifndef
   Базовая реализация для простых случаев"
  [condition defines]
  (cond
    ;; defined(MACRO) или defined MACRO
    (re-find #"defined\s*\(\s*(\w+)\s*\)" condition)
    (let [macro (second (re-find #"defined\s*\(\s*(\w+)\s*\)" condition))]
      (contains? defines macro))
    
    (re-find #"defined\s+(\w+)" condition)
    (let [macro (second (re-find #"defined\s+(\w+)" condition))]
      (contains? defines macro))
    
    ;; Простое имя макроса
    (re-matches #"\w+" condition)
    (contains? defines condition)
    
    ;; Числовые константы
    (re-matches #"\d+" condition)
    (not= "0" condition)
    
    ;; По умолчанию false для сложных выражений
    :else false))

(defn find-include-file
  "Ищет файл заголовка в указанных путях"
  [filename include-paths system-include?]
  (let [search-paths (if system-include?
                       ;; Для системных заголовков (<>) ищем только в системных путях
                       (filter #(not= "." %) include-paths)
                       ;; Для пользовательских заголовков ("") ищем сначала в текущей директории
                       include-paths)]
    (some (fn [path]
            (let [full-path (if (= path ".")
                              filename
                              (str path "/" filename))
                  file (io/file full-path)]
              (when (.exists file)
                (.getAbsolutePath file))))
          search-paths)))

(defn process-include
  "Обрабатывает директиву #include. Возвращает [result state] где result может быть строкой или nil при ошибке"
  [filename system-include? state]
  ;; Валидация входных параметров
  (if (str/blank? filename)
    [nil (add-error state "Пустое имя файла в директиве #include" {:filename filename})]
    
    (let [include-file (find-include-file filename (:include-paths state) system-include?)]
      (cond
        ;; Файл не найден
        (nil? include-file)
        [nil (add-error state (str "Файл заголовка не найден: " filename)
                       {:filename filename 
                        :paths (:include-paths state)
                        :system-include system-include?})]
        
        ;; Циклическое включение
        (contains? (set (:include-stack state)) include-file)
        [nil (add-error state (str "Циклическое включение файла: " include-file)
                       {:file include-file 
                        :stack (:include-stack state)})]
        
        ;; Превышена глубина включений
        (> (count (:include-stack state)) 50)
        [nil (add-error state "Превышена максимальная глубина включений файлов"
                       {:max-depth 50 
                        :current-depth (count (:include-stack state))
                        :stack (:include-stack state)})]
        
        ;; Обрабатываем файл
        :else
        (let [file-content (try 
                             (slurp include-file)
                             (catch Exception e
                               nil))] ; Возвращаем nil при ошибке чтения
          (if (nil? file-content)
            [nil (add-error state (str "Ошибка чтения файла: " include-file)
                           {:filename include-file})]
            
            (let [new-state (-> state
                                (update :include-stack conj include-file)
                                (assoc :current-file include-file :line-number 1))
                  lines (str/split-lines file-content)]
              ;; Обрабатываем каждую строку включенного файла
              (loop [remaining-lines lines
                     processed-lines []
                     current-state new-state]
                (if (empty? remaining-lines)
                  ;; Возвращаем обработанное содержимое и восстанавливаем исходный файл
                  [(str/join "\n" processed-lines) 
                   (-> current-state
                       (update :include-stack pop)
                       (assoc :current-file (:current-file state)))]
                  ;; Обрабатываем следующую строку
                  (let [current-line (first remaining-lines)
                        [processed-line updated-state] (process-line current-line current-state)]
                    (recur (rest remaining-lines)
                           (if (str/blank? processed-line)
                             processed-lines
                             (conj processed-lines processed-line))
                           updated-state)))))))))))

(defn process-line
  "Обрабатывает одну строку кода, применяя директивы препроцессора"
  [line state]
  (let [trimmed-line (str/trim line)]
    (cond
      ;; Пустая строка или комментарий
      (or (str/blank? trimmed-line) 
          (str/starts-with? trimmed-line "//"))
      [line state]
      
      ;; Директива #include
      (re-find (:include directive-patterns) trimmed-line)
      (let [[_ filename] (re-find (:include directive-patterns) trimmed-line)
            system-include? (str/starts-with? (str/trim line) "#include <")
            [included-content new-state] (process-include filename system-include? state)]
        (if (nil? included-content)
          ;; Ошибка включения - состояние уже содержит ошибку
          [(str "// " line " // ОШИБКА включения") new-state]
          ;; Успешное включение
          [(str "// #include \"" filename "\"\n" included-content) new-state]))
      
      ;; Директива #define
      (re-find (:define directive-patterns) trimmed-line)
      (let [should-process? (or (empty? (:conditional-stack state))
                               (every? :active (:conditional-stack state)))]
        (if should-process?
          (let [[_ macro-name definition] (re-find (:define directive-patterns) trimmed-line)
                [parsed-name parsed-def] (parse-macro-definition (str macro-name " " (or definition "")))
                new-state (assoc-in state [:defines parsed-name] parsed-def)]
            ["" new-state])
          ;; Пропускаем #define в неактивном блоке
          ["" state]))
      
      ;; Директива #undef
      (re-find (:undef directive-patterns) trimmed-line)
      (let [should-process? (or (empty? (:conditional-stack state))
                               (every? :active (:conditional-stack state)))]
        (if should-process?
          (let [[_ macro-name] (re-find (:undef directive-patterns) trimmed-line)
                new-state (update state :defines dissoc macro-name)]
            ["" new-state])
          ;; Пропускаем #undef в неактивном блоке
          ["" state]))
      
      ;; Директива #ifdef
      (re-find (:ifdef directive-patterns) trimmed-line)
      (let [[_ macro-name] (re-find (:ifdef directive-patterns) trimmed-line)
            condition-result (contains? (:defines state) macro-name)
            new-state (update state :conditional-stack conj {:type :ifdef :condition condition-result :active condition-result})]
        ["" new-state])
      
      ;; Директива #ifndef
      (re-find (:ifndef directive-patterns) trimmed-line)
      (let [[_ macro-name] (re-find (:ifndef directive-patterns) trimmed-line)
            condition-result (not (contains? (:defines state) macro-name))
            new-state (update state :conditional-stack conj {:type :ifndef :condition condition-result :active condition-result})]
        ["" new-state])
      
      ;; Директива #if
      (re-find (:if directive-patterns) trimmed-line)
      (let [[_ condition] (re-find (:if directive-patterns) trimmed-line)
            condition-result (evaluate-condition condition (:defines state))
            new-state (update state :conditional-stack conj {:type :if :condition condition-result :active condition-result})]
        ["" new-state])
      
      ;; Директива #else
      (re-find (:else directive-patterns) trimmed-line)
      (if (empty? (:conditional-stack state))
        ;; Ошибка: #else без соответствующего #if
        (let [new-state (add-error state "Директива #else без соответствующего #if")]
          [(str "// ОШИБКА: #else без соответствующего #if") new-state])
        (let [current-block (peek (:conditional-stack state))
              ;; ИСПРАВЛЕНИЕ: инвертируем active относительно исходного condition
              new-block (assoc current-block :active (not (:condition current-block)) :in-else true)
              new-state (assoc state :conditional-stack 
                              (conj (pop (:conditional-stack state)) new-block))]
          ["" new-state]))
      
      ;; Директива #endif
      (re-find (:endif directive-patterns) trimmed-line)
      (if (empty? (:conditional-stack state))
        ;; Ошибка: #endif без соответствующего #if
        (let [new-state (add-error state "Директива #endif без соответствующего #if")]
          [(str "// ОШИБКА: #endif без соответствующего #if") new-state])
        (let [new-state (update state :conditional-stack pop)]
          ["" new-state]))
      
      ;; Директива #error
      (re-find (:error directive-patterns) trimmed-line)
      (let [[_ message] (re-find (:error directive-patterns) trimmed-line)
            new-state (add-error state (str "Ошибка препроцессора: " message) {:type :user-error})]
        [(str "// #error " message) new-state])
      
      ;; Обычная строка кода
      :else
      (let [should-include? (or (empty? (:conditional-stack state))
                               (every? :active (:conditional-stack state)))]
        (if should-include?
          ;; Расширяем макросы в строке
          (let [expanded-line (expand-macros-in-text line (:defines state) state)]
            [expanded-line (update state :line-number inc)])
          ;; Пропускаем строку из-за условной компиляции
          ["" (update state :line-number inc)])))))

(defn validate-preprocessor-state
  "Валидирует состояние препроцессора на корректность. Возвращает [valid? state-with-errors]"
  [state]
  (let [errors (cond-> []
                 (not (map? (:defines state)))
                 (conj {:message "Некорректное состояние: defines должно быть map" :type :validation})
                 
                 (not (vector? (:include-stack state)))
                 (conj {:message "Некорректное состояние: include-stack должно быть vector" :type :validation})
                 
                 (not (vector? (:conditional-stack state)))
                 (conj {:message "Некорректное состояние: conditional-stack должно быть vector" :type :validation})
                 
                 (not (sequential? (:include-paths state)))
                 (conj {:message "Некорректное состояние: include-paths должно быть sequential" :type :validation})
                 
                 (not (number? (:line-number state)))
                 (conj {:message "Некорректное состояние: line-number должно быть number" :type :validation}))]
    (if (empty? errors)
      [true state]
      [false (update state :errors concat errors)])))

(defn preprocess
  "Основная функция препроцессора.
   Обрабатывает исходный код, применяя все директивы препроцессора"
  ([code] (preprocess code {}))
  ([code options]
   ;; Валидация входных параметров
   (when-not (string? code)
     (throw (ex-info "Код должен быть строкой" {:code-type (type code)})))
   
   (when-not (map? options)
     (throw (ex-info "Опции должны быть map" {:options-type (type options)})))
   
   (let [;; Сначала удаляем комментарии из всего кода
         code-without-comments (remove-comments code)
         ;; Нормализуем пробелы
         normalized-code (normalize-whitespace code-without-comments)
         initial-state (merge *preprocessor-state* 
                             {:defines (merge (:defines *preprocessor-state*) 
                                            (get options :defines {}))
                              :include-paths (or (:include-paths options) 
                                               (:include-paths *preprocessor-state*))
                              :current-file (or (:current-file options) "input")})
         [valid? validated-state] (validate-preprocessor-state initial-state)]
     
     ;; Проверяем валидность состояния
     (if (not valid?)
       {:result "" :errors (:errors validated-state) :success false}
       
       ;; Проверяем, что код не пустой
       (if (str/blank? normalized-code)
         {:result "" :errors [] :success true}
         (let [lines (str/split-lines normalized-code)]
           (try
             (loop [remaining-lines lines
                    processed-lines []
                    current-state initial-state]
               (if (empty? remaining-lines)
                 ;; Проверяем, что все условные блоки закрыты
                 (let [final-state (if (empty? (:conditional-stack current-state))
                                    current-state
                                    (add-error current-state 
                                              "Незакрытые условные блоки препроцессора"
                                              {:unclosed-blocks (:conditional-stack current-state)}))]
                   ;; Возвращаем результат с информацией об ошибках
                   {:result (str/join "\n" processed-lines)
                    :errors (:errors final-state)
                    :success (empty? (:errors final-state))})
                 ;; Обрабатываем следующую строку
                 (let [current-line (first remaining-lines)
                       [processed-line new-state] (try
                                                    (process-line current-line current-state)
                                                    (catch Exception e
                                                      ;; Добавляем ошибку вместо выбрасывания исключения
                                                      (let [error-state (add-error current-state 
                                                                                   (.getMessage e)
                                                                                   {:exception-type (type e)
                                                                                    :current-line current-line})]
                                                        [(str "// ОШИБКА: " (.getMessage e)) error-state])))]
                   (recur (rest remaining-lines)
                          (if (str/blank? processed-line)
                            processed-lines
                            (conj processed-lines processed-line))
                          new-state))))
             (catch Exception e
               ;; Критическая ошибка препроцессора
               {:result ""
                :errors [{:message (str "Критическая ошибка препроцессора: " (.getMessage e))
                         :type :critical
                         :exception-type (type e)
                         :input-length (count code)
                         :options options}]
                :success false}))))))))

;; Вспомогательные функции для создания защитных макросов
(defn generate-include-guard
  "Генерирует уникальный защитный макрос для файла заголовка"
  [filename]
  (let [normalized (-> filename
                      (str/replace #"[^a-zA-Z0-9_]" "_")
                      (str/upper-case))]
    (str "__" normalized "_H__")))

(defn wrap-with-include-guard
  "Оборачивает содержимое файла заголовка в защитный макрос"
  [content filename]
  (let [guard-macro (generate-include-guard filename)]
    (str "#ifndef " guard-macro "\n"
         "#define " guard-macro "\n\n"
         content "\n\n"
         "#endif // " guard-macro "\n")))

;; Экспортируемые функции для совместимости
(defn remove-whitespace 
  "Устаревшая функция для совместимости. Используйте normalize-whitespace"
  [code]
  (normalize-whitespace code))

;; Список директив препроцессора для экспорта
(def preprocessor-directives 
  [:include :define :undef :if :ifdef :ifndef :else :elif :endif :pragma :error :warning :line])


