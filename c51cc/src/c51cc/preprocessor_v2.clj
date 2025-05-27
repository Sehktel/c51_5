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
   (->EndifDirective)])

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
           
           ;; Обработка строк через transducers
           lines (str/split-lines code)
           
           ;; Создаем stateful transducer для отслеживания состояния
           final-state (volatile! initial-state)
           
           ;; Композиция transducers для обработки
           processing-xf (comp
                         (remove-comments-xf)
                         (normalize-whitespace-xf)
                         (map (fn [line]
                                (let [current-state @final-state
                                      ;; Ищем подходящую директиву
                                      matching-directive (first (filter #(matches? % line) directive-registry))]
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
           errors (:errors final-state-value)]
       
       {:result (str/join "\n" processed-lines)
        :errors errors
        :success (empty? errors)})
     
     (catch Exception e
       {:result ""
        :errors [{:message (str "Критическая ошибка препроцессора: " (.getMessage e))
                 :type :critical
                 :exception-type (type e)}]
        :success false})))) 