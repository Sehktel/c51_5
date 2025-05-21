(ns c51cc.parser
  (:require 
   [c51cc.logger :as log]
   [clojure.spec.alpha :as s] ;; Спецификации для проверки структуры токенов
   [clojure.spec.gen.alpha :as gen]
   [c51cc.config :refer [DEBUG]]
   ))



(declare get-current-token get-next-token move-next parse-expression parse-block)

;; ---- Функции работы с вектором токенов ----
;; Т.к. у нас вектор токенов, то мы можем использовать индексы для получения токенов
;; И по этому индексу мы можем перемещаться по вектору токенов
;; Получение текущего токена
(defn get-current-token
  "Возвращает токен по указанному индексу"
  [tokens state]
  {:pre [(pos? state) (< state (count tokens))]} ;; Проверка на индекс в векторе токенов
  ;; :pre -- это контракт, который проверяется перед выполнением функции
  ;; pos? -- positive? -- положительное число
  ;; (< state (count tokens)) -- проверка на индекс в векторе токенов
  ;; {:post [(token? %)]} ;; Проверка типа токена не нужна, так как мы работаем с вектором токенов
  ;; nth -- get-nth -- получение элемента по индексу
  (nth tokens state))

;; Получение следующего токена
(defn get-next-token
  "Возвращает следующий токен, если возможно"
  [tokens state]
  {:pre [(pos? state)]} ;; Проверка на положительный индекс в векторе токенов
  ;;{:post [(nil? %)]} ;; Проверка на nil не нужна, так как мы работаем с вектором токенов
  (if (< (inc state) (count tokens)) ;; Если мы не вышли за пределы вектора токенов
    (nth tokens (inc state)) ;; Возвращаем следующий токен
    ;; функция inc нам увеличивает индекс на 1
    nil)) ;; Если вышли за пределы вектора токенов, то возвращаем nil

;; Переход на следующий токен
;; Причем мы передаем state и не делаем проверку на возможность перехода на следующий токен
;; Поэтому мы должны работать в паре с get-next-token not nil
;; (defn move-next
;;   "Перемещает состояние на следующий токен"
;;   [state]
;;   {:pre [(pos? state)]} ;; Проверка на положительный индекс в векторе токенов
;;   {:post [(> % state)]} ;; Проверка что индекс увеличился
;;   (inc state)) ;; Увеличиваем индекс на 1
(defn move-next
  "Перемещает состояние на следующий токен"
  [state]
  {:pre [(pos? state)]}
  (let [result (inc state)]
    (assert (> result state) "Индекс должен увеличиться")
    result))


;; ---- Функции работы с вектором токенов ----

;; --- Структуры языка С51 ----

;; модификаторы типов signed и unsigned
(s/def ::sign-modifier #{:signed :unsigned})
;; базовые типы int, char, void
(s/def ::base-type #{:int :char :void})
;; спецификатор типа
(s/def ::typed-specifier
  (s/keys :opt-un [::sign-modifier] ;; необязательный параметр
          :req-un [::base-type])) ;; обязательный параметр

;; спецификатор функции
(s/def ::function-specifier
  (s/keys :req-un [::typed-specifier])) ;; обязательный параметр!

;; Далее пока не проверял...
;; Конструкция слеудющая: 
;; s/def -- определение что это структура 
;; ::for-token -- значение 
;; #{} -- множество токенов (именно множество!)
;; :for -- токен

(s/def ::for-token #{:for-keyword}) 

(s/def ::while-token #{:while-keyword})

(s/def ::if-token #{:if-keyword})

(s/def ::else-token #{:else-keyword})

(s/def ::return-token #{:return-keyword})

(s/def ::break-token #{:break-keyword})

;; Пример более развернутой структуры
(s/def ::for-loop
  ;; s/cat -- конструкция для создания структуры s/cat -- конкатенация
  (s/cat :for-keyword #{:for-keyword}
         :open-round-bracket #{:open-round-bracket} 
         :init-expr (s/cat :init-expr ::expression) ;;? 
         :semicolon #{:semicolon} 
         :condition (s/cat :condition ::expression) 
         :semicolon #{:semicolon} 
         :update-expr (s/cat :update-expr ::expression) 
         :close-round-bracket #{:close-round-bracket}
         :open-curly-bracket #{:open-curly-bracket}
         :statements (s/cat :statement ::statement)
         :close-curly-bracket #{:close-curly-bracket}))

(s/def ::while-loop
  (s/cat :while-keyword #{:while-keyword}
         :open-round-bracket #{:open-round-bracket}
         :condition (s/cat :condition ::expression)
         :close-round-bracket #{:close-round-bracket}
         :open-curly-bracket #{:open-curly-bracket}
         :statements (s/cat :statement ::statement)
         :close-curly-bracket #{:close-curly-bracket}))

;; Абстрактная модель конечного автомата для if-statement
(def if-statement-fsm
  "Декларативное описание конечного автомата разбора if-statement"
  {:initial-state :start
   :states 
   {:start 
    {:transitions 
     [{:token :if-keyword 
       :next-state :open-round-bracket
       :action :validate-if-keyword}]}
    
    :open-round-bracket 
    {:transitions 
     [{:token :open-round-bracket
       :next-state :condition
       :action :validate-open-round-bracket}]}
    
    :condition 
    {:transitions 
     [{:predicate :is-expression
       :next-state :close-round-bracket
       :action :parse-condition}]}
    
    :close-round-bracket 
    {:transitions 
     [{:token :close-round-bracket
       :next-state :open-curly-bracket
       :action :validate-close-round-bracket}]}
    
    :open-curly-bracket 
    {:transitions 
     [{:token :open-curly-bracket
       :next-state :body
       :action :validate-open-curly-bracket}]}
    
    :body 
    {:transitions 
     [{:predicate :is-statement-block
       :next-state :close-curly-bracket
       :action :parse-body}]}
    
    :close-curly-bracket 
    {:transitions 
     [{:token :close-curly-bracket
       :next-state :optional-else
       :action :validate-close-curly-bracket}]}
    
    :optional-else 
    {:transitions 
     [{:token :else-keyword
       :next-state :else-open-curly-bracket
       :action :handle-else}
      {:predicate :is-end-of-statement
       :next-state :final
       :action :complete-parsing}]}
    
    :else-open-curly-bracket 
    {:transitions 
     [{:token :open-curly-bracket
       :next-state :else-body
       :action :validate-else-open-curly-bracket}]}
    
    :else-body 
    {:transitions 
     [{:predicate :is-statement-block
       :next-state :else-close-curly-bracket
       :action :parse-else-body}]}
    
    :else-close-curly-bracket 
    {:transitions 
     [{:token :close-curly-bracket
       :next-state :final
       :action :validate-else-close-curly-bracket}]}
    
    :final {:type :accept}}
   
   ;; Семантические предикаты для валидации переходов
   :predicates 
   {:is-expression 
    (fn [tokens state] 
      "Проверка, является ли текущий токен выражением")
    
    :is-statement-block 
    (fn [tokens state] 
      "Проверка блока операторов")
    
    :is-end-of-statement 
    (fn [tokens state] 
      "Проверка завершения if-statement")}
   
   ;; Действия при переходах
   :actions 
   {:validate-if-keyword 
    (fn [tokens state] 
      "Валидация ключевого слова if")
    
    :validate-open-paren 
    (fn [tokens state] 
      "Валидация открывающей скобки")
    
    :parse-condition 
    (fn [tokens state] 
      "Парсинг условного выражения")
    
    :validate-close-paren 
    (fn [tokens state] 
      "Валидация закрывающей скобки")
    
    :validate-open-brace 
    (fn [tokens state] 
      "Валидация открывающей фигурной скобки")
    
    :parse-body 
    (fn [tokens state] 
      "Парсинг тела блока")
    
    :validate-close-brace 
    (fn [tokens state] 
      "Валидация закрывающей фигурной скобки")
    
    :handle-else 
    (fn [tokens state] 
      "Обработка ветки else")
    
    :complete-parsing 
    (fn [tokens state] 
      "Завершение парсинга")
    
    :validate-else-open-brace 
    (fn [tokens state] 
      "Валидация открывающей фигурной скобки else")
    
    :parse-else-body 
    (fn [tokens state] 
      "Парсинг тела else")
    
    :validate-else-close-brace 
    (fn [tokens state] 
      "Валидация закрывающей фигурной скобки else")}})

;; Автомат разбора блока if (конечный автомат парсинга)
(defn parse-if-statement
  "Функциональный парсер блока if с использованием конечного автомата
   
   Параметры:
   - tokens: вектор токенов
   - initial-state: начальное состояние парсера
   
   Возвращает структуру разобранного if-блока или nil в случае ошибки"
  [tokens initial-state]
  (let [parse-states
        {:start
         (fn [state]
           (let [current-token (get-current-token tokens state)]
             (if (= current-token :if-keyword)
               {:state (move-next state)
                :next-stage :open-round-bracket}
               {:error "Ожидался токен if"})))

         :open-round-bracket
         (fn [state]
           (let [current-token (get-current-token tokens state)]
             (if (= current-token :open-round-bracket)
               {:state (move-next state)
                :next-stage :condition}
               {:error "Ожидалась открывающая скобка"})))

         :condition
         (fn [state]
           (let [condition-result (parse-expression tokens state)]
             (if (:success condition-result)
               {:state (:state condition-result)
                :next-stage :close-round-bracket
                :condition (:expression condition-result)}
               {:error "Ошибка в условии if"})))

         :close-round-bracket
         (fn [state]
           (let [current-token (get-current-token tokens state)]
             (if (= current-token :close-round-bracket)
               {:state (move-next state)
                :next-stage :open-curly-bracket}
               {:error "Ожидалась закрывающая скобка"})))

         :open-curly-bracket
         (fn [state]
           (let [current-token (get-current-token tokens state)]
             (if (= current-token :open-curly-bracket)
               {:state (move-next state)
                :next-stage :body}
               {:error "Ожидалась открывающая фигурная скобка"})))

         :body
         (fn [state]
           (let [body-result (parse-block tokens state)]
             (if (:success body-result)
               {:state (:state body-result)
                :next-stage :close-curly-bracket
                :body (:block body-result)}
               {:error "Ошибка в теле if"})))

         :close-curly-bracket
         (fn [state]
           (let [current-token (get-current-token tokens state)]
             (if (= current-token :close-curly-bracket)
               {:state (move-next state)
                :next-stage :optional-else}
               {:error "Ожидалась закрывающая фигурная скобка"})))}]
    
    (loop [current-state initial-state
           current-stage :start]
      (if (nil? current-stage)
        {:success true 
         :state current-state}
        (let [stage-result ((get parse-states current-stage) current-state)]
          (if (:error stage-result)
            {:error (:error stage-result)}
            (recur (:state stage-result) 
                   (:next-stage stage-result))))))))

;; Заглушки для демонстрации (должны быть реализованы в соответствующих функциях)
(defn parse-expression [tokens state]
  ;; Временная реализация - просто пропускаем выражение
  {:success true 
   :state (move-next state)
   :expression :placeholder-expression})

(defn parse-block [tokens state]
  ;; Временная реализация - просто пропускаем блок
  {:success true 
   :state (move-next state)
   :block :placeholder-block})

;; 1. Спецификации для while-цикла
(s/def ::condition 
  (s/and 
   vector?                ; Должен быть вектором
   #(every? keyword? %)   ; Содержать только ключевые слова
   #(>= (count %) 1)))    ; Минимум один элемент

(s/def ::body 
  (s/coll-of 
   keyword? 
   :min-count 1 
   :max-count 10))

(s/def ::while-statement 
  (s/keys 
   :req-un [::condition ::body]))

;; 2. Конечный автомат для while-цикла
(def while-fsm 
  {:initial-state :start
   :states 
   {:start 
    {:transitions 
     [{:token :while-keyword 
       :next-state :open-round-bracket
       :action :validate-while-keyword
       :spec-validation 
       (fn [tokens] 
         (s/valid? ::while-statement 
                   {:condition tokens}))}]}
    
    :open-round-bracket 
    {:transitions 
     [{:token :open-round-bracket
       :next-state :condition
       :action :validate-open-round-bracket
       :spec-validation 
       (fn [tokens]
         (s/valid? ::condition tokens))}]}
    
    :condition 
    {:transitions 
     [{:predicate :is-valid-condition
       :next-state :close-round-bracket
       :action :parse-condition
       :spec-validation 
       (fn [tokens] 
         (s/valid? ::condition tokens))}]}
    
    :close-round-bracket 
    {:transitions 
     [{:token :close-round-bracket
       :next-state :open-curly-bracket
       :action :validate-close-round-bracket}]}
    
    :open-curly-bracket 
    {:transitions 
     [{:token :open-curly-bracket
       :next-state :body
       :action :validate-open-curly-bracket
       :spec-validation 
       (fn [tokens]
         (s/valid? ::body tokens))}]}
    
    :body 
    {:transitions 
     [{:predicate :is-valid-body
       :next-state :close-curly-bracket
       :action :parse-body
       :spec-validation 
       (fn [tokens] 
         (s/valid? ::body tokens))}]}
    
    :close-curly-bracket 
    {:transitions 
     [{:token :close-curly-bracket
       :next-state :final
       :action :validate-close-curly-bracket}]}
    
    :final {:type :accept}}
   
   ;; Расширенные предикаты с валидацией
   :predicates 
   {:is-valid-condition 
    (fn [tokens] 
      (s/valid? ::condition tokens))
    
    :is-valid-body 
    (fn [tokens] 
      (s/valid? ::body tokens))}
   
   ;; Действия при переходах
   :actions 
   {:validate-while-keyword 
    (fn [tokens state] 
      "Валидация ключевого слова while")
    
    :validate-open-round-bracket 
    (fn [tokens state] 
      "Валидация открывающей скобки")
    
    :parse-condition 
    (fn [tokens state] 
      "Парсинг условного выражения")
    
    :validate-close-round-bracket 
    (fn [tokens state] 
      "Валидация закрывающей скобки")
    
    :validate-open-curly-bracket 
    (fn [tokens state] 
      "Валидация открывающей фигурной скобки")
    
    :parse-body 
    (fn [tokens state] 
      "Парсинг тела цикла")
    
    :validate-close-curly-bracket 
    (fn [tokens state] 
      "Валидация закрывающей фигурной скобки")}})

;; 3. Функция парсинга while с гибридным подходом
(defn parse-while-statement 
  "Гибридный парсер while-цикла с использованием FSM и спецификаций"
  [tokens initial-state]
  (let [fsm while-fsm]
    (reduce 
     (fn [current-state next-stage]
       (let [stage-config (get-in fsm [:states current-state])
             valid-transition 
             (first 
              (filter 
               (fn [transition]
                 (let [spec-valid? 
                       (if-let [spec-fn (:spec-validation transition)]
                         (spec-fn tokens)
                         true)]
                   spec-valid?))
               (:transitions stage-config)))]
         
         (if valid-transition
           (let [next-state (:next-state valid-transition)
                 action-fn (get-in fsm [:actions (:action transition)])]
             
             (when action-fn 
               (action-fn tokens current-state))
             
             next-state)
           
           (throw 
            (ex-info "Невалидный переход в while-цикле" 
                     {:tokens tokens 
                      :current-state current-state})))))
     
     initial-state
     (keys (:states fsm)))))

;; Пример использования
(defn example-while-parsing []
  (let [sample-tokens 
        [:while-keyword 
         :open-round-bracket 
         :condition-token 
         :close-round-bracket 
         :open-curly-bracket 
         :body-token 
         :close-curly-bracket]]
    
    (try 
      (parse-while-statement sample-tokens :start)
      (catch clojure.lang.ExceptionInfo e
        (println "Ошибка парсинга:" (ex-message e))))))
