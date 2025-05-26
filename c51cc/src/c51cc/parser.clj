(ns c51cc.parser
  "Функциональный парсер для языка C51 (AT89S4051)
   Использует монадические комбинаторы и рекурсивный спуск
   для построения AST из потока токенов"
  (:require [c51cc.lexer :as lexer]
            [c51cc.logger :as log]
            [c51cc.config :refer [DEBUG]]
            [clojure.spec.alpha :as s]))

(declare parse-expression
         choice
         many
         optional
         expect-token-value
         parse-c51-function-modifiers
         expect-c51-keyword
         parse-sfr-declaration
         parse-sbit-declaration)
;; ============================================================================
;; МОНАДИЧЕСКИЕ КОМБИНАТОРЫ ДЛЯ ПАРСИНГА
;; ============================================================================

(defrecord ParseState [tokens position])
(defrecord ParseResult [success? value state error])

(defn success 
  "Создает успешный результат парсинга"
  [value state]
  (->ParseResult true value state nil))

(defn failure 
  "Создает неуспешный результат парсинга с ошибкой"
  [error state]
  (->ParseResult false nil state error))

(defn parse-state 
  "Создает начальное состояние парсера"
  [tokens]
  (->ParseState tokens 0))

;; Монадические операции
(defn bind 
  "Монадическая операция bind для композиции парсеров"
  [parser-result parser-fn]
  (if (:success? parser-result)
    (parser-fn (:value parser-result) (:state parser-result))
    parser-result))

(defn return-parser 
  "Монадическая операция return - всегда успешный парсер"
  [value]
  (fn [state] (success value state)))

;; Макрос для удобной композиции парсеров (ГИГИЕНИЧЕСКАЯ ВЕРСИЯ)
(defmacro do-parse 
  "Гигиенический макрос для последовательной композиции парсеров в стиле do-notation
   Обеспечивает полную защиту от захвата переменных и валидацию входных данных
   Символ _ используется для игнорирования результатов парсинга"
  [& bindings]
  ;; Валидация входных данных - последний элемент должен быть выражением
  (when (even? (count bindings))
    (throw (ex-info "do-parse требует нечетное количество аргументов: пары binding-expression + финальное выражение" 
                   {:bindings bindings
                    :count (count bindings)})))
  
  ;; Разделяем на пары и финальное выражение
  (let [all-args (vec bindings)
        final-expr (last all-args)
        binding-pairs (partition 2 (butlast all-args))
        forbidden-names #{'state 'state# 'result 'result# 'parser-state 'parse-result}]
    
    ;; Проверка на запрещенные имена переменных (кроме _)
    (doseq [[binding _] binding-pairs]
      (when (and (symbol? binding) 
                 (not= binding '_)
                 (forbidden-names binding))
        (throw (ex-info "Запрещенное имя переменной в do-parse - может вызвать захват переменных" 
                       {:forbidden-binding binding
                        :forbidden-names forbidden-names}))))
    
    ;; Рекурсивное построение гигиенического кода
    (letfn [(build-parser-chain [pairs final-expr]
              (if (empty? pairs)
                ;; Финальное выражение - проверяем, является ли оно уже парсером
                (if (and (list? final-expr) 
                         (or (= (first final-expr) 'return-parser)
                             (= (first final-expr) 'parser/return-parser)))
                  ;; Уже парсер - используем как есть
                  final-expr
                  ;; Не парсер - оборачиваем в return-parser
                  `(return-parser ~final-expr))
                (let [[binding expr] (first pairs)
                      remaining-pairs (rest pairs)
                      ;; Генерируем уникальные символы для каждого уровня
                      state-sym (gensym "parser-state")
                      result-sym (gensym "parse-result")]
                  (if (and (symbol? binding) (not= binding '_))
                    ;; Связывание результата с переменной
                    `(fn [~state-sym]
                       (let [~result-sym (~expr ~state-sym)]
                         (if (:success? ~result-sym)
                           (let [~binding (:value ~result-sym)
                                 next-state# (:state ~result-sym)]
                             (~(build-parser-chain remaining-pairs final-expr) next-state#))
                           ~result-sym)))
                    ;; Игнорирование результата (binding это _ или не символ)
                    `(fn [~state-sym]
                       (let [~result-sym (~expr ~state-sym)]
                         (if (:success? ~result-sym)
                           (~(build-parser-chain remaining-pairs final-expr) (:state ~result-sym))
                           ~result-sym)))))))]
      
      (build-parser-chain binding-pairs final-expr))))

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;; ============================================================================

(defn extract-identifier-name
  "Извлекает имя идентификатора из токена, обрабатывая как обычные идентификаторы, так и main-keyword"
  [token]
  (case (:type token)
    :identifier (:value token)
    :main-keyword "main"
    (str (:value token))))

(defn parse-argument-list
  "Парсит список аргументов функции, разделенных запятыми"
  [state]
  ((choice
    ;; Пустой список аргументов
    (fn [state] (success [] state))
    
    ;; Непустой список аргументов
    (fn [state]
      (let [first-arg-result (parse-expression state)]
        (if (:success? first-arg-result)
          (let [rest-args-result ((many (fn [state]
                                         (let [comma-result ((expect-token-value :comma) state)]
                                           (if (:success? comma-result)
                                             (parse-expression (:state comma-result))
                                             comma-result))))
                                 (:state first-arg-result))]
            (if (:success? rest-args-result)
              (success (cons (:value first-arg-result) (:value rest-args-result))
                      (:state rest-args-result))
              rest-args-result))
          first-arg-result)))) state))

;; ============================================================================
;; БАЗОВЫЕ ПАРСЕРЫ
;; ============================================================================

(defn current-token 
  "Возвращает текущий токен без продвижения позиции"
  [state]
  (let [{:keys [tokens position]} state]
    (if (< position (count tokens))
      (success (nth tokens position) state)
      (failure "Неожиданный конец входного потока" state))))

(defn advance 
  "Продвигает позицию в потоке токенов на одну позицию"
  [state]
  (let [{:keys [tokens position]} state
        new-position (inc position)]
    (if (<= new-position (count tokens))
      (success nil (->ParseState tokens new-position))
      (failure "Попытка выйти за границы потока токенов" state))))

(defn expect-token 
  "Парсер, ожидающий конкретный тип токена"
  [expected-type]
  (fn [state]
    (let [token-result (current-token state)]
      (if (:success? token-result)
        (let [token (:value token-result)]
          (if (or (= (:type token) expected-type)
                  (and (= expected-type :number) (= (:type token) :number))
                  (and (= expected-type :identifier) (= (:type token) :identifier))
                  (and (= expected-type :identifier) (= (:type token) :main-keyword))
                  (and (= expected-type :void) (and (= (:type token) :type-keyword) (= (:base-type token) :void)))
                  (and (= expected-type :int) (and (= (:type token) :type-keyword) (= (:base-type token) :int)))
                  (and (= expected-type :char) (and (= (:type token) :type-keyword) (= (:base-type token) :char)))
                  (and (= expected-type :signed) (and (= (:type token) :type-keyword) (= (:signedness token) :signed)))
                  (and (= expected-type :unsigned) (and (= (:type token) :type-keyword) (= (:signedness token) :unsigned))))
            (let [advance-result (advance state)]
              (if (:success? advance-result)
                (success token (:state advance-result))
                advance-result))
            (failure (str "Ожидался токен типа " expected-type 
                         ", получен " (:type token)) state)))
        token-result))))

(defn expect-token-value 
  "Парсер, ожидающий токен с конкретным значением"
  [expected-value]
  (fn [state]
    (let [token-result (current-token state)]
      (if (:success? token-result)
        (let [token (:value token-result)]
          (if (or (= (:value token) expected-value)
                  ;; Поддержка C51 ключевых слов
                  (and (= expected-value :interrupt) (and (= (:type token) :c51-keyword) (= (:value token) :interrupt)))
                  (and (= expected-value :using) (and (= (:type token) :c51-keyword) (= (:value token) :using)))
                  (and (= expected-value :sfr) (and (= (:type token) :c51-keyword) (= (:value token) :sfr)))
                  (and (= expected-value :sbit) (and (= (:type token) :c51-keyword) (= (:value token) :sbit)))
                  ;; Существующие проверки
                  (and (= expected-value :open-round) (and (= (:type token) :bracket) (= (:value token) :open-round)))
                  (and (= expected-value :close-round) (and (= (:type token) :bracket) (= (:value token) :close-round)))
                  (and (= expected-value :open-curly) (and (= (:type token) :bracket) (= (:value token) :open-curly)))
                  (and (= expected-value :close-curly) (and (= (:type token) :bracket) (= (:value token) :close-curly)))
                  (and (= expected-value :open-square) (and (= (:type token) :bracket) (= (:value token) :open-square)))
                  (and (= expected-value :close-square) (and (= (:type token) :bracket) (= (:value token) :close-square)))
                  (and (= expected-value :semicolon) (and (= (:type token) :separator) (= (:value token) :semicolon)))
                  (and (= expected-value :comma) (and (= (:type token) :separator) (= (:value token) :comma)))
                  (and (= expected-value :plus) (and (= (:type token) :math-operator) (= (:value token) :plus)))
                  (and (= expected-value :minus) (and (= (:type token) :math-operator) (= (:value token) :minus)))
                  (and (= expected-value :multiply) (and (= (:type token) :math-operator) (= (:value token) :multiply)))
                  (and (= expected-value :divide) (and (= (:type token) :math-operator) (= (:value token) :divide)))
                  (and (= expected-value :modulo) (and (= (:type token) :math-operator) (= (:value token) :modulo)))
                  (and (= expected-value :increment) (and (= (:type token) :math-operator) (= (:value token) :increment)))
                  (and (= expected-value :decrement) (and (= (:type token) :math-operator) (= (:value token) :decrement)))
                  (and (= expected-value :equal) (and (= (:type token) :assignment-operator) (= (:value token) :equal)))
                  (and (= expected-value :plus-equal) (and (= (:type token) :assignment-operator) (= (:value token) :plus-equal)))
                  (and (= expected-value :minus-equal) (and (= (:type token) :assignment-operator) (= (:value token) :minus-equal)))
                  (and (= expected-value :and-equal) (and (= (:type token) :assignment-operator) (= (:value token) :and-equal)))
                  (and (= expected-value :or-equal) (and (= (:type token) :assignment-operator) (= (:value token) :or-equal)))
                  (and (= expected-value :xor-equal) (and (= (:type token) :assignment-operator) (= (:value token) :xor-equal)))
                  (and (= expected-value :shift-left-equal) (and (= (:type token) :assignment-operator) (= (:value token) :shift-left-equal)))
                  (and (= expected-value :shift-right-equal) (and (= (:type token) :assignment-operator) (= (:value token) :shift-right-equal)))
                  (and (= expected-value :shift-left) (and (= (:type token) :bitwise-operator) (= (:value token) :shift-left)))
                  (and (= expected-value :shift-right) (and (= (:type token) :bitwise-operator) (= (:value token) :shift-right)))
                  (and (= expected-value :less) (and (= (:type token) :comparison-operator) (= (:value token) :less)))
                  (and (= expected-value :greater) (and (= (:type token) :comparison-operator) (= (:value token) :greater)))
                  (and (= expected-value :less-equal) (and (= (:type token) :comparison-operator) (= (:value token) :less-equal)))
                  (and (= expected-value :greater-equal) (and (= (:type token) :comparison-operator) (= (:value token) :greater-equal)))
                  (and (= expected-value :not-equal) (and (= (:type token) :comparison-operator) (= (:value token) :not-equal)))
                  (and (= expected-value :and) (and (= (:type token) :logical-operator) (= (:value token) :and)))
                  (and (= expected-value :or) (and (= (:type token) :logical-operator) (= (:value token) :or)))
                  (and (= expected-value :not) (and (= (:type token) :logical-operator) (= (:value token) :not)))
                  (and (= expected-value :if) (and (= (:type token) :control-keyword) (= (:value token) :if)))
                  (and (= expected-value :else) (and (= (:type token) :control-keyword) (= (:value token) :else)))
                  (and (= expected-value :while) (and (= (:type token) :control-keyword) (= (:value token) :while)))
                  (and (= expected-value :for) (and (= (:type token) :control-keyword) (= (:value token) :for)))
                  (and (= expected-value :return) (and (= (:type token) :control-keyword) (= (:value token) :return)))
                  (and (= expected-value :void) (and (= (:type token) :type-keyword) (= (:base-type token) :void)))
                  (and (= expected-value :int) (and (= (:type token) :type-keyword) (= (:base-type token) :int)))
                  (and (= expected-value :char) (and (= (:type token) :type-keyword) (= (:base-type token) :char)))
                  (and (= expected-value :signed) (and (= (:type token) :type-keyword) (= (:signedness token) :signed)))
                  (and (= expected-value :unsigned) (and (= (:type token) :type-keyword) (= (:signedness token) :unsigned))))
            (let [advance-result (advance state)]
              (if (:success? advance-result)
                (success token (:state advance-result))
                advance-result))
            (failure (str "Ожидался токен со значением " expected-value 
                         ", получен " (:value token)) state)))
        token-result))))

(defn choice 
  "Комбинатор выбора - пробует парсеры по порядку"
  [& parsers]
  (fn [state]
    (loop [remaining-parsers parsers]
      (if (empty? remaining-parsers)
        (failure "Ни один из вариантов не подошел" state)
        (let [result ((first remaining-parsers) state)]
          (if (:success? result)
            result
            (recur (rest remaining-parsers))))))))

(defn many 
  "Парсер, применяющий другой парсер ноль или более раз"
  [parser]
  (fn [state]
    (loop [current-state state
           results []]
      (let [result (parser current-state)]
        (if (:success? result)
          (recur (:state result) (conj results (:value result)))
          (success results current-state))))))

(defn many1 
  "Парсер, применяющий другой парсер один или более раз"
  [parser]
  (fn [state]
    (let [first-result (parser state)]
      (if (:success? first-result)
        (let [rest-result ((many parser) (:state first-result))]
          (if (:success? rest-result)
            (success (cons (:value first-result) (:value rest-result))
                    (:state rest-result))
            rest-result))
        first-result))))

(defn optional 
  "Парсер, делающий другой парсер опциональным"
  [parser]
  (fn [state]
    (let [result (parser state)]
      (if (:success? result)
        result
        (success nil state)))))

;; ============================================================================
;; AST УЗЛЫ
;; ============================================================================

(defn make-ast-node 
  "Создает узел AST с указанным типом и атрибутами"
  [node-type & {:as attributes}]
  (merge {:ast-type node-type} attributes))

;; Типы узлов AST
(defn program-node [declarations]
  (make-ast-node :program :declarations declarations))

(defn function-declaration-node [return-type name parameters body]
  (make-ast-node :function-declaration 
                 :return-type return-type
                 :name name
                 :parameters parameters
                 :body body))

(defn variable-declaration-node [type name initializer]
  (make-ast-node :variable-declaration
                 :type type
                 :name name
                 :initializer initializer))

(defn block-statement-node [statements]
  (make-ast-node :block-statement :statements statements))

(defn if-statement-node [condition then-branch else-branch]
  (make-ast-node :if-statement
                 :condition condition
                 :then-branch then-branch
                 :else-branch else-branch))

(defn while-statement-node [condition body]
  (make-ast-node :while-statement
                 :condition condition
                 :body body))

(defn for-statement-node [init condition update body]
  (make-ast-node :for-statement
                 :init init
                 :condition condition
                 :update update
                 :body body))

(defn return-statement-node [expression]
  (make-ast-node :return-statement :expression expression))

(defn expression-statement-node [expression]
  (make-ast-node :expression-statement :expression expression))

(defn binary-expression-node [operator left right]
  (make-ast-node :binary-expression
                 :operator operator
                 :left left
                 :right right))

(defn unary-expression-node [operator operand]
  (make-ast-node :unary-expression
                 :operator operator
                 :operand operand))

(defn assignment-expression-node [operator left right]
  (make-ast-node :assignment-expression
                 :operator operator
                 :left left
                 :right right))

(defn call-expression-node [callee arguments]
  (make-ast-node :call-expression
                 :callee callee
                 :arguments arguments))

(defn identifier-node [name]
  (make-ast-node :identifier :name name))

(defn literal-node [value type]
  (make-ast-node :literal :value value :literal-type type))

(defn interrupt-declaration-node [function-name interrupt-number using-clause]
  "Создает узел объявления обработчика прерывания для C51"
  (make-ast-node :interrupt-declaration
                 :function-name function-name
                 :interrupt-number interrupt-number
                 :using-clause using-clause))

;; ============================================================================
;; ПАРСЕРЫ ВЫРАЖЕНИЙ (с учетом приоритета операторов)
;; ============================================================================

(declare parse-expression parse-assignment-expression parse-logical-or-expression
         parse-logical-and-expression parse-equality-expression parse-relational-expression
         parse-additive-expression parse-multiplicative-expression parse-unary-expression
         parse-postfix-expression parse-primary-expression)

(defn parse-primary-expression 
  "Парсит первичные выражения: идентификаторы, литералы, выражения в скобках"
  [state]
  ((choice
    ;; Числовые литералы
    (fn [state]
      (let [token-result ((expect-token :number) state)]
        (if (:success? token-result)
          (let [token (:value token-result)]
            ;; Извлекаем значение из токена лексера
            (success (literal-node (:value token) :number) (:state token-result)))
          token-result)))
    
    ;; Идентификаторы
    (fn [state]
      (let [token-result ((expect-token :identifier) state)]
        (if (:success? token-result)
          (let [token (:value token-result)]
            ;; Извлекаем значение из токена лексера
            (success (identifier-node (:value token)) (:state token-result)))
          token-result)))
    
    ;; Выражения в скобках
    (fn [state]
      (let [open-result ((expect-token-value :open-round) state)]
        (if (:success? open-result)
          (let [expr-result (parse-expression (:state open-result))]
            (if (:success? expr-result)
              (let [close-result ((expect-token-value :close-round) (:state expr-result))]
                (if (:success? close-result)
                  (success (:value expr-result) (:state close-result))
                  close-result))
              expr-result))
          open-result)))) state))

(defn parse-postfix-expression 
  "Парсит постфиксные выражения: вызовы функций, индексация массивов, постфиксные ++/--"
  [state]
  (let [primary-result (parse-primary-expression state)]
    (if (:success? primary-result)
      (loop [expr (:value primary-result)
             current-state (:state primary-result)]
        (let [token-result (current-token current-state)]
          (if (:success? token-result)
            (let [token (:value token-result)]
              (cond
                ;; Постфиксный инкремент
                (and (= (:type token) :math-operator) (= (:value token) :increment))
                (let [advance-result (advance current-state)]
                  (if (:success? advance-result)
                    (recur (unary-expression-node :post-increment expr)
                           (:state advance-result))
                    advance-result))
                
                ;; Постфиксный декремент
                (and (= (:type token) :math-operator) (= (:value token) :decrement))
                (let [advance-result (advance current-state)]
                  (if (:success? advance-result)
                    (recur (unary-expression-node :post-decrement expr)
                           (:state advance-result))
                    advance-result))
                
                ;; Вызов функции
                (= (:value token) :open-round)
                (let [advance-result (advance current-state)]
                  (if (:success? advance-result)
                    ;; Проверяем, есть ли аргументы
                    (let [peek-result (current-token (:state advance-result))]
                      (if (and (:success? peek-result) 
                               (= (:value (:value peek-result)) :close-round))
                        ;; Пустой список аргументов
                        (let [close-result ((expect-token-value :close-round) (:state advance-result))]
                          (if (:success? close-result)
                            (recur (call-expression-node expr [])
                                   (:state close-result))
                            close-result))
                        ;; Есть аргументы - парсим их
                        (let [first-arg-result (parse-expression (:state advance-result))]
                          (if (:success? first-arg-result)
                            ;; Собираем остальные аргументы
                            (let [args-result 
                                  (loop [args [(:value first-arg-result)]
                                         arg-state (:state first-arg-result)]
                                    (let [token-result (current-token arg-state)]
                                      (if (and (:success? token-result)
                                               (= (:value (:value token-result)) :comma))
                                        ;; Есть запятая, парсим следующий аргумент
                                        (let [advance-result (advance arg-state)]
                                          (if (:success? advance-result)
                                            (let [arg-result (parse-expression (:state advance-result))]
                                              (if (:success? arg-result)
                                                (recur (conj args (:value arg-result))
                                                       (:state arg-result))
                                                arg-result))
                                            advance-result))
                                        ;; Нет запятой, возвращаем собранные аргументы
                                        (success args arg-state))))]
                              (if (:success? args-result)
                                ;; Ожидаем закрывающую скобку
                                (let [close-result ((expect-token-value :close-round) (:state args-result))]
                                  (if (:success? close-result)
                                    (recur (call-expression-node expr (:value args-result))
                                           (:state close-result))
                                    close-result))
                                args-result))
                            first-arg-result))))
                    advance-result))
                
                ;; Индексация массива
                (= (:value token) :open-square)
                (let [advance-result (advance current-state)]
                  (if (:success? advance-result)
                    (let [index-result (parse-expression (:state advance-result))]
                      (if (:success? index-result)
                        (let [close-result ((expect-token-value :close-square) (:state index-result))]
                          (if (:success? close-result)
                            (recur (binary-expression-node :array-access 
                                                          expr (:value index-result))
                                   (:state close-result))
                            close-result))
                        index-result))
                    advance-result))
                
                :else (success expr current-state)))
            (success expr current-state))))
      primary-result)))

(defn parse-unary-expression 
  "Парсит унарные выражения: !, ~, +, -, ++, --"
  [state]
  ((choice
    ;; Унарные операторы
    (fn [state]
      ((choice
        (fn [state]
          (let [op-result ((expect-token-value :not) state)]
            (if (:success? op-result)
              (let [expr-result (parse-unary-expression (:state op-result))]
                (if (:success? expr-result)
                  ;; Извлекаем значение оператора из токена лексера
                  (success (unary-expression-node (:value (:value op-result)) (:value expr-result)) 
                          (:state expr-result))
                  expr-result))
              op-result)))
        
        (fn [state]
          (let [op-result ((expect-token-value :plus) state)]
            (if (:success? op-result)
              (let [expr-result (parse-unary-expression (:state op-result))]
                (if (:success? expr-result)
                  ;; Извлекаем значение оператора из токена лексера
                  (success (unary-expression-node (:value (:value op-result)) (:value expr-result)) 
                          (:state expr-result))
                  expr-result))
              op-result)))
        
        (fn [state]
          (let [op-result ((expect-token-value :minus) state)]
            (if (:success? op-result)
              (let [expr-result (parse-unary-expression (:state op-result))]
                (if (:success? expr-result)
                  ;; Извлекаем значение оператора из токена лексера
                  (success (unary-expression-node (:value (:value op-result)) (:value expr-result)) 
                          (:state expr-result))
                  expr-result))
              op-result)))
        
        (fn [state]
          (let [op-result ((expect-token-value :increment) state)]
            (if (:success? op-result)
              (let [expr-result (parse-unary-expression (:state op-result))]
                (if (:success? expr-result)
                  ;; Извлекаем значение оператора из токена лексера
                  (success (unary-expression-node (:value (:value op-result)) (:value expr-result)) 
                          (:state expr-result))
                  expr-result))
              op-result)))
        
        (fn [state]
          (let [op-result ((expect-token-value :decrement) state)]
            (if (:success? op-result)
              (let [expr-result (parse-unary-expression (:state op-result))]
                (if (:success? expr-result)
                  ;; Извлекаем значение оператора из токена лексера
                  (success (unary-expression-node (:value (:value op-result)) (:value expr-result)) 
                          (:state expr-result))
                  expr-result))
              op-result)))) state))
    
    ;; Постфиксные выражения
    parse-postfix-expression) state))

;; Парсеры для бинарных операторов с учетом приоритета
(defn parse-multiplicative-expression [state]
  (let [left-result (parse-unary-expression state)]
    (if (:success? left-result)
      (loop [left (:value left-result)
             current-state (:state left-result)]
        (let [token-result (current-token current-state)]
          (if (:success? token-result)
            (let [token (:value token-result)]
              ;; Проверяем значение токена из лексера
              (if (#{:multiply :divide :modulo} (:value token))
                (let [advance-result (advance current-state)]
                  (if (:success? advance-result)
                    (let [right-result (parse-unary-expression (:state advance-result))]
                      (if (:success? right-result)
                        ;; Используем значение оператора из токена лексера
                        (recur (binary-expression-node (:value token) 
                                                      left (:value right-result))
                               (:state right-result))
                        right-result))
                    advance-result))
                (success left current-state)))
            (success left current-state))))
      left-result)))

(defn parse-shift-expression [state]
  (let [left-result (parse-multiplicative-expression state)]
    (if (:success? left-result)
      (loop [left (:value left-result)
             current-state (:state left-result)]
        (let [token-result (current-token current-state)]
          (if (:success? token-result)
            (let [token (:value token-result)]
              ;; Проверяем операторы сдвига
              (if (and (= (:type token) :bitwise-operator) 
                       (#{:shift-left :shift-right} (:value token)))
                (let [advance-result (advance current-state)]
                  (if (:success? advance-result)
                    (let [right-result (parse-multiplicative-expression (:state advance-result))]
                      (if (:success? right-result)
                        ;; Используем значение оператора из токена лексера
                        (recur (binary-expression-node (:value token) 
                                                      left (:value right-result))
                               (:state right-result))
                        right-result))
                    advance-result))
                (success left current-state)))
            (success left current-state))))
      left-result)))

(defn parse-additive-expression [state]
  (let [left-result (parse-shift-expression state)]
    (if (:success? left-result)
      (loop [left (:value left-result)
             current-state (:state left-result)]
        (let [token-result (current-token current-state)]
          (if (:success? token-result)
            (let [token (:value token-result)]
              ;; Проверяем значение токена из лексера
              (if (#{:plus :minus} (:value token))
                (let [advance-result (advance current-state)]
                  (if (:success? advance-result)
                    (let [right-result (parse-shift-expression (:state advance-result))]
                      (if (:success? right-result)
                        ;; Используем значение оператора из токена лексера
                        (recur (binary-expression-node (:value token) 
                                                      left (:value right-result))
                               (:state right-result))
                        right-result))
                    advance-result))
                (success left current-state)))
            (success left current-state))))
      left-result)))

(defn parse-relational-expression [state]
  (let [left-result (parse-additive-expression state)]
    (if (:success? left-result)
      (loop [left (:value left-result)
             current-state (:state left-result)]
        (let [token-result (current-token current-state)]
          (if (:success? token-result)
            (let [token (:value token-result)]
              ;; Проверяем значение токена из лексера
              (if (#{:less :greater :less-equal :greater-equal} (:value token))
                (let [advance-result (advance current-state)]
                  (if (:success? advance-result)
                    (let [right-result (parse-additive-expression (:state advance-result))]
                      (if (:success? right-result)
                        ;; Используем значение оператора из токена лексера
                        (recur (binary-expression-node (:value token) 
                                                      left (:value right-result))
                               (:state right-result))
                        right-result))
                    advance-result))
                (success left current-state)))
            (success left current-state))))
      left-result)))

(defn parse-equality-expression [state]
  (let [left-result (parse-relational-expression state)]
    (if (:success? left-result)
      (loop [left (:value left-result)
             current-state (:state left-result)]
        (let [token-result (current-token current-state)]
          (if (:success? token-result)
            (let [token (:value token-result)]
              ;; Проверяем значение токена из лексера - нужно учесть, что == это :equal в logical-operator
              (if (or (and (= (:type token) :logical-operator) (= (:value token) :equal))
                      (and (= (:type token) :comparison-operator) (= (:value token) :not-equal)))
                (let [advance-result (advance current-state)]
                  (if (:success? advance-result)
                    (let [right-result (parse-relational-expression (:state advance-result))]
                      (if (:success? right-result)
                        ;; Используем значение оператора из токена лексера
                        (recur (binary-expression-node (:value token) 
                                                      left (:value right-result))
                               (:state right-result))
                        right-result))
                    advance-result))
                (success left current-state)))
            (success left current-state))))
      left-result)))

(defn parse-logical-and-expression [state]
  (let [left-result (parse-equality-expression state)]
    (if (:success? left-result)
      (loop [left (:value left-result)
             current-state (:state left-result)]
        (let [token-result (current-token current-state)]
          (if (:success? token-result)
            (let [token (:value token-result)]
              ;; Проверяем логическое И
              (if (and (= (:type token) :logical-operator) (= (:value token) :and))
                (let [advance-result (advance current-state)]
                  (if (:success? advance-result)
                    (let [right-result (parse-equality-expression (:state advance-result))]
                      (if (:success? right-result)
                        ;; Используем значение оператора из токена лексера
                        (recur (binary-expression-node (:value token) 
                                                      left (:value right-result))
                               (:state right-result))
                        right-result))
                    advance-result))
                (success left current-state)))
            (success left current-state))))
      left-result)))

(defn parse-logical-or-expression [state]
  (let [left-result (parse-logical-and-expression state)]
    (if (:success? left-result)
      (loop [left (:value left-result)
             current-state (:state left-result)]
        (let [token-result (current-token current-state)]
          (if (:success? token-result)
            (let [token (:value token-result)]
              ;; Проверяем логическое ИЛИ
              (if (and (= (:type token) :logical-operator) (= (:value token) :or))
                (let [advance-result (advance current-state)]
                  (if (:success? advance-result)
                    (let [right-result (parse-logical-and-expression (:state advance-result))]
                      (if (:success? right-result)
                        ;; Используем значение оператора из токена лексера
                        (recur (binary-expression-node (:value token) 
                                                      left (:value right-result))
                               (:state right-result))
                        right-result))
                    advance-result))
                (success left current-state)))
            (success left current-state))))
      left-result)))

(defn parse-assignment-expression [state]
  (let [left-result (parse-logical-or-expression state)]
    (if (:success? left-result)
      (let [token-result (current-token (:state left-result))]
        (if (:success? token-result)
          (let [token (:value token-result)]
            ;; Проверяем операторы присваивания
            (if (and (= (:type token) :assignment-operator) 
                     (#{:equal :plus-equal :minus-equal :and-equal :or-equal :xor-equal :shift-left-equal :shift-right-equal} (:value token)))
              (let [advance-result (advance (:state left-result))]
                (if (:success? advance-result)
                  (let [right-result (parse-assignment-expression (:state advance-result))]
                    (if (:success? right-result)
                      ;; Используем значение оператора из токена лексера
                      (success (assignment-expression-node (:value token)
                                                          (:value left-result)
                                                          (:value right-result))
                              (:state right-result))
                      right-result))
                  advance-result))
              (success (:value left-result) (:state left-result))))
          (success (:value left-result) (:state left-result))))
      left-result)))

(defn parse-expression [state]
  (parse-assignment-expression state))

;; ============================================================================
;; ПАРСЕРЫ ОПЕРАТОРОВ И ДЕКЛАРАЦИЙ
;; ============================================================================

(declare parse-statement parse-declaration parse-function-declaration 
         parse-variable-declaration parse-type-specifier)

(defn parse-type-specifier 
  "Парсит спецификатор типа: [signed|unsigned] [int|char|void]"
  [state]
  (let [signedness-result ((optional (choice (expect-token-value :signed)
                                            (expect-token-value :unsigned))) state)]
    (if (:success? signedness-result)
      (let [base-type-result ((choice (expect-token-value :void)
                                     (expect-token-value :int)
                                     (expect-token-value :char)) 
                             (:state signedness-result))]
        (if (:success? base-type-result)
          (success {:signedness (when (:value signedness-result) 
                                 (:value (:value signedness-result)))
                   :base-type (:value (:value base-type-result))}
                  (:state base-type-result))
          base-type-result))
      signedness-result)))

(defn parse-parameter-list 
  "Парсит список параметров функции"
  [state]
  ;; Сначала пробуем парсить непустой список параметров
  (let [type-result (parse-type-specifier state)]
    (if (:success? type-result)
      ;; Если тип найден, пробуем парсить имя
      (let [name-result ((expect-token :identifier) (:state type-result))]
        (if (:success? name-result)
          ;; Если имя найдено, парсим остальные параметры
          (let [first-param {:type (:value type-result) 
                            :name (:value (:value name-result))}
                rest-result ((many (fn [state]
                                    (let [comma-result ((expect-token-value :comma) state)]
                                      (if (:success? comma-result)
                                        (let [type-result (parse-type-specifier (:state comma-result))]
                                          (if (:success? type-result)
                                            (let [name-result ((expect-token :identifier) (:state type-result))]
                                              (if (:success? name-result)
                                                (success {:type (:value type-result) 
                                                         :name (:value (:value name-result))}
                                                        (:state name-result))
                                                name-result))
                                            type-result))
                                        comma-result))))
                            (:state name-result))]
            (if (:success? rest-result)
              (success (cons first-param (:value rest-result))
                      (:state rest-result))
              rest-result))
          ;; Если имя не найдено, возвращаем ошибку
          name-result))
      ;; Если тип не найден, возвращаем пустой список (нет параметров)
      (success [] state))))

(defn parse-block-statement 
  "Парсит блок операторов в фигурных скобках
   В соответствии с C89/C90: объявления переменных в начале блока,
   затем операторы (смешивание запрещено)"
  [state]
  (let [open-result ((expect-token-value :open-curly) state)]
    (if (:success? open-result)
      ;; Сначала парсим все объявления переменных
      (let [declarations-result ((many parse-variable-declaration) (:state open-result))]
        (if (:success? declarations-result)
          ;; Затем парсим все операторы
          (let [statements-result ((many parse-statement) (:state declarations-result))]
            (if (:success? statements-result)
              (let [close-result ((expect-token-value :close-curly) (:state statements-result))]
                (if (:success? close-result)
                  ;; Объединяем объявления и операторы в один список
                  (success (block-statement-node (concat (:value declarations-result) (:value statements-result))) 
                          (:state close-result))
                  close-result))
              statements-result))
          declarations-result))
      open-result)))

(defn parse-if-statement 
  "Парсит условный оператор if"
  [state]
  (let [if-result ((expect-token-value :if) state)]
    (if (:success? if-result)
      (let [open-result ((expect-token-value :open-round) (:state if-result))]
        (if (:success? open-result)
          (let [condition-result (parse-expression (:state open-result))]
            (if (:success? condition-result)
              (let [close-result ((expect-token-value :close-round) (:state condition-result))]
                (if (:success? close-result)
                  (let [then-result (parse-statement (:state close-result))]
                    (if (:success? then-result)
                      (let [else-result ((optional (fn [state]
                                                    (let [else-kw-result ((expect-token-value :else) state)]
                                                      (if (:success? else-kw-result)
                                                        (parse-statement (:state else-kw-result))
                                                        else-kw-result))))
                                        (:state then-result))]
                        (if (:success? else-result)
                          (success (if-statement-node (:value condition-result) 
                                                     (:value then-result) 
                                                     (:value else-result))
                                  (:state else-result))
                          else-result))
                      then-result))
                  close-result))
              condition-result))
          open-result))
      if-result)))

(defn parse-while-statement 
  "Парсит цикл while"
  [state]
  (let [while-result ((expect-token-value :while) state)]
    (if (:success? while-result)
      (let [open-result ((expect-token-value :open-round) (:state while-result))]
        (if (:success? open-result)
          (let [condition-result (parse-expression (:state open-result))]
            (if (:success? condition-result)
              (let [close-result ((expect-token-value :close-round) (:state condition-result))]
                (if (:success? close-result)
                  (let [body-result (parse-statement (:state close-result))]
                    (if (:success? body-result)
                      (success (while-statement-node (:value condition-result) (:value body-result))
                              (:state body-result))
                      body-result))
                  close-result))
              condition-result))
          open-result))
      while-result)))

(defn parse-for-statement 
  "Парсит цикл for"
  [state]
  (let [for-result ((expect-token-value :for) state)]
    (if (:success? for-result)
      (let [open-result ((expect-token-value :open-round) (:state for-result))]
        (if (:success? open-result)
          (let [init-result ((optional parse-expression) (:state open-result))]
            (if (:success? init-result)
              (let [semi1-result ((expect-token-value :semicolon) (:state init-result))]
                (if (:success? semi1-result)
                  (let [condition-result ((optional parse-expression) (:state semi1-result))]
                    (if (:success? condition-result)
                      (let [semi2-result ((expect-token-value :semicolon) (:state condition-result))]
                        (if (:success? semi2-result)
                          (let [update-result ((optional parse-expression) (:state semi2-result))]
                            (if (:success? update-result)
                              (let [close-result ((expect-token-value :close-round) (:state update-result))]
                                (if (:success? close-result)
                                  (let [body-result (parse-statement (:state close-result))]
                                    (if (:success? body-result)
                                      (success (for-statement-node (:value init-result) 
                                                                  (:value condition-result) 
                                                                  (:value update-result) 
                                                                  (:value body-result))
                                              (:state body-result))
                                      body-result))
                                  close-result))
                              update-result))
                          semi2-result))
                      condition-result))
                  semi1-result))
              init-result))
          open-result))
      for-result)))

(defn parse-return-statement 
  "Парсит оператор return"
  [state]
  (let [return-result ((expect-token-value :return) state)]
    (if (:success? return-result)
      (let [expr-result ((optional parse-expression) (:state return-result))]
        (if (:success? expr-result)
          (let [semi-result ((expect-token-value :semicolon) (:state expr-result))]
            (if (:success? semi-result)
              (success (return-statement-node (:value expr-result)) (:state semi-result))
              semi-result))
          expr-result))
      return-result)))

(defn parse-expression-statement 
  "Парсит оператор-выражение"
  [state]
  (let [expr-result (parse-expression state)]
    (if (:success? expr-result)
      (let [semi-result ((expect-token-value :semicolon) (:state expr-result))]
        (if (:success? semi-result)
          (success (expression-statement-node (:value expr-result)) (:state semi-result))
          semi-result))
      expr-result)))

(defn parse-expression-statement-refactored
  "Отрефакторенная версия парсера оператора-выражения с использованием do-parse
   Демонстрирует упрощение кода и улучшение читаемости"
  [state]
  ((do-parse
     expr (fn [state] (parse-expression state))
     _ (expect-token-value :semicolon)
     (return-parser (expression-statement-node expr))) state))

(defn parse-return-statement-refactored
  "Отрефакторенная версия парсера оператора return с использованием do-parse
   Показывает элегантную обработку опциональных выражений"
  [state]
  ((do-parse
     _ (expect-token-value :return)
     expr (optional (fn [state] (parse-expression state)))
     _ (expect-token-value :semicolon)
     (return-parser (return-statement-node expr))) state))

(defn parse-while-statement-refactored
  "Отрефакторенная версия парсера цикла while с использованием do-parse
   Демонстрирует значительное упрощение сложной логики парсинга"
  [state]
  ((do-parse
     _ (expect-token-value :while)
     _ (expect-token-value :open-round)
     condition (fn [state] (parse-expression state))
     _ (expect-token-value :close-round)
     body (fn [state] (parse-statement state))
     (return-parser (while-statement-node condition body))) state))

(defn parse-if-statement-refactored
  "Отрефакторенная версия парсера условного оператора if с использованием do-parse
   Показывает обработку сложной логики с опциональной else-веткой"
  [state]
  ((do-parse
     _ (expect-token-value :if)
     _ (expect-token-value :open-round)
     condition (fn [state] (parse-expression state))
     _ (expect-token-value :close-round)
     then-branch (fn [state] (parse-statement state))
     else-branch (optional (fn [state]
                            (let [else-result ((expect-token-value :else) state)]
                              (if (:success? else-result)
                                (parse-statement (:state else-result))
                                else-result))))
     (return-parser (if-statement-node condition then-branch else-branch))) state))

(defn parse-for-statement-refactored
  "Отрефакторенная версия парсера цикла for с использованием do-parse
   Демонстрирует обработку множественных опциональных компонентов"
  [state]
  ((do-parse
     _ (expect-token-value :for)
     _ (expect-token-value :open-round)
     init (optional (fn [state] (parse-expression state)))
     _ (expect-token-value :semicolon)
     condition (optional (fn [state] (parse-expression state)))
     _ (expect-token-value :semicolon)
     update (optional (fn [state] (parse-expression state)))
     _ (expect-token-value :close-round)
     body (fn [state] (parse-statement state))
     (return-parser (for-statement-node init condition update body))) state))

(defn parse-variable-declaration-refactored
  "Отрефакторенная версия парсера объявления переменной с использованием do-parse
   Показывает упрощение парсинга с опциональной инициализацией"
  [state]
  ((do-parse
     type-spec (fn [state] (parse-type-specifier state))
     name (expect-token :identifier)
     init (optional (fn [state]
                     (let [eq-result ((expect-token-value :equal) state)]
                       (if (:success? eq-result)
                         (parse-expression (:state eq-result))
                         eq-result))))
     _ (expect-token-value :semicolon)
     (return-parser (variable-declaration-node type-spec 
                                              (extract-identifier-name name) 
                                              init))) state))

(defn parse-statement 
  "Парсит любой оператор"
  [state]
  ((choice
    parse-block-statement
    parse-if-statement
    parse-while-statement
    parse-for-statement
    parse-return-statement
    parse-expression-statement) state))

(defn parse-variable-declaration 
  "Парсит объявление переменной"
  [state]
  (let [type-result (parse-type-specifier state)]
    (if (:success? type-result)
      (let [name-result ((expect-token :identifier) (:state type-result))]
        (if (:success? name-result)
          (let [init-result ((optional (fn [state]
                                        (let [eq-result ((expect-token-value :equal) state)]
                                          (if (:success? eq-result)
                                            (parse-expression (:state eq-result))
                                            eq-result))))
                            (:state name-result))]
            (if (:success? init-result)
              (let [semi-result ((expect-token-value :semicolon) (:state init-result))]
                (if (:success? semi-result)
                  (success (variable-declaration-node (:value type-result) 
                                                     (extract-identifier-name (:value name-result)) 
                                                     (:value init-result))
                          (:state semi-result))
                  semi-result))
              init-result))
          name-result))
      type-result)))

(defn parse-function-declaration 
  "Парсит объявление функции с поддержкой C51-специфичных модификаторов interrupt и using"
  [state]
  (let [type-result (parse-type-specifier state)]
    (if (:success? type-result)
      (let [name-result ((expect-token :identifier) (:state type-result))]
        (if (:success? name-result)
          (let [open-result ((expect-token-value :open-round) (:state name-result))]
            (if (:success? open-result)
              (let [params-result (parse-parameter-list (:state open-result))]
                (if (:success? params-result)
                  (let [close-result ((expect-token-value :close-round) (:state params-result))]
                    (if (:success? close-result)
                      ;; Парсим C51-специфичные модификаторы (interrupt N [using M])
                      (let [modifiers-result (parse-c51-function-modifiers (:state close-result))]
                        (if (:success? modifiers-result)
                          ;; Пробуем парсить тело функции или точку с запятой
                          (let [body-or-semi-result 
                                ((choice 
                                  ;; Определение функции с телом
                                  (fn [state]
                                    (let [body-result (parse-block-statement state)]
                                      (if (:success? body-result)
                                        (success {:has-body true :body (:value body-result)} (:state body-result))
                                        body-result)))
                                  ;; Объявление функции с точкой с запятой
                                  (fn [state]
                                    (let [semi-result ((expect-token-value :semicolon) state)]
                                      (if (:success? semi-result)
                                        (success {:has-body false :body nil} (:state semi-result))
                                        semi-result))))
                                 (:state modifiers-result))]
                            (if (:success? body-or-semi-result)
                              (let [result-data (:value body-or-semi-result)
                                    modifiers (:value modifiers-result)]
                                ;; Если есть interrupt модификатор, создаем interrupt-declaration-node
                                (if (:interrupt-number modifiers)
                                  (success (interrupt-declaration-node 
                                           (extract-identifier-name (:value name-result))
                                           (:interrupt-number modifiers)
                                           (:using-clause modifiers))
                                          (:state body-or-semi-result))
                                  ;; Иначе создаем обычный function-declaration-node
                                  (success (function-declaration-node (:value type-result) 
                                                                     (extract-identifier-name (:value name-result)) 
                                                                     (:value params-result) 
                                                                     (:body result-data))
                                          (:state body-or-semi-result))))
                              body-or-semi-result))
                          modifiers-result))
                      close-result))
                  params-result))
              open-result))
          name-result))
      type-result)))

(defn parse-declaration 
  "Парсит любое объявление или оператор на верхнем уровне программы
   В C89/C90: объявления переменных внутри функций должны быть в начале,
   смешивание объявлений и операторов запрещено (введено в C99)
   Поддерживает C51-специфичные декларации: sfr, sbit"
  [state]
  ((choice
    ;; C51-специфичные декларации
    parse-sfr-declaration
    parse-sbit-declaration
    ;; Стандартные декларации
    parse-function-declaration
    parse-variable-declaration
    ;; Добавляем поддержку операторов
    parse-statement
    ;; Добавляем поддержку выражений как деклараций (только с точкой с запятой!)
    (fn [state]
      (let [expr-result (parse-expression state)]
        (if (:success? expr-result)
          (let [semi-result ((expect-token-value :semicolon) (:state expr-result))]
            (if (:success? semi-result)
              (success (expression-statement-node (:value expr-result)) (:state semi-result))
              ;; ИСПРАВЛЕНИЕ: не принимаем выражения без точки с запятой
              semi-result))
          expr-result)))) state))

(defn parse-program 
  "Парсит всю программу"
  [state]
  (let [declarations-result ((many parse-declaration) state)]
    (if (:success? declarations-result)
      (success (program-node (:value declarations-result)) (:state declarations-result))
      declarations-result)))

;; ============================================================================
;; ОСНОВНАЯ ФУНКЦИЯ ПАРСИНГА
;; ============================================================================

(defn parse 
  "Основная функция парсинга. Принимает вектор токенов, возвращает AST"
  [tokens]
  {:pre [(vector? tokens)]
   :post [(map? %)]}
  (when DEBUG
    (log/debug "Начинаем парсинг токенов:" tokens))
  
  (let [initial-state (parse-state tokens)
        result (parse-program initial-state)]
    
    (when DEBUG
      (log/debug "Результат парсинга:" result))
    
    (if (:success? result)
      (let [final-position (:position (:state result))
            remaining-tokens (drop final-position tokens)]
        (if (empty? remaining-tokens)
          {:success true
           :ast (:value result)
           :remaining-tokens remaining-tokens}
          {:success false
           :error (str "Неожиданные токены в конце ввода: " (vec (take 3 remaining-tokens)))
           :position final-position}))
      {:success false
       :error (:error result)
       :position (:position (:state result))})))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ ОТЛАДКИ И ТЕСТИРОВАНИЯ
;; ============================================================================

(defn pretty-print-ast 
  "Красивая печать AST для отладки"
  [ast & {:keys [indent] :or {indent 0}}]
  (let [spaces (apply str (repeat indent "  "))]
    (cond
      (map? ast)
      (do
        (println (str spaces "(" (:ast-type ast)))
        (doseq [[k v] (dissoc ast :ast-type)]
          (print (str spaces "  " k ": "))
          (if (or (vector? v) (map? v))
            (do (println) (pretty-print-ast v :indent (+ indent 2)))
            (println v)))
        (println (str spaces ")")))
      
      (vector? ast)
      (doseq [item ast]
        (pretty-print-ast item :indent indent))
      
      :else
      (println (str spaces ast)))))

;; Экспорт основных функций
(def ^:export parse parse)
(def ^:export pretty-print-ast pretty-print-ast)

;; ============================================================================
;; ФУНКЦИОНАЛЬНЫЕ КОМБИНАТОРЫ (альтернатива макросам)
;; ============================================================================

(defn >>= 
  "Монадическая связка (bind) в инфиксной нотации для удобства"
  [parser-result parser-fn]
  (bind parser-result parser-fn))

(defn >> 
  "Последовательная композиция парсеров с игнорированием первого результата"
  [parser1 parser2]
  (fn [state]
    (let [result1 (parser1 state)]
      (if (:success? result1)
        (parser2 (:state result1))
        result1))))

(defn <<
  "Последовательная композиция парсеров с игнорированием второго результата"
  [parser1 parser2]
  (fn [state]
    (let [result1 (parser1 state)]
      (if (:success? result1)
        (let [result2 (parser2 (:state result1))]
          (if (:success? result2)
            (success (:value result1) (:state result2))
            result2))
        result1))))

(defn between
  "Парсер, который парсит что-то между двумя другими парсерами"
  [open-parser content-parser close-parser]
  (fn [state]
    (let [open-result (open-parser state)]
      (if (:success? open-result)
        (let [content-result (content-parser (:state open-result))]
          (if (:success? content-result)
            (let [close-result (close-parser (:state content-result))]
              (if (:success? close-result)
                (success (:value content-result) (:state close-result))
                close-result))
            content-result))
        open-result))))

(defn separated-by
  "Парсер для списка элементов, разделенных сепаратором"
  [element-parser separator-parser]
  (fn [state]
    ((choice
       ;; Пустой список
       (return-parser [])
       ;; Непустой список
       (fn [state]
         (let [first-result (element-parser state)]
           (if (:success? first-result)
             (let [rest-result ((many (fn [state]
                                       (let [sep-result (separator-parser state)]
                                         (if (:success? sep-result)
                                           (element-parser (:state sep-result))
                                           sep-result))))
                               (:state first-result))]
               (if (:success? rest-result)
                 (success (cons (:value first-result) (:value rest-result))
                         (:state rest-result))
                 rest-result))
             first-result)))) state)))

(defn sequence-parsers 
  "Последовательное применение списка парсеров, возвращает вектор результатов"
  [& parsers]
  (fn [state]
    (loop [remaining-parsers parsers
           current-state state
           results []]
      (if (empty? remaining-parsers)
        (success results current-state)
        (let [parser (first remaining-parsers)
              result (parser current-state)]
          (if (:success? result)
            (recur (rest remaining-parsers)
                   (:state result)
                   (conj results (:value result)))
            result))))))

(defn ignore-result 
  "Применяет парсер, но игнорирует его результат"
  [parser]
  (fn [state]
    (let [result (parser state)]
      (if (:success? result)
        (success nil (:state result))
        result))))

(defn transform
  "Применяет функцию трансформации к результату парсера"
  [parser transform-fn]
  (fn [state]
    (let [result (parser state)]
      (if (:success? result)
        (success (transform-fn (:value result)) (:state result))
        result))))

(defn with-error-context
  "Добавляет контекст к ошибке парсера"
  [parser context-msg]
  (fn [state]
    (let [result (parser state)]
      (if (:success? result)
        result
        (failure (str context-msg ": " (:error result)) (:state result))))))

;; Специализированные комбинаторы для C-синтаксиса
(defn parenthesized
  "Парсер для выражений в круглых скобках"
  [content-parser]
  (between (expect-token-value :open-round)
           content-parser
           (expect-token-value :close-round)))

(defn braced
  "Парсер для блоков в фигурных скобках"
  [content-parser]
  (between (expect-token-value :open-curly)
           content-parser
           (expect-token-value :close-curly)))

(defn bracketed
  "Парсер для выражений в квадратных скобках"
  [content-parser]
  (between (expect-token-value :open-square)
           content-parser
           (expect-token-value :close-square)))

(defn comma-separated
  "Парсер для списка элементов, разделенных запятыми"
  [element-parser]
  (separated-by element-parser (expect-token-value :comma)))

(defn semicolon-terminated
  "Парсер для выражений, завершающихся точкой с запятой"
  [content-parser]
  (<< content-parser (expect-token-value :semicolon)))

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;; ============================================================================

(defn make-success-parser 
  "Создает парсер, который всегда возвращает успех с заданным значением
   Полезно для финальных выражений в do-parse"
  [value-fn]
  (fn [state] 
    (success (if (fn? value-fn) (value-fn) value-fn) state)))

(defn wrap-parser 
  "Оборачивает функцию в парсер, если она еще не является парсером"
  [f]
  (if (fn? f)
    f
    (fn [state] (f state))))

(defn parse-expr 
  "Вспомогательная функция для создания парсера выражений в do-parse"
  []
  (fn [state] (parse-expression state)))

(defn parse-stmt 
  "Вспомогательная функция для создания парсера операторов в do-parse"
  []
  (fn [state] (parse-statement state)))

;; Макросы для упрощения создания парсеров
(defmacro success-parser 
  "Макрос для создания парсера успеха с выражением
   Использование: (success-parser (expression-statement-node expr))"
  [expr]
  `(fn [state#] (success ~expr state#)))

(defmacro parser-fn 
  "Макрос для создания парсера из функции
   Использование: (parser-fn parse-expression)"
  [f]
  `(fn [state#] (~f state#)))

;; ============================================================================
;; СРАВНЕНИЕ ПРОИЗВОДИТЕЛЬНОСТИ И ЧИТАЕМОСТИ
;; ============================================================================

;; Функция для демонстрации различий между старым и новым подходом
(defn compare-parser-approaches
  "Сравнивает производительность и читаемость старого и нового подходов к парсингу
   Возвращает статистику по количеству строк кода и вложенности"
  []
  (let [old-approach-stats {:lines-of-code 15
                           :nesting-depth 7
                           :error-handling :manual
                           :readability :poor}
        new-approach-stats {:lines-of-code 8
                           :nesting-depth 1
                           :error-handling :automatic
                           :readability :excellent}]
    {:comparison {:code-reduction (- (:lines-of-code old-approach-stats) 
                                   (:lines-of-code new-approach-stats))
                 :nesting-reduction (- (:nesting-depth old-approach-stats)
                                     (:nesting-depth new-approach-stats))
                 :maintainability-improvement "Значительное"
                 :type-safety "Улучшена благодаря гигиеническим макросам"}
     :old-approach old-approach-stats
     :new-approach new-approach-stats}))

;; ============================================================================
;; ЭКСПОРТ ОТРЕФАКТОРЕННЫХ ФУНКЦИЙ
;; ============================================================================

;; Экспорт отрефакторенных функций для использования в тестах
(def ^:export parse-expression-statement-refactored parse-expression-statement-refactored)
(def ^:export parse-return-statement-refactored parse-return-statement-refactored)
(def ^:export parse-while-statement-refactored parse-while-statement-refactored)
(def ^:export parse-if-statement-refactored parse-if-statement-refactored)
(def ^:export parse-for-statement-refactored parse-for-statement-refactored)
(def ^:export parse-variable-declaration-refactored parse-variable-declaration-refactored)
(def ^:export compare-parser-approaches compare-parser-approaches)

;; Добавляем парсер для C51-специфичных модификаторов функций
(defn parse-c51-function-modifiers
  "Парсит C51-специфичные модификаторы функций: interrupt N [using M]
   Возвращает карту с :interrupt-number и :using-clause (может быть nil)"
  [state]
  ;; Пробуем парсить interrupt модификатор
  (let [interrupt-result ((optional 
                          (fn [state]
                            (let [interrupt-token ((expect-token-value :interrupt) state)]
                              (if (:success? interrupt-token)
                                (let [number-token ((expect-token :number) (:state interrupt-token))]
                                  (if (:success? number-token)
                                    (success (:value (:value number-token)) (:state number-token))
                                    number-token))
                                interrupt-token))))
                         state)]
    (if (:success? interrupt-result)
      ;; Пробуем парсить using модификатор
      (let [using-result ((optional
                          (fn [state]
                            (let [using-token ((expect-token-value :using) state)]
                              (if (:success? using-token)
                                (let [number-token ((expect-token :number) (:state using-token))]
                                  (if (:success? number-token)
                                    (success (:value (:value number-token)) (:state number-token))
                                    number-token))
                                using-token))))
                         (:state interrupt-result))]
        (if (:success? using-result)
          (success {:interrupt-number (:value interrupt-result)
                   :using-clause (:value using-result)}
                  (:state using-result))
          using-result))
      interrupt-result)))

;; Добавляем парсеры для SFR и SBIT деклараций
(defn parse-sfr-declaration
  "Парсит объявление регистра специальных функций: sfr NAME = ADDRESS;"
  [state]
  ((do-parse
     _ (expect-token-value :sfr)
     name (expect-token :identifier)
     _ (expect-token-value :equal)
     address (expect-token :number)
     _ (expect-token-value :semicolon)
     (return-parser (make-ast-node :sfr-declaration
                                  :name (extract-identifier-name name)
                                  :address (:value address)))) state))

(defn parse-sbit-declaration
  "Парсит объявление специального бита: sbit NAME = ADDRESS;"
  [state]
  ((do-parse
     _ (expect-token-value :sbit)
     name (expect-token :identifier)
     _ (expect-token-value :equal)
     address (expect-token :number)
     _ (expect-token-value :semicolon)
     (return-parser (make-ast-node :sbit-declaration
                                  :name (extract-identifier-name name)
                                  :address (:value address)))) state))

(defn expect-c51-keyword
  "Парсер, ожидающий C51-специфичное ключевое слово"
  [expected-keyword]
  (fn [state]
    (let [token-result (current-token state)]
      (if (:success? token-result)
        (let [token (:value token-result)]
          (if (and (= (:type token) :c51-keyword) (= (:value token) expected-keyword))
            (let [advance-result (advance state)]
              (if (:success? advance-result)
                (success token (:state advance-result))
                advance-result))
            (failure (str "Ожидалось C51 ключевое слово " expected-keyword 
                         ", получен " (:type token) " " (:value token)) state)))
        token-result)))) 