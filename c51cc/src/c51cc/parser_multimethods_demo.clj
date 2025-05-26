(ns c51cc.parser-multimethods-demo
  "Демонстрация использования multimethods как альтернативы cond
   для функционального диспетчинга в парсере"
  (:require [c51cc.parser :as parser]))

;; ============================================================================
;; MULTIMETHODS - ФУНКЦИОНАЛЬНАЯ АЛЬТЕРНАТИВА SWITCH-CASE
;; ============================================================================

;; Вместо громоздкого cond:
;; (cond 
;;   (= token-type :number) (parse-number token)
;;   (= token-type :identifier) (parse-identifier token)
;;   (= token-type :string) (parse-string token)
;;   :else (parse-unknown token))

;; Используем multimethods для элегантного диспетчинга:

(defmulti parse-token-by-type 
  "Диспетчер парсинга токенов по типу"
  (fn [token state] (:type token)))

(defmethod parse-token-by-type :number
  [token state]
  (parser/success (parser/literal-node (:value token) :number) state))

(defmethod parse-token-by-type :identifier  
  [token state]
  (parser/success (parser/identifier-node (:value token)) state))

(defmethod parse-token-by-type :string
  [token state]
  (parser/success (parser/literal-node (:value token) :string) state))

(defmethod parse-token-by-type :default
  [token state]
  (parser/failure (str "Неизвестный тип токена: " (:type token)) state))

;; ============================================================================
;; ДИСПЕТЧИНГ ПО НЕСКОЛЬКИМ КРИТЕРИЯМ
;; ============================================================================

(defmulti parse-operator
  "Диспетчер для операторов по типу и значению"
  (fn [token state] [(:type token) (:value token)]))

(defmethod parse-operator [:math-operator :plus]
  [token state]
  (parser/success {:operator :addition :precedence 1} state))

(defmethod parse-operator [:math-operator :multiply]
  [token state]
  (parser/success {:operator :multiplication :precedence 2} state))

(defmethod parse-operator [:logical-operator :and]
  [token state]
  (parser/success {:operator :logical-and :precedence 0} state))

(defmethod parse-operator :default
  [token state]
  (parser/failure (str "Неподдерживаемый оператор: " (:type token) " " (:value token)) state))

;; ============================================================================
;; ИЕРАРХИЧЕСКИЙ ДИСПЕТЧИНГ
;; ============================================================================

;; Создаем иерархию типов выражений
(derive ::arithmetic-expr ::expression)
(derive ::logical-expr ::expression)
(derive ::assignment-expr ::expression)

(derive ::binary-expr ::arithmetic-expr)
(derive ::unary-expr ::arithmetic-expr)

(defmulti optimize-expression
  "Оптимизация выражений с использованием иерархии"
  :ast-type)

(defmethod optimize-expression ::binary-expr
  [expr]
  (println "Оптимизируем бинарное выражение")
  ;; Логика оптимизации бинарных выражений
  expr)

(defmethod optimize-expression ::unary-expr
  [expr]
  (println "Оптимизируем унарное выражение")
  ;; Логика оптимизации унарных выражений
  expr)

(defmethod optimize-expression ::expression
  [expr]
  (println "Базовая оптимизация выражения")
  ;; Базовая оптимизация для всех выражений
  expr)

;; ============================================================================
;; ДИСПЕТЧИНГ ПО СЛОЖНЫМ УСЛОВИЯМ
;; ============================================================================

(defmulti parse-c51-construct
  "Парсинг C51-специфичных конструкций"
  (fn [tokens state]
    (let [first-token (first tokens)
          second-token (second tokens)]
      (cond
        (and (= (:type first-token) :c51-keyword)
             (= (:value first-token) :interrupt)) :interrupt-declaration
        (and (= (:type first-token) :c51-keyword)
             (= (:value first-token) :sfr)) :sfr-declaration
        (and (= (:type first-token) :c51-keyword)
             (= (:value first-token) :sbit)) :sbit-declaration
        (and (= (:type first-token) :type-keyword)
             (= (:type second-token) :identifier)) :function-or-variable
        :else :unknown))))

(defmethod parse-c51-construct :interrupt-declaration
  [tokens state]
  (println "Парсим interrupt декларацию")
  ;; Логика парсинга interrupt
  (parser/success {:type :interrupt} state))

(defmethod parse-c51-construct :sfr-declaration
  [tokens state]
  (println "Парсим SFR декларацию")
  ;; Логика парсинга SFR
  (parser/success {:type :sfr} state))

(defmethod parse-c51-construct :sbit-declaration
  [tokens state]
  (println "Парсим SBIT декларацию")
  ;; Логика парсинга SBIT
  (parser/success {:type :sbit} state))

(defmethod parse-c51-construct :function-or-variable
  [tokens state]
  (println "Парсим функцию или переменную")
  ;; Логика различения функций и переменных
  (parser/success {:type :function-or-variable} state))

(defmethod parse-c51-construct :default
  [tokens state]
  (parser/failure "Неизвестная C51 конструкция" state))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ ПРЕИМУЩЕСТВ
;; ============================================================================

(defn demo-multimethods []
  (println "🎯 === ДЕМОНСТРАЦИЯ MULTIMETHODS === 🎯")
  (println)
  
  ;; Демонстрация парсинга токенов
  (println "📋 Парсинг токенов по типу:")
  (let [number-token {:type :number :value 42}
        id-token {:type :identifier :value "x"}
        unknown-token {:type :unknown :value "???"}
        state {}]
    
    (println "  Число:" (parse-token-by-type number-token state))
    (println "  Идентификатор:" (parse-token-by-type id-token state))
    (println "  Неизвестный:" (parse-token-by-type unknown-token state)))
  
  (println)
  (println "🔧 Парсинг операторов:")
  (let [plus-token {:type :math-operator :value :plus}
        mult-token {:type :math-operator :value :multiply}
        and-token {:type :logical-operator :value :and}
        state {}]
    
    (println "  Плюс:" (parse-operator plus-token state))
    (println "  Умножение:" (parse-operator mult-token state))
    (println "  Логическое И:" (parse-operator and-token state)))
  
  (println)
  (println "🏗️ Иерархическая оптимизация:")
  (optimize-expression {:ast-type ::binary-expr :operator :plus})
  (optimize-expression {:ast-type ::unary-expr :operator :minus})
  (optimize-expression {:ast-type ::assignment-expr :operator :equal})
  
  (println)
  (println "🎉 Multimethods обеспечивают:")
  (println "  • Открытость для расширения (новые методы)")
  (println "  • Закрытость для модификации (существующий код)")
  (println "  • Полиморфизм без наследования")
  (println "  • Диспетчинг по любым критериям")
  (println "  • Иерархические отношения"))

;; ============================================================================
;; СРАВНЕНИЕ С ТРАДИЦИОННЫМИ ПОДХОДАМИ
;; ============================================================================

(defn compare-approaches []
  (println "\n📊 === СРАВНЕНИЕ ПОДХОДОВ === 📊")
  (println)
  
  (println "❌ Традиционный cond (плохо масштабируется):")
  (println "   (cond")
  (println "     (= type :number) (parse-number ...)")
  (println "     (= type :string) (parse-string ...)")
  (println "     (= type :identifier) (parse-identifier ...)")
  (println "     :else (error ...))")
  (println)
  
  (println "✅ Multimethods (отлично масштабируется):")
  (println "   (defmulti parse-token :type)")
  (println "   (defmethod parse-token :number [token] ...)")
  (println "   (defmethod parse-token :string [token] ...)")
  (println "   (defmethod parse-token :identifier [token] ...)")
  (println)
  
  (println "🎯 Преимущества multimethods:")
  (println "   • Новые типы добавляются без изменения существующего кода")
  (println "   • Каждый метод изолирован и тестируется отдельно")
  (println "   • Поддержка иерархий и сложного диспетчинга")
  (println "   • Лучшая читаемость и поддерживаемость"))

;; Экспорт функций
(def ^:export demo-multimethods demo-multimethods)
(def ^:export compare-approaches compare-approaches) 