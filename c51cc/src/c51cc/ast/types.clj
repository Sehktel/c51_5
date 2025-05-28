(ns c51cc.ast.types
  "Определения типов узлов AST и констант компилятора C51
   
   Этот модуль содержит:
   - Множества всех допустимых типов узлов AST
   - Определения операторов (бинарных и унарных)
   - Константы для типов данных C51
   - Базовые предикаты для проверки типов
   
   Философия модуля: Централизованное определение всех типов
   обеспечивает консистентность и упрощает расширение языка."
  (:require [clojure.set :as set]))

;; ============================================================================
;; ТИПЫ УЗЛОВ AST
;; ============================================================================

(def ^:const ast-node-types
  "Множество всех допустимых типов узлов AST в компиляторе C51
   
   Организовано по категориям для лучшей читаемости:
   - Корневые узлы программы
   - Объявления (declarations)  
   - Операторы (statements)
   - Выражения (expressions)
   - Специфичные для C51 конструкции"
  #{;; Корневые узлы
    :program
    
    ;; Объявления
    :function-declaration
    :variable-declaration
    :parameter-declaration
    
    ;; Операторы (statements)
    :block-statement
    :expression-statement
    :if-statement
    :while-statement
    :for-statement
    :return-statement
    :break-statement
    :continue-statement
    
    ;; Выражения (expressions)
    :binary-expression
    :unary-expression
    :assignment-expression
    :call-expression
    :identifier
    :literal
    :array-access
    :member-access
    
    ;; Специфичные для C51
    :interrupt-declaration
    :sfr-declaration
    :sbit-declaration
    :using-declaration})

;; Категоризация типов узлов для удобства анализа
(def ^:const declaration-types
  "Типы узлов, представляющих объявления"
  #{:function-declaration :variable-declaration :parameter-declaration
    :interrupt-declaration :sfr-declaration :sbit-declaration :using-declaration})

(def ^:const statement-types
  "Типы узлов, представляющих операторы"
  #{:block-statement :expression-statement :if-statement :while-statement
    :for-statement :return-statement :break-statement :continue-statement})

(def ^:const expression-types
  "Типы узлов, представляющих выражения"
  #{:binary-expression :unary-expression :assignment-expression :call-expression
    :identifier :literal :array-access :member-access})

(def ^:const c51-specific-types
  "Типы узлов, специфичные для архитектуры C51"
  #{:interrupt-declaration :sfr-declaration :sbit-declaration :using-declaration})

;; ============================================================================
;; ОПЕРАТОРЫ
;; ============================================================================

(def ^:const binary-operators
  "Множество всех бинарных операторов с их категоризацией
   
   Организовано по семантическим группам для упрощения
   анализа приоритетов и типизации выражений"
  {:arithmetic #{:plus :minus :multiply :divide :modulo}
   :comparison #{:greater :less :greater-equal :less-equal :equal :not-equal}
   :logical    #{:and :or}
   :bitwise    #{:bitwise-and :bitwise-or :bitwise-xor :shift-left :shift-right}
   :assignment #{:assign :plus-equal :minus-equal :multiply-equal :divide-equal
                :modulo-equal :and-equal :or-equal :xor-equal
                :shift-left-equal :shift-right-equal}})

(def ^:const unary-operators
  "Множество всех унарных операторов
   
   Включает как префиксные, так и постфиксные операторы"
  #{:plus :minus :not :bitwise-not 
    :pre-increment :post-increment 
    :pre-decrement :post-decrement 
    :address-of :dereference})

;; Плоские множества для быстрой проверки принадлежности
(def ^:const all-binary-operators
  "Плоское множество всех бинарных операторов для быстрой проверки"
  (reduce set/union (vals binary-operators)))

(def ^:const arithmetic-operators
  "Арифметические операторы для анализа типов"
  (:arithmetic binary-operators))

(def ^:const comparison-operators
  "Операторы сравнения для анализа типов"
  (:comparison binary-operators))

(def ^:const assignment-operators
  "Операторы присваивания для анализа типов"
  (:assignment binary-operators))

;; ============================================================================
;; ПРИОРИТЕТЫ ОПЕРАТОРОВ
;; ============================================================================

(def ^:const operator-precedence
  "Приоритеты операторов (чем больше число, тем выше приоритет)
   
   Соответствует стандарту C для совместимости"
  {;; Постфиксные операторы (самый высокий приоритет)
   :post-increment 16
   :post-decrement 16
   :array-access   16
   :member-access  16
   :call-expression 16
   
   ;; Унарные префиксные операторы
   :pre-increment  15
   :pre-decrement  15
   :address-of     15
   :dereference    15
   :plus           15  ; унарный плюс
   :minus          15  ; унарный минус
   :not            15
   :bitwise-not    15
   
   ;; Мультипликативные
   :multiply       13
   :divide         13
   :modulo         13
   
   ;; Аддитивные
   ;; :plus        12  ; бинарный плюс (конфликт с унарным)
   ;; :minus       12  ; бинарный минус (конфликт с унарным)
   
   ;; Сдвиги
   :shift-left     11
   :shift-right    11
   
   ;; Реляционные
   :less           10
   :less-equal     10
   :greater        10
   :greater-equal  10
   
   ;; Равенство
   :equal          9
   :not-equal      9
   
   ;; Битовые операции
   :bitwise-and    8
   :bitwise-xor    7
   :bitwise-or     6
   
   ;; Логические операции
   :and            5
   :or             4
   
   ;; Присваивание (самый низкий приоритет)
   :assign                1
   :plus-equal           1
   :minus-equal          1
   :multiply-equal       1
   :divide-equal         1
   :modulo-equal         1
   :and-equal            1
   :or-equal             1
   :xor-equal            1
   :shift-left-equal     1
   :shift-right-equal    1})

;; ============================================================================
;; ТИПЫ ДАННЫХ C51
;; ============================================================================

(def ^:const c51-data-types
  "Типы данных, поддерживаемые в C51
   
   Включает как стандартные типы C, так и специфичные для C51"
  #{;; Базовые типы
    :void :char :int :long :float
    
    ;; Модификаторы знака
    :signed :unsigned
    
    ;; Специфичные для C51
    :bit :sbit :sfr :sfr16
    
    ;; Указатели и массивы
    :pointer :array
    
    ;; Составные типы
    :struct :union :enum
    
    ;; Функциональные типы
    :function})

(def ^:const literal-types
  "Типы литералов в языке"
  #{:number :string :char :boolean :null})

;; ============================================================================
;; ПРЕДИКАТЫ ДЛЯ ПРОВЕРКИ ТИПОВ
;; ============================================================================

(defn ast-node-type?
  "Проверяет, является ли тип допустимым типом узла AST"
  [type]
  (contains? ast-node-types type))

(defn declaration-type?
  "Проверяет, является ли тип узлом объявления"
  [type]
  (contains? declaration-types type))

(defn statement-type?
  "Проверяет, является ли тип узлом оператора"
  [type]
  (contains? statement-types type))

(defn expression-type?
  "Проверяет, является ли тип узлом выражения"
  [type]
  (contains? expression-types type))

(defn c51-specific-type?
  "Проверяет, является ли тип специфичным для C51"
  [type]
  (contains? c51-specific-types type))

(defn binary-operator?
  "Проверяет, является ли символ бинарным оператором"
  [op]
  (contains? all-binary-operators op))

(defn unary-operator?
  "Проверяет, является ли символ унарным оператором"
  [op]
  (contains? unary-operators op))

(defn arithmetic-operator?
  "Проверяет, является ли оператор арифметическим"
  [op]
  (contains? arithmetic-operators op))

(defn comparison-operator?
  "Проверяет, является ли оператор сравнения"
  [op]
  (contains? comparison-operators op))

(defn assignment-operator?
  "Проверяет, является ли оператор присваивания"
  [op]
  (contains? assignment-operators op))

(defn get-operator-precedence
  "Возвращает приоритет оператора или nil, если оператор неизвестен"
  [op]
  (get operator-precedence op))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ РАСШИРЕНИЯ ТИПОВ
;; ============================================================================

(defn register-ast-node-type!
  "Регистрирует новый тип узла AST (для плагинов и расширений)
   
   ВНИМАНИЕ: Эта функция изменяет глобальное состояние!
   Используйте только в фазе инициализации компилятора."
  [new-type category]
  {:pre [(keyword? new-type)
         (contains? #{:declaration :statement :expression :c51-specific} category)]}
  ;; В реальной реализации здесь была бы мутация через atom или ref
  ;; Пока оставляем как заглушку для демонстрации архитектуры
  (println (str "ПРЕДУПРЕЖДЕНИЕ: register-ast-node-type! не реализована. "
                "Попытка регистрации типа " new-type " в категории " category)))

(defn validate-type-consistency
  "Проверяет консистентность определений типов
   
   Возвращает вектор ошибок (пустой, если все корректно)"
  []
  (let [errors (atom [])]
    
    ;; Проверяем, что все категории покрывают ast-node-types
    (let [all-categorized (set/union declaration-types statement-types 
                                    expression-types #{:program})
          uncategorized (set/difference ast-node-types all-categorized)]
      (when (seq uncategorized)
        (swap! errors conj {:error :uncategorized-types
                           :types uncategorized
                           :message "Найдены некатегоризированные типы узлов"})))
    
    ;; Проверяем пересечения категорий (должны быть пустыми)
    (let [intersections [(set/intersection declaration-types statement-types)
                        (set/intersection declaration-types expression-types)
                        (set/intersection statement-types expression-types)]]
      (doseq [[i intersection] (map-indexed vector intersections)]
        (when (seq intersection)
          (swap! errors conj {:error :category-intersection
                             :intersection intersection
                             :categories (case i
                                          0 [:declaration :statement]
                                          1 [:declaration :expression]
                                          2 [:statement :expression])
                             :message "Найдены пересечения между категориями типов"}))))
    
    ;; Проверяем, что все операторы имеют приоритеты
    (let [ops-without-precedence (set/difference all-binary-operators 
                                               (set (keys operator-precedence)))]
      (when (seq ops-without-precedence)
        (swap! errors conj {:error :missing-precedence
                           :operators ops-without-precedence
                           :message "Операторы без определенного приоритета"})))
    
    @errors))

;; ============================================================================
;; ЭКСПОРТ ДЛЯ ДРУГИХ МОДУЛЕЙ
;; ============================================================================

;; Основные константы для экспорта
(def ^:export type-definitions
  "Карта всех определений типов для удобного доступа из других модулей"
  {:ast-node-types ast-node-types
   :declaration-types declaration-types
   :statement-types statement-types
   :expression-types expression-types
   :c51-specific-types c51-specific-types
   :binary-operators binary-operators
   :unary-operators unary-operators
   :operator-precedence operator-precedence
   :c51-data-types c51-data-types
   :literal-types literal-types})

;; Основные предикаты для экспорта
(def ^:export type-predicates
  "Карта предикатов для проверки типов"
  {:ast-node-type? ast-node-type?
   :declaration-type? declaration-type?
   :statement-type? statement-type?
   :expression-type? expression-type?
   :c51-specific-type? c51-specific-type?
   :binary-operator? binary-operator?
   :unary-operator? unary-operator?
   :arithmetic-operator? arithmetic-operator?
   :comparison-operator? comparison-operator?
   :assignment-operator? assignment-operator?}) 