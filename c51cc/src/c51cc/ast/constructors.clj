(ns c51cc.ast.constructors
  "Конструкторы узлов AST компилятора C51
   
   Этот модуль предоставляет:
   - Базовую функцию создания узлов AST с валидацией
   - Специализированные конструкторы для всех типов узлов
   - Фабричные функции для создания сложных структур
   - Утилиты для клонирования и модификации узлов
   
   Философия модуля: Каждый конструктор должен создавать валидный узел
   с полными метаданными и проверкой предусловий."
  (:require [c51cc.ast.types :as types]
            [clojure.string :as str]))

;; ============================================================================
;; БАЗОВЫЙ КОНСТРУКТОР УЗЛОВ
;; ============================================================================

(defn make-ast-node
  "Создает узел AST с валидацией типа и обязательными метаданными
   
   Аргументы:
   - node-type: тип узла из ast-node-types
   - attributes: карта атрибутов узла
   
   Возвращает: карту, представляющую узел AST с метаданными
   
   Каждый узел содержит:
   - :ast-type - тип узла для диспетчеризации
   - :source-location - информация о позиции в исходном коде
   - :type-info - информация о типах для семантического анализа  
   - :scope-info - информация об области видимости
   - :node-id - уникальный идентификатор узла (для отладки)"
  [node-type & {:as attributes}]
  {:pre [(types/ast-node-type? node-type)
         (map? attributes)]
   :post [(map? %)
          (= (:ast-type %) node-type)
          (contains? % :node-id)]}
  (merge {:ast-type node-type
          :source-location nil  ; будет заполнено парсером
          :type-info nil        ; информация о типах для семантического анализа
          :scope-info nil       ; информация об области видимости
          :node-id (gensym "node-")} ; уникальный ID для отладки
         attributes))

;; ============================================================================
;; КОНСТРУКТОРЫ КОРНЕВЫХ УЗЛОВ
;; ============================================================================

(defn program-node 
  "Создает корневой узел программы
   
   Аргументы:
   - declarations: последовательность объявлений верхнего уровня
   
   Инварианты:
   - Все элементы declarations должны быть узлами объявлений
   - Программа может быть пустой (declarations = [])"
  [declarations]
  {:pre [(sequential? declarations)
         (every? map? declarations)
         (every? #(types/declaration-type? (:ast-type %)) declarations)]
   :post [(= (:ast-type %) :program)]}
  (make-ast-node :program :declarations (vec declarations)))

;; ============================================================================
;; КОНСТРУКТОРЫ ОБЪЯВЛЕНИЙ
;; ============================================================================

(defn function-declaration-node 
  "Создает узел объявления функции
   
   Аргументы:
   - return-type: узел типа возвращаемого значения
   - name: имя функции (строка)
   - parameters: вектор узлов параметров
   - body: узел тела функции (обычно block-statement)
   
   Инварианты:
   - Имя функции не может быть пустым
   - Все параметры должны быть parameter-declaration узлами
   - Тело должно быть statement узлом"
  [return-type name parameters body]
  {:pre [(map? return-type)
         (string? name)
         (not (str/blank? name))
         (sequential? parameters)
         (every? #(= (:ast-type %) :parameter-declaration) parameters)
         (map? body)
         (types/statement-type? (:ast-type body))]
   :post [(= (:ast-type %) :function-declaration)]}
  (make-ast-node :function-declaration
                 :return-type return-type
                 :name name
                 :parameters (vec parameters)
                 :body body))

(defn variable-declaration-node 
  "Создает узел объявления переменной
   
   Аргументы:
   - type: узел типа переменной
   - name: имя переменной (строка)
   - initializer: узел инициализатора (может быть nil)
   
   Инварианты:
   - Имя переменной не может быть пустым
   - Если initializer не nil, должен быть expression узлом"
  [type name initializer]
  {:pre [(map? type)
         (string? name)
         (not (str/blank? name))
         (or (nil? initializer) 
             (and (map? initializer) 
                  (types/expression-type? (:ast-type initializer))))]
   :post [(= (:ast-type %) :variable-declaration)]}
  (make-ast-node :variable-declaration
                 :type type
                 :name name
                 :initializer initializer))

(defn parameter-declaration-node
  "Создает узел объявления параметра функции
   
   Аргументы:
   - type: узел типа параметра
   - name: имя параметра (строка)
   
   Инварианты:
   - Имя параметра не может быть пустым"
  [type name]
  {:pre [(map? type)
         (string? name)
         (not (str/blank? name))]
   :post [(= (:ast-type %) :parameter-declaration)]}
  (make-ast-node :parameter-declaration
                 :type type
                 :name name))

;; ============================================================================
;; КОНСТРУКТОРЫ ОПЕРАТОРОВ (STATEMENTS)
;; ============================================================================

(defn block-statement-node 
  "Создает узел блока операторов
   
   Аргументы:
   - statements: последовательность операторов
   
   Инварианты:
   - Все элементы должны быть statement узлами
   - Блок может быть пустым"
  [statements]
  {:pre [(sequential? statements)
         (every? map? statements)
         (every? #(types/statement-type? (:ast-type %)) statements)]
   :post [(= (:ast-type %) :block-statement)]}
  (make-ast-node :block-statement :statements (vec statements)))

(defn if-statement-node 
  "Создает узел условного оператора
   
   Аргументы:
   - condition: узел условного выражения
   - then-branch: узел оператора для истинной ветки
   - else-branch: узел оператора для ложной ветки (может быть nil)
   
   Инварианты:
   - Условие должно быть expression узлом
   - Ветки должны быть statement узлами"
  [condition then-branch else-branch]
  {:pre [(map? condition)
         (types/expression-type? (:ast-type condition))
         (map? then-branch)
         (types/statement-type? (:ast-type then-branch))
         (or (nil? else-branch) 
             (and (map? else-branch) 
                  (types/statement-type? (:ast-type else-branch))))]
   :post [(= (:ast-type %) :if-statement)]}
  (make-ast-node :if-statement
                 :condition condition
                 :then-branch then-branch
                 :else-branch else-branch))

(defn while-statement-node 
  "Создает узел цикла while
   
   Аргументы:
   - condition: узел условного выражения
   - body: узел тела цикла
   
   Инварианты:
   - Условие должно быть expression узлом
   - Тело должно быть statement узлом"
  [condition body]
  {:pre [(map? condition)
         (types/expression-type? (:ast-type condition))
         (map? body)
         (types/statement-type? (:ast-type body))]
   :post [(= (:ast-type %) :while-statement)]}
  (make-ast-node :while-statement
                 :condition condition
                 :body body))

(defn for-statement-node 
  "Создает узел цикла for
   
   Аргументы:
   - init: узел инициализации (может быть nil)
   - condition: узел условия (может быть nil)
   - update: узел обновления (может быть nil)
   - body: узел тела цикла
   
   Инварианты:
   - init может быть statement или expression узлом
   - condition должно быть expression узлом (если не nil)
   - update должно быть expression узлом (если не nil)
   - body должно быть statement узлом"
  [init condition update body]
  {:pre [(or (nil? init) 
             (and (map? init) 
                  (or (types/statement-type? (:ast-type init))
                      (types/expression-type? (:ast-type init)))))
         (or (nil? condition) 
             (and (map? condition) 
                  (types/expression-type? (:ast-type condition))))
         (or (nil? update) 
             (and (map? update) 
                  (types/expression-type? (:ast-type update))))
         (map? body)
         (types/statement-type? (:ast-type body))]
   :post [(= (:ast-type %) :for-statement)]}
  (make-ast-node :for-statement
                 :init init
                 :condition condition
                 :update update
                 :body body))

(defn return-statement-node 
  "Создает узел оператора return
   
   Аргументы:
   - expression: узел возвращаемого выражения (может быть nil для void)
   
   Инварианты:
   - Если expression не nil, должно быть expression узлом"
  [expression]
  {:pre [(or (nil? expression) 
             (and (map? expression) 
                  (types/expression-type? (:ast-type expression))))]
   :post [(= (:ast-type %) :return-statement)]}
  (make-ast-node :return-statement :expression expression))

(defn break-statement-node
  "Создает узел оператора break"
  []
  (make-ast-node :break-statement))

(defn continue-statement-node
  "Создает узел оператора continue"
  []
  (make-ast-node :continue-statement))

(defn expression-statement-node 
  "Создает узел оператора-выражения
   
   Аргументы:
   - expression: узел выражения
   
   Инварианты:
   - expression должно быть expression узлом"
  [expression]
  {:pre [(map? expression)
         (types/expression-type? (:ast-type expression))]
   :post [(= (:ast-type %) :expression-statement)]}
  (make-ast-node :expression-statement :expression expression))

;; ============================================================================
;; КОНСТРУКТОРЫ ВЫРАЖЕНИЙ (EXPRESSIONS)
;; ============================================================================

(defn binary-expression-node 
  "Создает узел бинарного выражения
   
   Аргументы:
   - operator: символ оператора
   - left: узел левого операнда
   - right: узел правого операнда
   
   Инварианты:
   - operator должен быть валидным бинарным оператором
   - Операнды должны быть expression узлами"
  [operator left right]
  {:pre [(keyword? operator)
         (types/binary-operator? operator)
         (map? left)
         (types/expression-type? (:ast-type left))
         (map? right)
         (types/expression-type? (:ast-type right))]
   :post [(= (:ast-type %) :binary-expression)]}
  (make-ast-node :binary-expression
                 :operator operator
                 :left left
                 :right right))

(defn unary-expression-node 
  "Создает узел унарного выражения
   
   Аргументы:
   - operator: символ оператора
   - operand: узел операнда
   
   Инварианты:
   - operator должен быть валидным унарным оператором
   - operand должен быть expression узлом"
  [operator operand]
  {:pre [(keyword? operator)
         (types/unary-operator? operator)
         (map? operand)
         (types/expression-type? (:ast-type operand))]
   :post [(= (:ast-type %) :unary-expression)]}
  (make-ast-node :unary-expression
                 :operator operator
                 :operand operand))

(defn assignment-expression-node 
  "Создает узел выражения присваивания
   
   Аргументы:
   - operator: символ оператора присваивания
   - left: узел левого операнда (lvalue)
   - right: узел правого операнда (rvalue)
   
   Инварианты:
   - operator должен быть валидным оператором присваивания
   - Операнды должны быть expression узлами"
  [operator left right]
  {:pre [(keyword? operator)
         (types/assignment-operator? operator)
         (map? left)
         (types/expression-type? (:ast-type left))
         (map? right)
         (types/expression-type? (:ast-type right))]
   :post [(= (:ast-type %) :assignment-expression)]}
  (make-ast-node :assignment-expression
                 :operator operator
                 :left left
                 :right right))

(defn call-expression-node 
  "Создает узел вызова функции
   
   Аргументы:
   - callee: узел вызываемого выражения
   - arguments: последовательность узлов аргументов
   
   Инварианты:
   - callee должен быть expression узлом
   - Все аргументы должны быть expression узлами"
  [callee arguments]
  {:pre [(map? callee)
         (types/expression-type? (:ast-type callee))
         (sequential? arguments)
         (every? map? arguments)
         (every? #(types/expression-type? (:ast-type %)) arguments)]
   :post [(= (:ast-type %) :call-expression)]}
  (make-ast-node :call-expression
                 :callee callee
                 :arguments (vec arguments)))

(defn identifier-node 
  "Создает узел идентификатора
   
   Аргументы:
   - name: имя идентификатора (строка)
   
   Инварианты:
   - Имя не может быть пустым или состоять только из пробелов"
  [name]
  {:pre [(string? name)
         (not (str/blank? name))]
   :post [(= (:ast-type %) :identifier)]}
  (make-ast-node :identifier :name name))

(defn literal-node 
  "Создает узел литерала
   
   Аргументы:
   - value: значение литерала
   - literal-type: тип литерала
   
   Инварианты:
   - literal-type должен быть валидным типом литерала"
  [value literal-type]
  {:pre [(keyword? literal-type)
         (contains? types/literal-types literal-type)]
   :post [(= (:ast-type %) :literal)]}
  (make-ast-node :literal 
                 :value value 
                 :literal-type literal-type))

(defn array-access-node
  "Создает узел доступа к элементу массива
   
   Аргументы:
   - array: узел массива
   - index: узел индекса
   
   Инварианты:
   - Оба аргумента должны быть expression узлами"
  [array index]
  {:pre [(map? array)
         (types/expression-type? (:ast-type array))
         (map? index)
         (types/expression-type? (:ast-type index))]
   :post [(= (:ast-type %) :array-access)]}
  (make-ast-node :array-access
                 :array array
                 :index index))

(defn member-access-node
  "Создает узел доступа к члену структуры
   
   Аргументы:
   - object: узел объекта
   - member: имя члена (строка)
   - is-pointer: флаг, указывающий на использование -> вместо .
   
   Инварианты:
   - object должен быть expression узлом
   - member не может быть пустым"
  [object member is-pointer]
  {:pre [(map? object)
         (types/expression-type? (:ast-type object))
         (string? member)
         (not (str/blank? member))
         (boolean? is-pointer)]
   :post [(= (:ast-type %) :member-access)]}
  (make-ast-node :member-access
                 :object object
                 :member member
                 :is-pointer is-pointer))

;; ============================================================================
;; КОНСТРУКТОРЫ СПЕЦИФИЧНЫХ ДЛЯ C51 УЗЛОВ
;; ============================================================================

(defn interrupt-declaration-node
  "Создает узел объявления обработчика прерывания
   
   Аргументы:
   - function-name: имя функции-обработчика
   - interrupt-number: номер прерывания (0-31)
   - using-clause: номер банка регистров (0-3, может быть nil)
   
   Инварианты:
   - Номер прерывания должен быть в диапазоне 0-31
   - Номер банка регистров должен быть в диапазоне 0-3"
  [function-name interrupt-number using-clause]
  {:pre [(string? function-name)
         (not (str/blank? function-name))
         (integer? interrupt-number)
         (<= 0 interrupt-number 31)
         (or (nil? using-clause) 
             (and (integer? using-clause) 
                  (<= 0 using-clause 3)))]
   :post [(= (:ast-type %) :interrupt-declaration)]}
  (make-ast-node :interrupt-declaration
                 :function-name function-name
                 :interrupt-number interrupt-number
                 :using-clause using-clause))

(defn sfr-declaration-node
  "Создает узел объявления регистра специальных функций
   
   Аргументы:
   - name: имя SFR
   - address: адрес в памяти (0x80-0xFF)
   
   Инварианты:
   - Адрес должен быть в диапазоне SFR (0x80-0xFF)"
  [name address]
  {:pre [(string? name)
         (not (str/blank? name))
         (integer? address)
         (<= 0x80 address 0xFF)]
   :post [(= (:ast-type %) :sfr-declaration)]}
  (make-ast-node :sfr-declaration
                 :name name
                 :address address))

(defn sbit-declaration-node
  "Создает узел объявления специального бита
   
   Аргументы:
   - name: имя SBIT
   - address: адрес бита (0x80-0xFF для битово-адресуемых SFR)
   
   Инварианты:
   - Адрес должен быть в диапазоне битово-адресуемых SFR"
  [name address]
  {:pre [(string? name)
         (not (str/blank? name))
         (integer? address)
         (<= 0x80 address 0xFF)]
   :post [(= (:ast-type %) :sbit-declaration)]}
  (make-ast-node :sbit-declaration
                 :name name
                 :address address))

(defn using-declaration-node
  "Создает узел объявления использования банка регистров
   
   Аргументы:
   - bank-number: номер банка регистров (0-3)
   
   Инварианты:
   - Номер банка должен быть в диапазоне 0-3"
  [bank-number]
  {:pre [(integer? bank-number)
         (<= 0 bank-number 3)]
   :post [(= (:ast-type %) :using-declaration)]}
  (make-ast-node :using-declaration
                 :bank-number bank-number))

;; ============================================================================
;; ФАБРИЧНЫЕ ФУНКЦИИ ДЛЯ СЛОЖНЫХ СТРУКТУР
;; ============================================================================

(defn simple-function
  "Фабричная функция для создания простой функции
   
   Создает функцию с базовыми параметрами и пустым телом"
  [name return-type-keyword param-specs]
  {:pre [(string? name)
         (keyword? return-type-keyword)
         (sequential? param-specs)]}
  (let [return-type (make-ast-node :type-specifier :type return-type-keyword)
        parameters (mapv (fn [[param-type param-name]]
                          (parameter-declaration-node
                            (make-ast-node :type-specifier :type param-type)
                            param-name))
                        param-specs)
        body (block-statement-node [])]
    (function-declaration-node return-type name parameters body)))

(defn simple-variable
  "Фабричная функция для создания простой переменной"
  [name type-keyword initial-value]
  {:pre [(string? name)
         (keyword? type-keyword)]}
  (let [type (make-ast-node :type-specifier :type type-keyword)
        initializer (when initial-value
                     (literal-node initial-value 
                                  (cond
                                    (number? initial-value) :number
                                    (string? initial-value) :string
                                    (char? initial-value) :char
                                    (boolean? initial-value) :boolean
                                    :else :number)))]
    (variable-declaration-node type name initializer)))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ КЛОНИРОВАНИЯ И МОДИФИКАЦИИ
;; ============================================================================

(defn clone-node
  "Создает глубокую копию узла AST с новым node-id
   
   Полезно для трансформаций, где нужно сохранить оригинальную структуру"
  [node]
  {:pre [(map? node)
         (contains? node :ast-type)]
   :post [(map? %)
          (= (:ast-type %) (:ast-type node))
          (not= (:node-id %) (:node-id node))]}
  (let [cloned (assoc node :node-id (gensym "cloned-node-"))]
    ;; Рекурсивно клонируем дочерние узлы
    (clojure.walk/postwalk
      (fn [x]
        (if (and (map? x) (contains? x :ast-type) (not= x cloned))
          (assoc x :node-id (gensym "cloned-node-"))
          x))
      cloned)))

(defn update-node
  "Обновляет узел AST с сохранением валидности
   
   Аргументы:
   - node: исходный узел
   - updates: карта обновлений
   
   Проверяет, что обновленный узел остается валидным"
  [node updates]
  {:pre [(map? node)
         (contains? node :ast-type)
         (map? updates)]
   :post [(map? %)
          (= (:ast-type %) (:ast-type node))]}
  (let [updated (merge node updates)]
    ;; Здесь можно добавить дополнительные проверки валидности
    updated))

;; ============================================================================
;; ЭКСПОРТ КОНСТРУКТОРОВ
;; ============================================================================

(def ^:export ast-constructors
  "Карта конструкторов узлов AST для удобного доступа"
  {:program program-node
   :function-declaration function-declaration-node
   :variable-declaration variable-declaration-node
   :parameter-declaration parameter-declaration-node
   :block-statement block-statement-node
   :if-statement if-statement-node
   :while-statement while-statement-node
   :for-statement for-statement-node
   :return-statement return-statement-node
   :break-statement break-statement-node
   :continue-statement continue-statement-node
   :expression-statement expression-statement-node
   :binary-expression binary-expression-node
   :unary-expression unary-expression-node
   :assignment-expression assignment-expression-node
   :call-expression call-expression-node
   :identifier identifier-node
   :literal literal-node
   :array-access array-access-node
   :member-access member-access-node
   :interrupt-declaration interrupt-declaration-node
   :sfr-declaration sfr-declaration-node
   :sbit-declaration sbit-declaration-node
   :using-declaration using-declaration-node})

(def ^:export factory-functions
  "Карта фабричных функций для создания сложных структур"
  {:simple-function simple-function
   :simple-variable simple-variable})

(def ^:export node-utilities
  "Карта утилит для работы с узлами"
  {:clone-node clone-node
   :update-node update-node
   :make-ast-node make-ast-node}) 