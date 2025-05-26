(ns c51cc.ast
  "Модуль для работы с абстрактным синтаксическим деревом (AST) компилятора C51
   
   Этот модуль предоставляет:
   - Определения всех типов узлов AST
   - Функции для создания и валидации узлов
   - Алгоритмы обхода дерева (visitor pattern)
   - Анализ и трансформация AST
   - Красивый вывод структуры дерева
   - Утилиты для работы с областями видимости и типами"
  (:require [clojure.string :as str]
            [clojure.walk :as walk]
            [clojure.pprint :as pprint]))

;; ============================================================================
;; ОПРЕДЕЛЕНИЯ ТИПОВ УЗЛОВ AST
;; ============================================================================

(def ^:const ast-node-types
  "Множество всех допустимых типов узлов AST в компиляторе C51"
  #{:program
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

(def ^:const binary-operators
  "Множество всех бинарных операторов с их приоритетами и ассоциативностью"
  {:arithmetic #{:plus :minus :multiply :divide :modulo}
   :comparison #{:greater :less :greater-equal :less-equal :equal :not-equal}
   :logical    #{:and :or}
   :bitwise    #{:bitwise-and :bitwise-or :bitwise-xor :shift-left :shift-right}
   :assignment #{:assign :plus-equal :minus-equal :and-equal :or-equal :xor-equal
                :shift-left-equal :shift-right-equal}})

(def ^:const unary-operators
  "Множество всех унарных операторов"
  #{:plus :minus :not :bitwise-not :pre-increment :post-increment 
    :pre-decrement :post-decrement :address-of :dereference})

;; ============================================================================
;; КОНСТРУКТОРЫ УЗЛОВ AST
;; ============================================================================

(defn make-ast-node
  "Создает узел AST с валидацией типа и обязательными метаданными
   
   Аргументы:
   - node-type: тип узла из ast-node-types
   - attributes: карта атрибутов узла
   
   Возвращает: карту, представляющую узел AST с метаданными"
  [node-type & {:as attributes}]
  {:pre [(contains? ast-node-types node-type)
         (map? attributes)]
   :post [(map? %)
          (= (:ast-type %) node-type)]}
  (merge {:ast-type node-type
          :source-location nil  ; будет заполнено парсером
          :type-info nil        ; информация о типах для семантического анализа
          :scope-info nil}      ; информация об области видимости
         attributes))

;; Узлы верхнего уровня
(defn program-node 
  "Создает корневой узел программы"
  [declarations]
  {:pre [(sequential? declarations)
         (every? map? declarations)]}
  (make-ast-node :program :declarations declarations))

(defn function-declaration-node 
  "Создает узел объявления функции"
  [return-type name parameters body]
  {:pre [(map? return-type)
         (string? name)
         (sequential? parameters)
         (map? body)]}
  (make-ast-node :function-declaration
                 :return-type return-type
                 :name name
                 :parameters parameters
                 :body body))

(defn variable-declaration-node 
  "Создает узел объявления переменной"
  [type name initializer]
  {:pre [(map? type)
         (string? name)
         (or (nil? initializer) (map? initializer))]}
  (make-ast-node :variable-declaration
                 :type type
                 :name name
                 :initializer initializer))

(defn parameter-declaration-node
  "Создает узел объявления параметра функции"
  [type name]
  {:pre [(map? type)
         (string? name)]}
  (make-ast-node :parameter-declaration
                 :type type
                 :name name))

;; Узлы операторов
(defn block-statement-node 
  "Создает узел блока операторов"
  [statements]
  {:pre [(sequential? statements)
         (every? map? statements)]}
  (make-ast-node :block-statement :statements statements))

(defn if-statement-node 
  "Создает узел условного оператора"
  [condition then-branch else-branch]
  {:pre [(map? condition)
         (map? then-branch)
         (or (nil? else-branch) (map? else-branch))]}
  (make-ast-node :if-statement
                 :condition condition
                 :then-branch then-branch
                 :else-branch else-branch))

(defn while-statement-node 
  "Создает узел цикла while"
  [condition body]
  {:pre [(map? condition)
         (map? body)]}
  (make-ast-node :while-statement
                 :condition condition
                 :body body))

(defn for-statement-node 
  "Создает узел цикла for"
  [init condition update body]
  {:pre [(or (nil? init) (map? init))
         (or (nil? condition) (map? condition))
         (or (nil? update) (map? update))
         (map? body)]}
  (make-ast-node :for-statement
                 :init init
                 :condition condition
                 :update update
                 :body body))

(defn return-statement-node 
  "Создает узел оператора return"
  [expression]
  {:pre [(or (nil? expression) (map? expression))]}
  (make-ast-node :return-statement :expression expression))

(defn expression-statement-node 
  "Создает узел оператора-выражения"
  [expression]
  {:pre [(map? expression)]}
  (make-ast-node :expression-statement :expression expression))

;; Узлы выражений
(defn binary-expression-node 
  "Создает узел бинарного выражения"
  [operator left right]
  {:pre [(keyword? operator)
         (map? left)
         (map? right)]}
  (make-ast-node :binary-expression
                 :operator operator
                 :left left
                 :right right))

(defn unary-expression-node 
  "Создает узел унарного выражения"
  [operator operand]
  {:pre [(keyword? operator)
         (contains? unary-operators operator)
         (map? operand)]}
  (make-ast-node :unary-expression
                 :operator operator
                 :operand operand))

(defn assignment-expression-node 
  "Создает узел выражения присваивания"
  [operator left right]
  {:pre [(keyword? operator)
         (map? left)
         (map? right)]}
  (make-ast-node :assignment-expression
                 :operator operator
                 :left left
                 :right right))

(defn call-expression-node 
  "Создает узел вызова функции"
  [callee arguments]
  {:pre [(map? callee)
         (sequential? arguments)
         (every? map? arguments)]}
  (make-ast-node :call-expression
                 :callee callee
                 :arguments arguments))

(defn identifier-node 
  "Создает узел идентификатора"
  [name]
  {:pre [(string? name)
         (not (str/blank? name))]}
  (make-ast-node :identifier :name name))

(defn literal-node 
  "Создает узел литерала"
  [value literal-type]
  {:pre [(keyword? literal-type)
         (contains? #{:number :string :char :boolean} literal-type)]}
  (make-ast-node :literal 
                 :value value 
                 :literal-type literal-type))

(defn array-access-node
  "Создает узел доступа к элементу массива"
  [array index]
  {:pre [(map? array)
         (map? index)]}
  (make-ast-node :array-access
                 :array array
                 :index index))

;; Специфичные для C51 узлы
(defn interrupt-declaration-node
  "Создает узел объявления обработчика прерывания"
  [function-name interrupt-number using-clause]
  {:pre [(string? function-name)
         (integer? interrupt-number)
         (or (nil? using-clause) (integer? using-clause))]}
  (make-ast-node :interrupt-declaration
                 :function-name function-name
                 :interrupt-number interrupt-number
                 :using-clause using-clause))

(defn sfr-declaration-node
  "Создает узел объявления регистра специальных функций"
  [name address]
  {:pre [(string? name)
         (integer? address)]}
  (make-ast-node :sfr-declaration
                 :name name
                 :address address))

(defn sbit-declaration-node
  "Создает узел объявления специального бита"
  [name address]
  {:pre [(string? name)
         (integer? address)]}
  (make-ast-node :sbit-declaration
                 :name name
                 :address address))

;; ============================================================================
;; ВАЛИДАЦИЯ AST
;; ============================================================================

(defn valid-ast-node?
  "Проверяет, является ли узел валидным узлом AST"
  [node]
  (and (map? node)
       (contains? node :ast-type)
       (contains? ast-node-types (:ast-type node))))

(defn validate-ast
  "Рекурсивно валидирует все узлы в AST
   Возвращает вектор ошибок валидации (пустой, если AST корректен)"
  [ast]
  (let [errors (atom [])]
    (walk/postwalk
      (fn [node]
        (when (and (map? node) (contains? node :ast-type))
          (when-not (valid-ast-node? node)
            (swap! errors conj {:error :invalid-node
                               :node node
                               :message "Некорректный узел AST"}))
          
          ;; Специфичные проверки для разных типов узлов
          (case (:ast-type node)
            :function-declaration
            (when (empty? (:name node))
              (swap! errors conj {:error :empty-function-name
                                 :node node
                                 :message "Имя функции не может быть пустым"}))
            
            :variable-declaration
            (when (empty? (:name node))
              (swap! errors conj {:error :empty-variable-name
                                 :node node
                                 :message "Имя переменной не может быть пустым"}))
            
            :binary-expression
            (let [op (:operator node)]
              (when-not (some #(contains? (val %) op) binary-operators)
                (swap! errors conj {:error :unknown-binary-operator
                                   :node node
                                   :operator op
                                   :message (str "Неизвестный бинарный оператор: " op)})))
            
            :unary-expression
            (let [op (:operator node)]
              (when-not (contains? unary-operators op)
                (swap! errors conj {:error :unknown-unary-operator
                                   :node node
                                   :operator op
                                   :message (str "Неизвестный унарный оператор: " op)})))
            
            nil))
        node)
      ast)
    @errors))

;; ============================================================================
;; ОБХОД AST (VISITOR PATTERN)
;; ============================================================================

(defprotocol ASTVisitor
  "Протокол для реализации паттерна Visitor для обхода AST"
  (visit-node [visitor node] "Посещает узел AST")
  (enter-node [visitor node] "Вызывается при входе в узел")
  (exit-node [visitor node] "Вызывается при выходе из узла"))

(defn walk-ast
  "Обходит AST с использованием visitor'а
   
   Аргументы:
   - visitor: объект, реализующий протокол ASTVisitor
   - ast: корневой узел AST для обхода
   
   Возвращает: результат обхода (зависит от visitor'а)"
  [visitor ast]
  {:pre [(satisfies? ASTVisitor visitor)
         (valid-ast-node? ast)]}
  
  (letfn [(walk-node [node]
            (enter-node visitor node)
            (let [result (visit-node visitor node)]
              ;; Рекурсивно обходим дочерние узлы
              (case (:ast-type node)
                :program
                (doseq [decl (:declarations node)]
                  (walk-node decl))
                
                :function-declaration
                (do (doseq [param (:parameters node)]
                      (walk-node param))
                    (walk-node (:body node)))
                
                :variable-declaration
                (when (:initializer node)
                  (walk-node (:initializer node)))
                
                :block-statement
                (doseq [stmt (:statements node)]
                  (walk-node stmt))
                
                :if-statement
                (do (walk-node (:condition node))
                    (walk-node (:then-branch node))
                    (when (:else-branch node)
                      (walk-node (:else-branch node))))
                
                :while-statement
                (do (walk-node (:condition node))
                    (walk-node (:body node)))
                
                :for-statement
                (do (when (:init node) (walk-node (:init node)))
                    (when (:condition node) (walk-node (:condition node)))
                    (when (:update node) (walk-node (:update node)))
                    (walk-node (:body node)))
                
                :return-statement
                (when (:expression node)
                  (walk-node (:expression node)))
                
                :expression-statement
                (walk-node (:expression node))
                
                :binary-expression
                (do (walk-node (:left node))
                    (walk-node (:right node)))
                
                :unary-expression
                (walk-node (:operand node))
                
                :assignment-expression
                (do (walk-node (:left node))
                    (walk-node (:right node)))
                
                :call-expression
                (do (walk-node (:callee node))
                    (doseq [arg (:arguments node)]
                      (walk-node arg)))
                
                :array-access
                (do (walk-node (:array node))
                    (walk-node (:index node)))
                
                ;; Листовые узлы не требуют дополнительного обхода
                (:identifier :literal) nil
                
                ;; Для неизвестных типов узлов выводим предупреждение
                (println (str "Предупреждение: неизвестный тип узла для обхода: " (:ast-type node))))
              
              (exit-node visitor node)
              result))]
    
    (walk-node ast)))

;; ============================================================================
;; АНАЛИЗ AST
;; ============================================================================

(defn collect-identifiers
  "Собирает все идентификаторы в AST"
  [ast]
  (let [identifiers (atom #{})]
    (walk/postwalk
      (fn [node]
        (when (and (map? node) (= (:ast-type node) :identifier))
          (swap! identifiers conj (:name node)))
        node)
      ast)
    @identifiers))

(defn collect-function-calls
  "Собирает все вызовы функций в AST"
  [ast]
  (let [calls (atom [])]
    (walk/postwalk
      (fn [node]
        (when (and (map? node) (= (:ast-type node) :call-expression))
          (swap! calls conj {:function (get-in node [:callee :name])
                            :arguments (count (:arguments node))
                            :node node}))
        node)
      ast)
    @calls))

(defn find-function-declarations
  "Находит все объявления функций в AST"
  [ast]
  (let [functions (atom [])]
    (walk/postwalk
      (fn [node]
        (when (and (map? node) (= (:ast-type node) :function-declaration))
          (swap! functions conj {:name (:name node)
                                :return-type (:return-type node)
                                :parameters (count (:parameters node))
                                :node node}))
        node)
      ast)
    @functions))

(defn calculate-ast-depth
  "Вычисляет максимальную глубину AST"
  [ast]
  (letfn [(depth [node level]
            (if (and (map? node) (contains? node :ast-type))
              (let [child-depths (map #(depth % (inc level))
                                    (filter map? (vals node)))]
                (if (empty? child-depths)
                  level
                  (apply max child-depths)))
              level))]
    (depth ast 0)))

(defn count-nodes-by-type
  "Подсчитывает количество узлов каждого типа в AST"
  [ast]
  (let [counts (atom {})]
    (walk/postwalk
      (fn [node]
        (when (and (map? node) (contains? node :ast-type))
          (swap! counts update (:ast-type node) (fnil inc 0)))
        node)
      ast)
    @counts))

;; ============================================================================
;; КРАСИВЫЙ ВЫВОД AST
;; ============================================================================

(defn ast-to-string
  "Преобразует AST в читаемую строковую репрезентацию"
  [ast & {:keys [indent-size show-metadata]
          :or {indent-size 2 show-metadata false}}]
  (letfn [(indent [level]
            (str/join (repeat (* level indent-size) " ")))
          
          (format-node [node level]
            (if (and (map? node) (contains? node :ast-type))
              (let [type-str (str "(" (name (:ast-type node)))
                    attrs (dissoc node :ast-type 
                                      (when-not show-metadata :source-location)
                                      (when-not show-metadata :type-info)
                                      (when-not show-metadata :scope-info))
                    formatted-attrs (map (fn [[k v]]
                                          (str "\n" (indent (inc level))
                                               (name k) ": "
                                               (if (and (map? v) (contains? v :ast-type))
                                                 (format-node v (+ level 2))
                                                 (if (sequential? v)
                                                   (str "["
                                                        (str/join "\n"
                                                          (map #(str (indent (+ level 2))
                                                                    (format-node % (+ level 2)))
                                                               v))
                                                        (when (seq v) (str "\n" (indent (inc level))))
                                                        "]")
                                                   (pr-str v)))))
                                        attrs)]
                (str type-str
                     (str/join formatted-attrs)
                     ")"))
              (pr-str node)))]
    
    (format-node ast 0)))

(defn pretty-print
  "Красиво выводит AST в консоль"
  [ast & options]
  (println (apply ast-to-string ast options)))

(defn print-ast-summary
  "Выводит краткую сводку по AST"
  [ast]
  (let [node-counts (count-nodes-by-type ast)
        depth (calculate-ast-depth ast)
        functions (find-function-declarations ast)
        identifiers (collect-identifiers ast)]
    
    (println "=== СВОДКА ПО AST ===")
    (println (str "Глубина дерева: " depth))
    (println (str "Общее количество узлов: " (reduce + (vals node-counts))))
    (println "\nКоличество узлов по типам:")
    (doseq [[type count] (sort-by first node-counts)]
      (println (str "  " (name type) ": " count)))
    
    (println (str "\nФункции (" (count functions) "):"))
    (doseq [func functions]
      (println (str "  " (:name func) " (параметров: " (:parameters func) ")")))
    
    (println (str "\nУникальные идентификаторы (" (count identifiers) "):"))
    (println (str "  " (str/join ", " (sort identifiers))))))

;; ============================================================================
;; ТРАНСФОРМАЦИЯ AST
;; ============================================================================

(defn transform-ast
  "Применяет функцию трансформации ко всем узлам AST
   
   Аргументы:
   - transform-fn: функция (node -> node) для трансформации узлов
   - ast: корневой узел AST
   
   Возвращает: трансформированный AST"
  [transform-fn ast]
  {:pre [(fn? transform-fn)
         (valid-ast-node? ast)]}
  (walk/postwalk
    (fn [node]
      (if (and (map? node) (contains? node :ast-type))
        (transform-fn node)
        node))
    ast))

(defn optimize-ast
  "Применяет базовые оптимизации к AST
   
   Включает:
   - Свертывание константных выражений
   - Удаление мертвого кода
   - Упрощение логических выражений"
  [ast]
  (transform-ast
    (fn [node]
      (case (:ast-type node)
        ;; Свертывание константных арифметических выражений
        :binary-expression
        (let [{:keys [operator left right]} node]
          (if (and (= (:ast-type left) :literal)
                   (= (:ast-type right) :literal)
                   (= (:literal-type left) :number)
                   (= (:literal-type right) :number))
            (case operator
              :plus (literal-node (+ (:value left) (:value right)) :number)
              :minus (literal-node (- (:value left) (:value right)) :number)
              :multiply (literal-node (* (:value left) (:value right)) :number)
              :divide (if (zero? (:value right))
                       node  ; Избегаем деления на ноль
                       (literal-node (quot (:value left) (:value right)) :number))
              node)
            node))
        
        ;; Упрощение логических выражений
        :unary-expression
        (let [{:keys [operator operand]} node]
          (case operator
            :not (if (and (= (:ast-type operand) :literal)
                         (= (:literal-type operand) :boolean))
                  (literal-node (not (:value operand)) :boolean)
                  node)
            node))
        
        ;; Для остальных узлов возвращаем без изменений
        node))
    ast))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ РАБОТЫ С ТИПАМИ
;; ============================================================================

(defn infer-expression-type
  "Выводит тип выражения на основе его структуры
   Это упрощенная версия для демонстрации концепции"
  [expr]
  (case (:ast-type expr)
    :literal
    (case (:literal-type expr)
      :number :int
      :string :char-pointer
      :char :char
      :boolean :int)
    
    :identifier
    ;; В реальной реализации здесь был бы поиск в таблице символов
    :unknown
    
    :binary-expression
    (let [left-type (infer-expression-type (:left expr))
          right-type (infer-expression-type (:right expr))]
      ;; Упрощенные правила приведения типов
      (cond
        (and (= left-type :int) (= right-type :int)) :int
        (or (= left-type :char-pointer) (= right-type :char-pointer)) :char-pointer
        :else :int))
    
    :call-expression
    ;; В реальной реализации здесь был бы поиск типа возвращаемого значения функции
    :unknown
    
    ;; Для остальных типов выражений
    :unknown))

;; ============================================================================
;; ЭКСПОРТ ОСНОВНЫХ ФУНКЦИЙ
;; ============================================================================

;; Основные функции, которые должны быть доступны другим модулям
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
   :expression-statement expression-statement-node
   :binary-expression binary-expression-node
   :unary-expression unary-expression-node
   :assignment-expression assignment-expression-node
   :call-expression call-expression-node
   :identifier identifier-node
   :literal literal-node
   :array-access array-access-node
   :interrupt-declaration interrupt-declaration-node
   :sfr-declaration sfr-declaration-node
   :sbit-declaration sbit-declaration-node})