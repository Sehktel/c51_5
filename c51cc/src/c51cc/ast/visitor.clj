(ns c51cc.ast.visitor
  "Паттерн Visitor для обхода AST с использованием мультиметодов
   
   Этот модуль предоставляет:
   - Расширяемую систему обхода узлов через мультиметоды
   - Различные стратегии обхода (pre-order, post-order, breadth-first)
   - Протокол ASTVisitor для реализации пользовательских visitor'ов
   - Готовые visitor'ы для типичных задач
   
   Преимущества мультиметодов над case:
   - Расширяемость без изменения основного кода
   - Возможность добавления новых типов узлов через плагины
   - Лучшая производительность диспетчеризации
   - Более чистая архитектура"
  (:require [c51cc.ast.types :as types]
            [clojure.core.match :refer [match]]
            [clojure.walk :as walk]))

;; ============================================================================
;; ПРОТОКОЛ VISITOR
;; ============================================================================

(defprotocol ASTVisitor
  "Протокол для реализации паттерна Visitor для обхода AST
   
   Методы:
   - visit-node: основная логика обработки узла
   - enter-node: вызывается при входе в узел (pre-order)
   - exit-node: вызывается при выходе из узла (post-order)
   - should-visit-children?: определяет, нужно ли обходить дочерние узлы"
  (visit-node [visitor node] 
    "Посещает узел AST и выполняет основную логику обработки")
  (enter-node [visitor node] 
    "Вызывается при входе в узел (до обхода дочерних узлов)")
  (exit-node [visitor node] 
    "Вызывается при выходе из узла (после обхода дочерних узлов)")
  (should-visit-children? [visitor node] 
    "Определяет, нужно ли обходить дочерние узлы данного узла"))

;; ============================================================================
;; МУЛЬТИМЕТОДЫ ДЛЯ ОБХОДА ДОЧЕРНИХ УЗЛОВ
;; ============================================================================

(defmulti walk-node-children
  "Обходит дочерние узлы в зависимости от типа узла
   
   Диспетчеризация по :ast-type узла.
   Каждый метод должен вызывать walk-node для всех дочерних узлов."
  (fn [node visitor walk-fn] (:ast-type node)))

;; Корневые узлы
(defmethod walk-node-children :program
  [node visitor walk-fn]
  (doseq [decl (:declarations node)]
    (walk-fn decl visitor)))

;; Объявления
(defmethod walk-node-children :function-declaration
  [node visitor walk-fn]
  (walk-fn (:return-type node) visitor)
  (doseq [param (:parameters node)]
    (walk-fn param visitor))
  (walk-fn (:body node) visitor))

(defmethod walk-node-children :variable-declaration
  [node visitor walk-fn]
  (walk-fn (:type node) visitor)
  (when (:initializer node)
    (walk-fn (:initializer node) visitor)))

(defmethod walk-node-children :parameter-declaration
  [node visitor walk-fn]
  (walk-fn (:type node) visitor))

;; Операторы
(defmethod walk-node-children :block-statement
  [node visitor walk-fn]
  (doseq [stmt (:statements node)]
    (walk-fn stmt visitor)))

(defmethod walk-node-children :if-statement
  [node visitor walk-fn]
  (walk-fn (:condition node) visitor)
  (walk-fn (:then-branch node) visitor)
  (when (:else-branch node)
    (walk-fn (:else-branch node) visitor)))

(defmethod walk-node-children :while-statement
  [node visitor walk-fn]
  (walk-fn (:condition node) visitor)
  (walk-fn (:body node) visitor))

(defmethod walk-node-children :for-statement
  [node visitor walk-fn]
  (when (:init node) 
    (walk-fn (:init node) visitor))
  (when (:condition node) 
    (walk-fn (:condition node) visitor))
  (when (:update node) 
    (walk-fn (:update node) visitor))
  (walk-fn (:body node) visitor))

(defmethod walk-node-children :return-statement
  [node visitor walk-fn]
  (when (:expression node)
    (walk-fn (:expression node) visitor)))

(defmethod walk-node-children :expression-statement
  [node visitor walk-fn]
  (walk-fn (:expression node) visitor))

;; Операторы без дочерних узлов
(defmethod walk-node-children :break-statement
  [node visitor walk-fn]
  ;; Нет дочерних узлов
  nil)

(defmethod walk-node-children :continue-statement
  [node visitor walk-fn]
  ;; Нет дочерних узлов
  nil)

;; Выражения
(defmethod walk-node-children :binary-expression
  [node visitor walk-fn]
  (walk-fn (:left node) visitor)
  (walk-fn (:right node) visitor))

(defmethod walk-node-children :unary-expression
  [node visitor walk-fn]
  (walk-fn (:operand node) visitor))

(defmethod walk-node-children :assignment-expression
  [node visitor walk-fn]
  (walk-fn (:left node) visitor)
  (walk-fn (:right node) visitor))

(defmethod walk-node-children :call-expression
  [node visitor walk-fn]
  (walk-fn (:callee node) visitor)
  (doseq [arg (:arguments node)]
    (walk-fn arg visitor)))

(defmethod walk-node-children :array-access
  [node visitor walk-fn]
  (walk-fn (:array node) visitor)
  (walk-fn (:index node) visitor))

(defmethod walk-node-children :member-access
  [node visitor walk-fn]
  (walk-fn (:object node) visitor))

;; Листовые узлы (без дочерних узлов)
(defmethod walk-node-children :identifier
  [node visitor walk-fn]
  ;; Нет дочерних узлов
  nil)

(defmethod walk-node-children :literal
  [node visitor walk-fn]
  ;; Нет дочерних узлов
  nil)

;; Специфичные для C51 узлы
(defmethod walk-node-children :interrupt-declaration
  [node visitor walk-fn]
  ;; Нет дочерних узлов (только метаданные)
  nil)

(defmethod walk-node-children :sfr-declaration
  [node visitor walk-fn]
  ;; Нет дочерних узлов
  nil)

(defmethod walk-node-children :sbit-declaration
  [node visitor walk-fn]
  ;; Нет дочерних узлов
  nil)

(defmethod walk-node-children :using-declaration
  [node visitor walk-fn]
  ;; Нет дочерних узлов
  nil)

;; Вспомогательные узлы
(defmethod walk-node-children :type-specifier
  [node visitor walk-fn]
  ;; Нет дочерних узлов (только метаданные типа)
  nil)

;; Обработка неизвестных типов узлов
(defmethod walk-node-children :default
  [node visitor walk-fn]
  (println (str "ПРЕДУПРЕЖДЕНИЕ: Неизвестный тип узла для обхода: " (:ast-type node)))
  ;; Пытаемся обойти все значения, которые выглядят как узлы AST
  (doseq [value (vals node)]
    (cond
      ;; Одиночный узел
      (and (map? value) (contains? value :ast-type))
      (walk-fn value visitor)
      
      ;; Коллекция узлов
      (and (sequential? value) 
           (every? #(and (map? %) (contains? % :ast-type)) value))
      (doseq [child value]
        (walk-fn child visitor)))))

;; ============================================================================
;; СТРАТЕГИИ ОБХОДА
;; ============================================================================

(defn walk-ast-preorder
  "Обходит AST в порядке pre-order (сначала узел, потом дочерние)
   
   Аргументы:
   - visitor: объект, реализующий протокол ASTVisitor
   - ast: корневой узел AST для обхода
   
   Возвращает: результат обхода (зависит от visitor'а)"
  [visitor ast]
  {:pre [(satisfies? ASTVisitor visitor)
         (and (map? ast) (contains? ast :ast-type))]}
  
  (letfn [(walk-node [node]
            (enter-node visitor node)
            (let [result (visit-node visitor node)]
              (when (should-visit-children? visitor node)
                (walk-node-children node visitor walk-node))
              (exit-node visitor node)
              result))]
    
    (walk-node ast)))

(defn walk-ast-postorder
  "Обходит AST в порядке post-order (сначала дочерние, потом узел)
   
   Полезно для операций, которые зависят от результатов обработки дочерних узлов"
  [visitor ast]
  {:pre [(satisfies? ASTVisitor visitor)
         (and (map? ast) (contains? ast :ast-type))]}
  
  (letfn [(walk-node [node]
            (enter-node visitor node)
            (when (should-visit-children? visitor node)
              (walk-node-children node visitor walk-node))
            (let [result (visit-node visitor node)]
              (exit-node visitor node)
              result))]
    
    (walk-node ast)))

(defn walk-ast-breadth-first
  "Обходит AST в ширину (breadth-first)
   
   Полезно для анализа по уровням дерева"
  [visitor ast]
  {:pre [(satisfies? ASTVisitor visitor)
         (and (map? ast) (contains? ast :ast-type))]}
  
  (let [queue (atom [ast])
        results (atom [])]
    
    (while (seq @queue)
      (let [current (first @queue)]
        (swap! queue rest)
        (enter-node visitor current)
        (let [result (visit-node visitor current)]
          (swap! results conj result))
        
        (when (should-visit-children? visitor current)
          ;; Собираем всех дочерних узлов
          (let [children (atom [])]
            (walk-node-children current visitor 
                               (fn [child _] (swap! children conj child)))
            (swap! queue concat @children)))
        
        (exit-node visitor current)))
    
    @results))

;; Основная функция обхода (по умолчанию pre-order)
(defn walk-ast
  "Обходит AST с использованием visitor'а
   
   Аргументы:
   - visitor: объект, реализующий протокол ASTVisitor
   - ast: корневой узел AST для обхода
   - strategy: стратегия обхода (:preorder, :postorder, :breadth-first)
   
   Возвращает: результат обхода (зависит от visitor'а)"
  [visitor ast & {:keys [strategy] :or {strategy :preorder}}]
  {:pre [(satisfies? ASTVisitor visitor)
         (and (map? ast) (contains? ast :ast-type))
         (contains? #{:preorder :postorder :breadth-first} strategy)]}
  
  (case strategy
    :preorder (walk-ast-preorder visitor ast)
    :postorder (walk-ast-postorder visitor ast)
    :breadth-first (walk-ast-breadth-first visitor ast)))

;; ============================================================================
;; БАЗОВЫЕ РЕАЛИЗАЦИИ VISITOR
;; ============================================================================

(defrecord NoOpVisitor []
  ASTVisitor
  (visit-node [_ node] node)
  (enter-node [_ node] nil)
  (exit-node [_ node] nil)
  (should-visit-children? [_ node] true))

(defrecord CollectingVisitor [collected-nodes]
  ASTVisitor
  (visit-node [this node] 
    (swap! collected-nodes conj node)
    node)
  (enter-node [_ node] nil)
  (exit-node [_ node] nil)
  (should-visit-children? [_ node] true))

(defrecord CountingVisitor [node-counts]
  ASTVisitor
  (visit-node [this node]
    (swap! node-counts update (:ast-type node) (fnil inc 0))
    node)
  (enter-node [_ node] nil)
  (exit-node [_ node] nil)
  (should-visit-children? [_ node] true))

(defrecord FilteringVisitor [predicate collected-nodes]
  ASTVisitor
  (visit-node [this node]
    (when (predicate node)
      (swap! collected-nodes conj node))
    node)
  (enter-node [_ node] nil)
  (exit-node [_ node] nil)
  (should-visit-children? [_ node] true))

(defrecord TransformingVisitor [transform-fn]
  ASTVisitor
  (visit-node [this node]
    (transform-fn node))
  (enter-node [_ node] nil)
  (exit-node [_ node] nil)
  (should-visit-children? [_ node] true))

;; ============================================================================
;; ФАБРИЧНЫЕ ФУНКЦИИ ДЛЯ VISITOR'ОВ
;; ============================================================================

(defn make-collecting-visitor
  "Создает visitor, который собирает все посещенные узлы"
  []
  (->CollectingVisitor (atom [])))

(defn make-counting-visitor
  "Создает visitor, который подсчитывает узлы по типам"
  []
  (->CountingVisitor (atom {})))

(defn make-filtering-visitor
  "Создает visitor, который собирает узлы, удовлетворяющие предикату"
  [predicate]
  (->FilteringVisitor predicate (atom [])))

(defn make-transforming-visitor
  "Создает visitor, который трансформирует узлы с помощью функции"
  [transform-fn]
  (->TransformingVisitor transform-fn))

(defn make-debug-visitor
  "Создает visitor для отладки, который выводит информацию о каждом узле"
  [& {:keys [show-enter show-exit show-details] 
      :or {show-enter true show-exit false show-details false}}]
  (reify ASTVisitor
    (visit-node [_ node]
      (when show-details
        (println (str "Посещение узла: " (:ast-type node) 
                     " (ID: " (:node-id node) ")")))
      node)
    (enter-node [_ node]
      (when show-enter
        (println (str "Вход в узел: " (:ast-type node))))
      nil)
    (exit-node [_ node]
      (when show-exit
        (println (str "Выход из узла: " (:ast-type node))))
      nil)
    (should-visit-children? [_ node] true)))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ РАБОТЫ С VISITOR'АМИ
;; ============================================================================

(defn collect-nodes
  "Собирает все узлы AST в плоский список"
  [ast & {:keys [strategy] :or {strategy :preorder}}]
  (let [visitor (make-collecting-visitor)]
    (walk-ast visitor ast :strategy strategy)
    @(:collected-nodes visitor)))

(defn count-nodes-by-type
  "Подсчитывает количество узлов каждого типа в AST"
  [ast]
  (let [visitor (make-counting-visitor)]
    (walk-ast visitor ast)
    @(:node-counts visitor)))

(defn find-nodes
  "Находит все узлы, удовлетворяющие предикату"
  [ast predicate]
  (let [visitor (make-filtering-visitor predicate)]
    (walk-ast visitor ast)
    @(:collected-nodes visitor)))

(defn find-nodes-by-type
  "Находит все узлы заданного типа"
  [ast node-type]
  (find-nodes ast #(= (:ast-type %) node-type)))

(defn transform-ast
  "Трансформирует AST, применяя функцию к каждому узлу"
  [ast transform-fn & {:keys [strategy] :or {strategy :postorder}}]
  (let [visitor (make-transforming-visitor transform-fn)]
    (walk-ast visitor ast :strategy strategy)))

(defn calculate-ast-depth
  "Вычисляет максимальную глубину AST"
  [ast]
  (let [max-depth (atom 0)
        current-depth (atom 0)]
    (walk-ast 
      (reify ASTVisitor
        (visit-node [_ node] node)
        (enter-node [_ node]
          (swap! current-depth inc)
          (swap! max-depth max @current-depth))
        (exit-node [_ node]
          (swap! current-depth dec))
        (should-visit-children? [_ node] true))
      ast)
    @max-depth))

;; ============================================================================
;; СПЕЦИАЛИЗИРОВАННЫЕ VISITOR'Ы ДЛЯ C51
;; ============================================================================

(defn collect-function-declarations
  "Собирает все объявления функций в AST"
  [ast]
  (find-nodes-by-type ast :function-declaration))

(defn collect-variable-declarations
  "Собирает все объявления переменных в AST"
  [ast]
  (find-nodes-by-type ast :variable-declaration))

(defn collect-identifiers
  "Собирает все идентификаторы в AST"
  [ast]
  (map :name (find-nodes-by-type ast :identifier)))

(defn collect-function-calls
  "Собирает все вызовы функций в AST"
  [ast]
  (let [calls (find-nodes-by-type ast :call-expression)]
    (map (fn [call]
           {:function (get-in call [:callee :name])
            :arguments (count (:arguments call))
            :node call})
         calls)))

(defn collect-c51-specific-nodes
  "Собирает все узлы, специфичные для C51"
  [ast]
  (find-nodes ast #(types/c51-specific-type? (:ast-type %))))

;; ============================================================================
;; ЭКСПОРТ
;; ============================================================================

(def ^:export visitor-functions
  "Экспорт основных функций visitor pattern"
  {:walk-ast walk-ast
   :walk-preorder walk-ast-preorder
   :walk-postorder walk-ast-postorder
   :walk-breadth-first walk-ast-breadth-first
   :collect-nodes collect-nodes
   :count-nodes count-nodes-by-type
   :find-nodes find-nodes
   :find-by-type find-nodes-by-type
   :transform-ast transform-ast
   :calculate-depth calculate-ast-depth})

(def ^:export visitor-factories
  "Экспорт фабричных функций для создания visitor'ов"
  {:collecting make-collecting-visitor
   :counting make-counting-visitor
   :filtering make-filtering-visitor
   :transforming make-transforming-visitor
   :debug make-debug-visitor})

(def ^:export c51-visitors
  "Экспорт специализированных visitor'ов для C51"
  {:functions collect-function-declarations
   :variables collect-variable-declarations
   :identifiers collect-identifiers
   :function-calls collect-function-calls
   :c51-specific collect-c51-specific-nodes}) 