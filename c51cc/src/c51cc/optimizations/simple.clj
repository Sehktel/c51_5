(ns c51cc.optimizations.simple
  "Простые оптимизации для компилятора C51
   
   Реализует базовые оптимизации:
   - Constant folding (свертывание констант)
   - Dead code elimination (удаление мертвого кода)
   - Strength reduction (упрощение операций)
   - Copy propagation (распространение копий)
   
   Оптимизации работают на уровне HIR и фокусируются на:
   - Уменьшении размера кода (приоритет для AT89S4051)
   - Простоте реализации и отладки
   - Безопасности трансформаций"
  (:require [c51cc.ir.hir :as hir]))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ ОПТИМИЗАЦИЙ
;; ============================================================================

(defn constant-node? [node]
  "Проверяет, является ли узел константой"
  (and (hir/hir-node? node) (= (:op node) :constant)))

(defn get-constant-value [node]
  "Извлекает значение константы из узла"
  (when (constant-node? node)
    (hir/first-operand node)))

(defn variable-node? [node]
  "Проверяет, является ли узел переменной"
  (and (hir/hir-node? node) (= (:op node) :variable)))

(defn get-variable-name [node]
  "Извлекает имя переменной из узла"
  (when (variable-node? node)
    (hir/first-operand node)))

(defn safe-arithmetic-op [op left right]
  "Безопасно выполняет арифметическую операцию с проверкой переполнения"
  (try
    (case op
      :add (+ left right)
      :sub (- left right)
      :mul (* left right)
      :div (when (not= right 0) (quot left right))
      :mod (when (not= right 0) (rem left right))
      nil)
    (catch Exception _ nil)))

(defn safe-bitwise-op [op left right]
  "Безопасно выполняет битовую операцию"
  (try
    (case op
      :and (bit-and left right)
      :or  (bit-or left right)
      :xor (bit-xor left right)
      :shl (bit-shift-left left right)
      :shr (bit-shift-right left right)
      nil)
    (catch Exception _ nil)))

;; ============================================================================
;; CONSTANT FOLDING (Свертывание констант)
;; ============================================================================

(defn fold-binary-constants
  "Свертывает бинарную операцию с двумя константами"
  [node]
  (when (and (hir/binary-op? node)
             (constant-node? (hir/first-operand node))
             (constant-node? (hir/second-operand node)))
    (let [op (:op node)
          left (get-constant-value (hir/first-operand node))
          right (get-constant-value (hir/second-operand node))
          result-type (:type node)]
      
      ;; Пытаемся вычислить результат
      (when-let [result (or (safe-arithmetic-op op left right)
                           (safe-bitwise-op op left right))]
        ;; Создаем новый константный узел
        (hir/hir-constant result result-type (:metadata node))))))

(defn fold-unary-constants
  "Свертывает унарную операцию с константой"
  [node]
  (when (and (hir/unary-op? node)
             (constant-node? (hir/first-operand node)))
    (let [op (:op node)
          operand (get-constant-value (hir/first-operand node))
          result-type (:type node)]
      
      (when-let [result (case op
                         :neg (- operand)
                         :abs (Math/abs operand)
                         :not (bit-not operand)
                         :logical-not (if (zero? operand) 1 0)
                         nil)]
        (hir/hir-constant result result-type (:metadata node))))))

(defn constant-fold-node
  "Применяет constant folding к одному узлу"
  [node]
  (if (hir/hir-node? node)
    (or (fold-binary-constants node)
        (fold-unary-constants node)
        node)
    node))

(defn constant-fold
  "Применяет constant folding ко всему HIR дереву"
  [hir-tree]
  (hir/walk-hir constant-fold-node hir-tree))

;; ============================================================================
;; STRENGTH REDUCTION (Упрощение операций)
;; ============================================================================

(defn power-of-two? [n]
  "Проверяет, является ли число степенью двойки"
  (and (pos? n) (zero? (bit-and n (dec n)))))

(defn log2 [n]
  "Вычисляет log2 для степени двойки"
  (loop [n n, result 0]
    (if (= n 1)
      result
      (recur (bit-shift-right n 1) (inc result)))))

(defn strength-reduce-multiplication
  "Упрощает умножение на степень двойки в сдвиг"
  [node]
  (when (and (= (:op node) :mul)
             (constant-node? (hir/second-operand node)))
    (let [multiplier (get-constant-value (hir/second-operand node))]
      (cond
        ;; x * 0 → 0
        (= multiplier 0)
        (hir/hir-constant 0 (:type node) (:metadata node))
        
        ;; x * 1 → x
        (= multiplier 1)
        (hir/first-operand node)
        
        ;; x * 2^n → x << n
        (power-of-two? multiplier)
        (hir/hir-shl (hir/first-operand node)
                     (hir/hir-constant (log2 multiplier) :uint8)
                     (:type node)
                     (:metadata node))
        
        :else node))))

(defn strength-reduce-division
  "Упрощает деление на степень двойки в сдвиг"
  [node]
  (when (and (= (:op node) :div)
             (constant-node? (hir/second-operand node)))
    (let [divisor (get-constant-value (hir/second-operand node))]
      (cond
        ;; x / 1 → x
        (= divisor 1)
        (hir/first-operand node)
        
        ;; x / 2^n → x >> n (только для беззнаковых типов)
        (and (power-of-two? divisor)
             (#{:uint8 :uint16} (:type node)))
        (hir/hir-shr (hir/first-operand node)
                     (hir/hir-constant (log2 divisor) :uint8)
                     (:type node)
                     (:metadata node))
        
        :else node))))

(defn strength-reduce-addition
  "Упрощает сложение с нулем"
  [node]
  (when (= (:op node) :add)
    (let [left (hir/first-operand node)
          right (hir/second-operand node)]
      (cond
        ;; x + 0 → x
        (and (constant-node? right) (= (get-constant-value right) 0))
        left
        
        ;; 0 + x → x
        (and (constant-node? left) (= (get-constant-value left) 0))
        right
        
        :else node))))

(defn strength-reduce-subtraction
  "Упрощает вычитание нуля"
  [node]
  (when (and (= (:op node) :sub)
             (constant-node? (hir/second-operand node))
             (= (get-constant-value (hir/second-operand node)) 0))
    ;; x - 0 → x
    (hir/first-operand node)))

(defn strength-reduce-bitwise
  "Упрощает битовые операции"
  [node]
  (case (:op node)
    :and
    (let [left (hir/first-operand node)
          right (hir/second-operand node)]
      (cond
        ;; x & 0 → 0
        (and (constant-node? right) (= (get-constant-value right) 0))
        (hir/hir-constant 0 (:type node) (:metadata node))
        
        ;; x & 0xFF → x (для uint8)
        (and (constant-node? right) 
             (= (get-constant-value right) 0xFF)
             (= (:type node) :uint8))
        left
        
        ;; x & 0xFFFF → x (для uint16)
        (and (constant-node? right) 
             (= (get-constant-value right) 0xFFFF)
             (= (:type node) :uint16))
        left
        
        :else node))
    
    :or
    (let [left (hir/first-operand node)
          right (hir/second-operand node)]
      (cond
        ;; x | 0 → x
        (and (constant-node? right) (= (get-constant-value right) 0))
        left
        
        ;; 0 | x → x
        (and (constant-node? left) (= (get-constant-value left) 0))
        right
        
        :else node))
    
    :xor
    (let [right (hir/second-operand node)]
      (cond
        ;; x ^ 0 → x
        (and (constant-node? right) (= (get-constant-value right) 0))
        (hir/first-operand node)
        
        :else node))
    
    node))

(defn strength-reduce-node
  "Применяет strength reduction к одному узлу"
  [node]
  (if (hir/hir-node? node)
    (or (strength-reduce-multiplication node)
        (strength-reduce-division node)
        (strength-reduce-addition node)
        (strength-reduce-subtraction node)
        (strength-reduce-bitwise node)
        node)
    node))

(defn strength-reduce
  "Применяет strength reduction ко всему HIR дереву"
  [hir-tree]
  (hir/walk-hir strength-reduce-node hir-tree))

;; ============================================================================
;; DEAD CODE ELIMINATION (Удаление мертвого кода)
;; ============================================================================

(defn collect-used-variables
  "Собирает все используемые переменные в HIR дереве"
  [hir-tree]
  (let [used-vars (atom #{})]
    (hir/walk-hir 
      (fn [node]
        (when (variable-node? node)
          (swap! used-vars conj (get-variable-name node)))
        node)
      hir-tree)
    @used-vars))

(defn assignment-to-unused-var?
  "Проверяет, является ли узел присваиванием в неиспользуемую переменную"
  [node used-vars]
  (and (= (:op node) :assign)
       (variable-node? (hir/first-operand node))
       (not (contains? used-vars (get-variable-name (hir/first-operand node))))))

(defn has-side-effects?
  "Проверяет, имеет ли узел побочные эффекты"
  [node]
  ;; Консервативный подход: считаем, что вызовы функций и операции с памятью имеют побочные эффекты
  (#{:call :store :sfr-access :sbit-access} (:op node)))

(defn dead-code-elimination-node
  "Удаляет мертвый код из узла"
  [node used-vars]
  (if (and (hir/hir-node? node)
           (not (has-side-effects? node))
           (assignment-to-unused-var? node used-vars))
    ;; Заменяем мертвое присваивание на no-op (пустой блок)
    (hir/hir-block [] (:metadata node))
    node))

(defn dead-code-elimination
  "Удаляет мертвый код из HIR дерева"
  [hir-tree]
  (let [used-vars (collect-used-variables hir-tree)]
    (hir/walk-hir #(dead-code-elimination-node % used-vars) hir-tree)))

;; ============================================================================
;; COPY PROPAGATION (Распространение копий)
;; ============================================================================

(defn simple-assignment? [node]
  "Проверяет, является ли узел простым присваиванием переменной"
  (and (= (:op node) :assign)
       (variable-node? (hir/first-operand node))
       (variable-node? (hir/second-operand node))))

(defn collect-copy-assignments
  "Собирает простые присваивания вида x = y"
  [hir-tree]
  (let [copies (atom {})]
    (hir/walk-hir
      (fn [node]
        (when (simple-assignment? node)
          (let [dest (get-variable-name (hir/first-operand node))
                src (get-variable-name (hir/second-operand node))]
            (swap! copies assoc dest src)))
        node)
      hir-tree)
    @copies))

(defn substitute-variable
  "Заменяет переменную её копией, если возможно"
  [node copy-map]
  (if (variable-node? node)
    (let [var-name (get-variable-name node)]
      (if-let [replacement (get copy-map var-name)]
        (hir/hir-variable replacement (:type node) (:metadata node))
        node))
    node))

(defn copy-propagation
  "Применяет copy propagation к HIR дереву"
  [hir-tree]
  (let [copy-map (collect-copy-assignments hir-tree)]
    (hir/walk-hir #(substitute-variable % copy-map) hir-tree)))

;; ============================================================================
;; КОМПОЗИЦИЯ ОПТИМИЗАЦИЙ
;; ============================================================================

(defn apply-simple-optimizations
  "Применяет все простые оптимизации к HIR дереву
   
   Порядок важен:
   1. Constant folding - создает новые константы
   2. Strength reduction - упрощает операции
   3. Copy propagation - распространяет копии
   4. Dead code elimination - удаляет неиспользуемый код"
  [hir-tree]
  (-> hir-tree
      constant-fold
      strength-reduce
      copy-propagation
      dead-code-elimination))

(defn optimize-iteratively
  "Применяет оптимизации итеративно до достижения неподвижной точки"
  [hir-tree & {:keys [max-iterations] :or {max-iterations 10}}]
  (loop [current hir-tree
         iteration 0]
    (if (>= iteration max-iterations)
      current
      (let [optimized (apply-simple-optimizations current)]
        (if (= optimized current)
          optimized  ; Достигли неподвижной точки
          (recur optimized (inc iteration)))))))

;; ============================================================================
;; СТАТИСТИКА ОПТИМИЗАЦИЙ
;; ============================================================================

(defn count-nodes-by-type
  "Подсчитывает количество узлов каждого типа"
  [hir-tree]
  (let [counts (atom {})]
    (hir/walk-hir
      (fn [node]
        (when (hir/hir-node? node)
          (swap! counts update (:op node) (fnil inc 0)))
        node)
      hir-tree)
    @counts))

(defn optimization-statistics
  "Собирает статистику до и после оптимизаций"
  [original-tree optimized-tree]
  (let [original-counts (count-nodes-by-type original-tree)
        optimized-counts (count-nodes-by-type optimized-tree)
        
        original-total (reduce + (vals original-counts))
        optimized-total (reduce + (vals optimized-counts))
        
        reduction-percent (if (pos? original-total)
                           (* 100.0 (/ (- original-total optimized-total) original-total))
                           0.0)]
    
    {:original-nodes original-total
     :optimized-nodes optimized-total
     :nodes-eliminated (- original-total optimized-total)
     :reduction-percent reduction-percent
     :original-counts original-counts
     :optimized-counts optimized-counts}))

;; ============================================================================
;; ЭКСПОРТ ФУНКЦИЙ
;; ============================================================================

(def ^:export constant-fold constant-fold)
(def ^:export strength-reduce strength-reduce)
(def ^:export dead-code-elimination dead-code-elimination)
(def ^:export copy-propagation copy-propagation)
(def ^:export apply-simple-optimizations apply-simple-optimizations)
(def ^:export optimize-iteratively optimize-iteratively)
(def ^:export optimization-statistics optimization-statistics) 