# План реализации промежуточного представления (IR) для компилятора C51

## 🎯 Цель и принципы

**Основной принцип**: "Правильный инструмент для правильной задачи"

- **Парсер**: Монады + гигиенические макросы (УЖЕ РЕАЛИЗОВАН - НЕ ТРОГАЕМ!)
- **IR трансформации**: Threading macros + `tap>` (НОВАЯ РЕАЛИЗАЦИЯ)
- **Препроцессор**: Transducers + Protocols (УЖЕ РЕАЛИЗОВАН)

---

## 🏗️ Архитектура IR

### Трехуровневая структура

```
AST → HIR → MIR → LIR → AT89S4051 Assembly
      ↑     ↑     ↑
   Высокий Средний Низкий
   уровень уровень уровень
```

### Протоколы для IR узлов

```clojure
;; Базовый протокол для всех IR узлов
(defprotocol IRNode
  (get-type [this] "Возвращает тип узла")
  (get-children [this] "Возвращает дочерние узлы")
  (transform [this f] "Применяет трансформацию"))

;; Протокол для HIR (High-level IR)
(defprotocol HIRNode
  (lower-to-mir [this context] "Понижает HIR до MIR")
  (optimize-hir [this] "Оптимизирует на уровне HIR"))

;; Протокол для MIR (Mid-level IR)  
(defprotocol MIRInstruction
  (optimize-mir [this] "Оптимизирует инструкцию MIR")
  (estimate-cost [this] "Оценивает стоимость инструкции")
  (lower-to-lir [this context] "Понижает до LIR"))

;; Протокол для LIR (Low-level IR)
(defprotocol LIRInstruction
  (allocate-registers [this register-map] "Распределяет регистры")
  (generate-assembly [this] "Генерирует ассемблер AT89S4051"))
```

---

## 📋 Детальный план реализации

### **Фаза 1: Базовая инфраструктура IR (Неделя 1-2)**

#### Неделя 1: Структуры данных и протоколы

```clojure
;; Файл: src/c51cc/ir/core.clj
(ns c51cc.ir.core
  "Базовые структуры данных для промежуточного представления")

;; HIR узлы
(defrecord HIRFunction [name params return-type body])
(defrecord HIRAssignment [dest src])
(defrecord HIRBinaryOp [op left right])
(defrecord HIRCall [function args])
(defrecord HIRIf [condition then-branch else-branch])
(defrecord HIRWhile [condition body])

;; MIR инструкции
(defrecord MIRAdd [dest src1 src2])
(defrecord MIRLoad [dest src])
(defrecord MIRStore [dest src])
(defrecord MIRJump [label])
(defrecord MIRBranch [condition true-label false-label])

;; LIR инструкции (близко к AT89S4051)
(defrecord LIRMovA [src])      ; MOV A, src
(defrecord LIRAddA [src])      ; ADD A, src
(defrecord LIRMovReg [reg src]) ; MOV Rn, src
(defrecord LIRJmp [addr])      ; JMP addr
```

#### Неделя 2: AST → HIR трансформация

```clojure
;; Файл: src/c51cc/ir/ast_to_hir.clj
(defn transform-ast-to-hir [ast]
  "Трансформирует AST в HIR с использованием threading macros"
  (->> ast
       (tap> "Input AST")
       normalize-ast-structure
       (tap> "Normalized AST")
       transform-declarations
       (tap> "HIR declarations")
       transform-statements
       (tap> "HIR statements")
       transform-expressions
       (tap> "Complete HIR")))

;; Конкретные трансформации
(defn transform-function-declaration [ast-node]
  (-> ast-node
      extract-function-signature
      (transform-parameter-list)
      (transform-function-body)
      (create-hir-function)))
```

### **Фаза 2: HIR оптимизации (Неделя 3)**

```clojure
;; Файл: src/c51cc/ir/hir_optimizations.clj
(defn optimize-hir [hir]
  "Оптимизации на уровне HIR"
  (->> hir
       (tap> "HIR before optimization")
       inline-simple-functions
       (tap> "After function inlining")
       constant-propagation
       (tap> "After constant propagation")
       dead-code-elimination-hir
       (tap> "After dead code elimination")
       simplify-control-flow
       (tap> "Optimized HIR")))

;; Специфичные оптимизации
(defn inline-simple-functions [hir]
  "Инлайнинг простых функций для экономии памяти AT89S4051"
  (->> hir
       :functions
       (map inline-if-simple)
       (assoc hir :functions)))
```

### **Фаза 3: HIR → MIR понижение (Неделя 4)**

```clojure
;; Файл: src/c51cc/ir/hir_to_mir.clj
(defn lower-hir-to-mir [hir]
  "Понижает HIR до MIR (трехадресный код)"
  (->> hir
       (tap> "HIR input")
       linearize-control-flow
       (tap> "Linearized control flow")
       convert-to-three-address
       (tap> "Three-address code")
       create-basic-blocks
       (tap> "Basic blocks")
       build-control-flow-graph
       (tap> "Control flow graph")))

;; Создание трехадресного кода
(defn convert-expression-to-mir [expr temp-counter]
  (match expr
    {:type :binary-op :op op :left left :right right}
    (let [left-mir (convert-expression-to-mir left temp-counter)
          right-mir (convert-expression-to-mir right (+ temp-counter 1))
          result-temp (str "t" (+ temp-counter 2))]
      [(->MIRAdd result-temp (:result left-mir) (:result right-mir))
       {:result result-temp :instructions (concat (:instructions left-mir)
                                                 (:instructions right-mir))}])
    
    {:type :constant :value val}
    [[] {:result val :instructions []}]))
```

### **Фаза 4: MIR оптимизации (Неделя 5)**

```clojure
;; Файл: src/c51cc/ir/mir_optimizations.clj
(defn optimize-mir [mir-cfg]
  "Оптимизации на уровне MIR"
  (->> mir-cfg
       (tap> "MIR before optimization")
       optimize-basic-blocks
       (tap> "After basic block optimization")
       eliminate-common-subexpressions
       (tap> "After CSE")
       optimize-for-at89s4051
       (tap> "After AT89S4051 optimizations")))

(defn optimize-basic-block [basic-block]
  "Оптимизирует один базовый блок"
  (->> basic-block
       :instructions
       (map constant-fold-instruction)
       (remove dead-instruction?)
       (map strength-reduce-instruction)
       (map optimize-at89s4051-specific)
       (vec)
       (assoc basic-block :instructions)))

;; Специфичные для AT89S4051 оптимизации
(defn optimize-at89s4051-specific [instruction]
  "Оптимизации для архитектуры AT89S4051"
  (match instruction
    {:op :mul :src2 {:type :const :value n}}
    (when (power-of-two? n)
      {:op :shift-left :amount (log2 n)})  ; x * 2^n → x << n
    
    {:op :add :src2 {:type :const :value 0}}
    {:op :nop}  ; x + 0 → nop (будет удалено)
    
    :else instruction))
```

### **Фаза 5: Распределение регистров (Неделя 6)**

```clojure
;; Файл: src/c51cc/ir/register_allocation.clj
(defn allocate-registers [mir-cfg]
  "Распределение регистров для AT89S4051"
  (->> mir-cfg
       (tap> "MIR before register allocation")
       compute-liveness-analysis
       (tap> "Liveness analysis complete")
       build-interference-graph
       (tap> "Interference graph built")
       color-graph-at89s4051
       (tap> "Register allocation complete")
       rewrite-with-registers
       (tap> "MIR with allocated registers")))

;; Регистры AT89S4051
(def at89s4051-registers
  {:accumulator :A
   :general [:R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7]
   :special [:DPTR :SP :PSW]})

(defn color-graph-at89s4051 [interference-graph]
  "Раскраска графа интерференции для AT89S4051"
  (let [available-colors (concat [:A] (:general at89s4051-registers))]
    (->> interference-graph
         (sort-by degree >)  ; Сортируем по степени (больше соседей = раньше)
         (reduce assign-register available-colors))))
```

### **Фаза 6: Генерация ассемблера (Неделя 7)**

```clojure
;; Файл: src/c51cc/ir/code_generation.clj
(defn generate-assembly [mir-with-registers]
  "Генерирует ассемблер AT89S4051"
  (->> mir-with-registers
       (tap> "MIR with registers")
       convert-to-lir
       (tap> "LIR instructions")
       optimize-lir
       (tap> "Optimized LIR")
       generate-at89s4051-assembly
       (tap> "Final assembly code")))

(defn convert-mir-to-lir [mir-instruction]
  "Конвертирует MIR инструкцию в LIR"
  (match mir-instruction
    {:op :add :dest dest :src1 src1 :src2 src2}
    [(->LIRMovA src1)
     (->LIRAddA src2)
     (->LIRMovReg dest :A)]
    
    {:op :load :dest dest :src src}
    [(->LIRMovReg dest src)]
    
    {:op :jump :label label}
    [(->LIRJmp label)]))

(defn generate-at89s4051-instruction [lir-instruction]
  "Генерирует строку ассемблера для AT89S4051"
  (match lir-instruction
    (->LIRMovA src)
    (str "MOV A, " (format-operand src))
    
    (->LIRAddA src)
    (str "ADD A, " (format-operand src))
    
    (->LIRMovReg reg src)
    (str "MOV " (name reg) ", " (format-operand src))))
```

### **Фаза 7: Интеграция с существующим парсером (Неделя 8)**

```clojure
;; Файл: src/c51cc/compiler.clj
(defn compile-c51 [source-code options]
  "Полный pipeline компиляции C51"
  (->> source-code
       (tap> "Source code")
       
       ;; Препроцессор (УЖЕ РЕАЛИЗОВАН)
       (preprocess-v2 options)
       :result
       (tap> "Preprocessed code")
       
       ;; Лексер (УЖЕ РЕАЛИЗОВАН)
       tokenize
       (tap> "Tokens")
       
       ;; Парсер (УЖЕ РЕАЛИЗОВАН - НЕ ТРОГАЕМ!)
       parse
       :ast
       (tap> "AST from existing parser")
       
       ;; НОВЫЙ IR pipeline
       transform-ast-to-hir
       (tap> "HIR")
       optimize-hir
       (tap> "Optimized HIR")
       lower-hir-to-mir
       (tap> "MIR")
       optimize-mir
       (tap> "Optimized MIR")
       allocate-registers
       (tap> "MIR with registers")
       generate-assembly
       (tap> "Final AT89S4051 assembly")))
```

---

## 🔧 Настройка отладки с `tap>`

```clojure
;; Файл: src/c51cc/debug.clj
(defn setup-compilation-debugging []
  "Настраивает отладку компиляции"
  
  ;; Вывод в консоль
  (add-tap println)
  
  ;; Запись в файл
  (add-tap #(spit "compilation-debug.log" 
                  (str (java.time.LocalDateTime/now) ": " % "\n") 
                  :append true))
  
  ;; Структурированный вывод для IR
  (add-tap (fn [data]
             (when (map? data)
               (clojure.pprint/pprint data)))))

;; Использование
(setup-compilation-debugging)
(compile-c51 source-code {:target :at89s4051})
```

---

## 📊 Метрики и тестирование

### Тестирование каждой фазы

```clojure
;; Тесты для AST → HIR
(deftest test-ast-to-hir-transformation
  (let [ast {:type :function :name "test" :body [...]}
        hir (transform-ast-to-hir ast)]
    (is (= (:type hir) :hir-function))
    (is (= (:name hir) "test"))))

;; Тесты для оптимизаций
(deftest test-constant-folding
  (let [mir-before {:op :add :src1 5 :src2 3}
        mir-after (constant-fold-instruction mir-before)]
    (is (= mir-after {:op :load :src 8}))))

;; Интеграционные тесты
(deftest test-full-compilation
  (let [source "void main() { int x = 5 + 3; }"
        assembly (compile-c51 source {})]
    (is (str/includes? assembly "MOV"))
    (is (str/includes? assembly "8"))))  ; Константа должна быть свернута
```

### Бенчмарки производительности

```clojure
(defn benchmark-compilation []
  (let [test-programs ["simple.c" "complex.c" "interrupt.c"]]
    (doseq [program test-programs]
      (let [source (slurp program)
            start-time (System/nanoTime)]
        (compile-c51 source {})
        (let [end-time (System/nanoTime)
              duration-ms (/ (- end-time start-time) 1000000.0)]
          (println (str program ": " duration-ms " ms")))))))
```

---

## 🎯 Ожидаемые результаты

### Качество генерируемого кода
- **Размер кода**: -20-30% (благодаря оптимизациям)
- **Скорость выполнения**: +10-15% (оптимизация для AT89S4051)
- **Использование памяти**: Оптимальное (учет ограничений 4KB ROM)

### Качество разработки
- **Отладочность**: +200% (благодаря `tap>`)
- **Тестируемость**: +150% (каждая фаза тестируется отдельно)
- **Сопровождаемость**: +100% (четкое разделение ответственности)

---

## 🚨 Риски и митигация

### Технические риски
1. **Сложность интеграции**: Минимальна (существующий парсер не трогаем)
2. **Производительность**: Контролируется бенчмарками
3. **Качество кода**: Контролируется тестами

### Временные риски
1. **Превышение сроков**: Фазы можно выполнять независимо
2. **Блокирующие зависимости**: Отсутствуют (парсер уже работает)

---

## ✅ Критерии готовности

### Фаза считается завершенной, если:
1. Все тесты проходят
2. Бенчмарки показывают приемлемую производительность
3. `tap>` отладка работает корректно
4. Документация обновлена
5. Интеграция с существующим кодом не нарушена

**Главный принцип**: Существующий парсер с монадами остается нетронутым!

**Статус**: ✅ ГОТОВ К РЕАЛИЗАЦИИ

**Приоритет**: ВЫСОКИЙ

**Риски**: НИЗКИЕ 