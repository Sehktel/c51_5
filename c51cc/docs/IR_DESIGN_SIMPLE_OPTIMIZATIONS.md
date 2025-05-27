# Промежуточное представление (IR) и простые оптимизации для AT89S4051

## 🎯 Цели и ограничения

**Целевая платформа**: AT89S4051 (8-битный микроконтроллер)  
**Уровень оптимизаций**: Простые (constant folding, dead code elimination, strength reduction)  
**Приоритет**: Размер кода > Скорость выполнения (ограниченная память 4KB ROM)

---

## 🏗️ Архитектура IR

### Трёхуровневая структура IR

```
AST → HIR → MIR → AT89S4051 Assembly
      ↑     ↑     ↑
   Высокий Средний Низкий
   уровень уровень уровень
```

#### 1. HIR (High-level IR) - Близко к AST
- Сохраняет структуру исходного кода
- Операции высокого уровня (for, while, function calls)
- Типизированные выражения
- Области видимости

#### 2. MIR (Mid-level IR) - Трёхадресный код
- Простые операции (add, sub, load, store)
- Временные переменные
- Базовые блоки и control flow graph
- Платформо-независимые оптимизации

#### 3. LIR (Low-level IR) - Близко к ассемблеру
- Инструкции AT89S4051
- Распределение регистров
- Адресация памяти
- Платформо-зависимые оптимизации

---

## 📊 Определение структур данных

### HIR (High-level IR)

```clojure
(ns c51cc.ir.hir
  "High-level промежуточное представление")

;; Базовая структура HIR узла
(defrecord HIR-Node 
  [op          ; Операция (:assign, :call, :if, :while, :for, :return)
   type        ; Тип данных (:uint8, :uint16, :int8, :int16, :bit, :pointer)
   operands    ; Операнды (вектор HIR-Node или примитивов)
   metadata])  ; Метаданные (номер строки, файл, комментарии)

;; Типы операций HIR
(def hir-operations
  #{:assign      ; Присваивание: x = y
    :binary-op   ; Бинарные операции: x = y + z
    :unary-op    ; Унарные операции: x = -y
    :call        ; Вызов функции: f(x, y)
    :if          ; Условный переход
    :while       ; Цикл while
    :for         ; Цикл for
    :return      ; Возврат из функции
    :block       ; Блок операторов
    :load        ; Загрузка переменной
    :store       ; Сохранение переменной
    :cast        ; Приведение типов
    :array-ref   ; Доступ к массиву
    :member-ref  ; Доступ к полю структуры
    })

;; Конструкторы HIR узлов
(defn hir-assign [dest src type]
  (->HIR-Node :assign type [dest src] {}))

(defn hir-binary-op [op left right result-type]
  (->HIR-Node :binary-op result-type [op left right] {}))

(defn hir-call [function args return-type]
  (->HIR-Node :call return-type [function args] {}))

;; Пример HIR для простого выражения: x = a + b * 2
(def example-hir
  (hir-assign 
    (hir-load "x" :uint8)
    (hir-binary-op :add
      (hir-load "a" :uint8)
      (hir-binary-op :multiply
        (hir-load "b" :uint8)
        (hir-constant 2 :uint8)
        :uint8)
      :uint8)
    :uint8))
```

### MIR (Mid-level IR)

```clojure
(ns c51cc.ir.mir
  "Mid-level промежуточное представление - трёхадресный код")

;; Трёхадресная инструкция
(defrecord MIR-Instruction
  [op      ; Операция (:add, :sub, :mul, :div, :load, :store, :jump, :branch)
   dest    ; Назначение (переменная или временная)
   src1    ; Первый операнд
   src2    ; Второй операнд (может быть nil для унарных операций)
   type    ; Тип данных
   label]) ; Метка для переходов (опционально)

;; Базовый блок
(defrecord BasicBlock
  [label        ; Уникальная метка блока
   instructions ; Вектор MIR-Instruction
   successors   ; Множество следующих блоков
   predecessors ; Множество предыдущих блоков
   live-in      ; Множество живых переменных на входе
   live-out])   ; Множество живых переменных на выходе

;; Control Flow Graph
(defrecord CFG
  [entry-block  ; Входной блок
   exit-block   ; Выходной блок
   blocks       ; Map label -> BasicBlock
   ])

;; Конструкторы MIR инструкций
(defn mir-add [dest src1 src2 type]
  (->MIR-Instruction :add dest src1 src2 type nil))

(defn mir-load [dest src type]
  (->MIR-Instruction :load dest src nil type nil))

(defn mir-store [dest src type]
  (->MIR-Instruction :store dest src nil type nil))

(defn mir-branch [condition true-label false-label]
  (->MIR-Instruction :branch nil condition nil :bool true-label))

;; Пример MIR для x = a + b * 2:
(def example-mir
  [(mir-load "t1" "a" :uint8)      ; t1 = a
   (mir-load "t2" "b" :uint8)      ; t2 = b  
   (mir-add "t3" "t2" 2 :uint8)    ; t3 = t2 * 2 (будет оптимизировано в shift)
   (mir-add "t4" "t1" "t3" :uint8) ; t4 = t1 + t3
   (mir-store "x" "t4" :uint8)])   ; x = t4
```

---

## 🔧 Простые оптимизации

### 1. Constant Folding (Свертывание констант)

```clojure
(ns c51cc.optimizations.constant-folding
  "Оптимизация: свертывание константных выражений")

(defn constant? [operand]
  "Проверяет, является ли операнд константой"
  (number? operand))

(defn fold-binary-op [op left right]
  "Вычисляет результат бинарной операции с константами"
  (when (and (constant? left) (constant? right))
    (case op
      :add (+ left right)
      :sub (- left right)
      :mul (* left right)
      :div (when (not= right 0) (quot left right))
      :mod (when (not= right 0) (rem left right))
      :and (bit-and left right)
      :or  (bit-or left right)
      :xor (bit-xor left right)
      :shl (bit-shift-left left right)
      :shr (bit-shift-right left right)
      nil)))

(defn constant-fold-instruction [instr]
  "Применяет constant folding к одной инструкции"
  (if (and (= (:op instr) :add)
           (constant? (:src1 instr))
           (constant? (:src2 instr)))
    ;; Заменяем add на load с вычисленной константой
    (mir-load (:dest instr) 
              (fold-binary-op :add (:src1 instr) (:src2 instr))
              (:type instr))
    instr))

(defn constant-fold-block [basic-block]
  "Применяет constant folding ко всем инструкциям блока"
  (update basic-block :instructions 
          #(mapv constant-fold-instruction %)))

;; Пример:
;; БЫЛО: t1 = 5 + 3
;; СТАЛО: t1 = 8
```

### 2. Dead Code Elimination (Удаление мертвого кода)

```clojure
(ns c51cc.optimizations.dead-code-elimination
  "Оптимизация: удаление неиспользуемого кода")

(defn collect-used-variables [instructions]
  "Собирает множество используемых переменных"
  (reduce (fn [used instr]
            (-> used
                (cond-> (:src1 instr) (conj (:src1 instr)))
                (cond-> (:src2 instr) (conj (:src2 instr)))))
          #{}
          instructions))

(defn variable-used? [var used-vars]
  "Проверяет, используется ли переменная"
  (contains? used-vars var))

(defn dead-instruction? [instr used-vars]
  "Проверяет, является ли инструкция мертвой"
  (and (#{:add :sub :mul :div :load} (:op instr))
       (not (variable-used? (:dest instr) used-vars))))

(defn eliminate-dead-code [basic-block]
  "Удаляет мертвые инструкции из блока"
  (let [instructions (:instructions basic-block)
        used-vars (collect-used-variables instructions)
        live-instructions (remove #(dead-instruction? % used-vars) instructions)]
    (assoc basic-block :instructions (vec live-instructions))))

;; Пример:
;; БЫЛО: t1 = a + b
;;       t2 = c + d  ; t2 не используется
;;       return t1
;; СТАЛО: t1 = a + b
;;        return t1
```

### 3. Strength Reduction (Упрощение операций)

```clojure
(ns c51cc.optimizations.strength-reduction
  "Оптимизация: замена дорогих операций на дешевые")

(defn power-of-two? [n]
  "Проверяет, является ли число степенью двойки"
  (and (pos? n) (zero? (bit-and n (dec n)))))

(defn log2 [n]
  "Вычисляет log2 для степени двойки"
  (loop [n n, result 0]
    (if (= n 1)
      result
      (recur (bit-shift-right n 1) (inc result)))))

(defn strength-reduce-instruction [instr]
  "Применяет strength reduction к инструкции"
  (case (:op instr)
    :mul
    (cond
      ;; x * 2^n → x << n
      (and (constant? (:src2 instr)) (power-of-two? (:src2 instr)))
      (->MIR-Instruction :shl (:dest instr) (:src1 instr) 
                         (log2 (:src2 instr)) (:type instr) nil)
      
      ;; x * 1 → x
      (and (constant? (:src2 instr)) (= (:src2 instr) 1))
      (mir-load (:dest instr) (:src1 instr) (:type instr))
      
      ;; x * 0 → 0
      (and (constant? (:src2 instr)) (= (:src2 instr) 0))
      (mir-load (:dest instr) 0 (:type instr))
      
      :else instr)
    
    :div
    (cond
      ;; x / 2^n → x >> n
      (and (constant? (:src2 instr)) (power-of-two? (:src2 instr)))
      (->MIR-Instruction :shr (:dest instr) (:src1 instr) 
                         (log2 (:src2 instr)) (:type instr) nil)
      
      ;; x / 1 → x
      (and (constant? (:src2 instr)) (= (:src2 instr) 1))
      (mir-load (:dest instr) (:src1 instr) (:type instr))
      
      :else instr)
    
    :add
    (cond
      ;; x + 0 → x
      (and (constant? (:src2 instr)) (= (:src2 instr) 0))
      (mir-load (:dest instr) (:src1 instr) (:type instr))
      
      :else instr)
    
    ;; Для остальных операций возвращаем без изменений
    instr))

;; Пример:
;; БЫЛО: t1 = a * 8
;; СТАЛО: t1 = a << 3
```

### 4. Copy Propagation (Распространение копий)

```clojure
(ns c51cc.optimizations.copy-propagation
  "Оптимизация: распространение копий переменных")

(defn copy-instruction? [instr]
  "Проверяет, является ли инструкция простым копированием"
  (and (= (:op instr) :load)
       (not (constant? (:src1 instr)))))

(defn build-copy-map [instructions]
  "Строит карту копий: dest -> src"
  (reduce (fn [copy-map instr]
            (if (copy-instruction? instr)
              (assoc copy-map (:dest instr) (:src1 instr))
              copy-map))
          {}
          instructions))

(defn substitute-copies [instr copy-map]
  "Заменяет переменные их копиями"
  (-> instr
      (update :src1 #(get copy-map % %))
      (update :src2 #(get copy-map % %))))

(defn propagate-copies [basic-block]
  "Применяет copy propagation к блоку"
  (let [instructions (:instructions basic-block)
        copy-map (build-copy-map instructions)
        propagated (mapv #(substitute-copies % copy-map) instructions)]
    (assoc basic-block :instructions propagated)))

;; Пример:
;; БЫЛО: t1 = a
;;       t2 = t1 + b
;; СТАЛО: t1 = a
;;        t2 = a + b
```

---

## 🎯 Специфичные для AT89S4051 оптимизации

### 1. Bit-операции

```clojure
(defn optimize-bit-operations [instr]
  "Оптимизирует операции с битами для AT89S4051"
  (case (:op instr)
    :and
    (cond
      ;; x & 1 → проверка младшего бита
      (and (constant? (:src2 instr)) (= (:src2 instr) 1))
      (->MIR-Instruction :bit-test (:dest instr) (:src1 instr) 0 :bit nil)
      
      ;; x & 0xFF → x (для uint8)
      (and (constant? (:src2 instr)) (= (:src2 instr) 0xFF) (= (:type instr) :uint8))
      (mir-load (:dest instr) (:src1 instr) (:type instr))
      
      :else instr)
    
    :or
    (cond
      ;; x | 0 → x
      (and (constant? (:src2 instr)) (= (:src2 instr) 0))
      (mir-load (:dest instr) (:src1 instr) (:type instr))
      
      :else instr)
    
    instr))
```

### 2. Оптимизация использования аккумулятора

```clojure
(defn optimize-accumulator-usage [instructions]
  "Оптимизирует использование аккумулятора AT89S4051"
  ;; Группирует операции для минимизации загрузок в аккумулятор
  ;; Пример: вместо MOV A, R0; ADD A, R1; MOV R2, A; MOV A, R2; ADD A, R3
  ;; Генерирует: MOV A, R0; ADD A, R1; ADD A, R3; MOV R2, A
  )
```

### 3. Оптимизация памяти

```clojure
(defn optimize-memory-usage [cfg]
  "Оптимизирует использование ограниченной памяти AT89S4051"
  ;; 1. Переиспользование временных переменных
  ;; 2. Упаковка битовых переменных
  ;; 3. Минимизация использования стека
  )
```

---

## 📋 План реализации

### Фаза 1: Базовая инфраструктура IR (1 неделя)
1. ✅ Определить структуры данных HIR/MIR
2. ✅ Создать конструкторы и утилиты
3. ✅ Написать базовые тесты
4. 🆕 Реализовать AST → HIR трансформацию

### Фаза 2: Простые оптимизации (1 неделя)
1. 🆕 Constant folding
2. 🆕 Dead code elimination  
3. 🆕 Strength reduction
4. 🆕 Copy propagation

### Фаза 3: HIR → MIR понижение (1 неделя)
1. 🆕 Трансформация циклов в переходы
2. 🆕 Линеаризация выражений
3. 🆕 Создание базовых блоков
4. 🆕 Построение CFG

### Фаза 4: AT89S4051-специфичные оптимизации (1 неделя)
1. 🆕 Оптимизация битовых операций
2. 🆕 Оптимизация использования аккумулятора
3. 🆕 Минимизация использования памяти
4. 🆕 Интеграционные тесты

---

## 🧪 Тестирование

### Unit тесты для каждой оптимизации:
```clojure
(deftest test-constant-folding
  (testing "Свертывание арифметических констант"
    (let [instr (mir-add "t1" 5 3 :uint8)
          optimized (constant-fold-instruction instr)]
      (is (= (:op optimized) :load))
      (is (= (:src1 optimized) 8)))))

(deftest test-strength-reduction
  (testing "Замена умножения на степень 2 сдвигом"
    (let [instr (mir-mul "t1" "a" 8 :uint8)
          optimized (strength-reduce-instruction instr)]
      (is (= (:op optimized) :shl))
      (is (= (:src2 optimized) 3)))))
```

### Интеграционные тесты:
```clojure
(deftest test-optimization-pipeline
  (testing "Полный pipeline оптимизаций"
    (let [original-mir [(mir-add "t1" 5 3 :uint8)
                        (mir-mul "t2" "t1" 8 :uint8)
                        (mir-add "unused" 1 2 :uint8)]
          optimized (-> original-mir
                        (apply-optimization constant-fold-block)
                        (apply-optimization eliminate-dead-code)
                        (apply-optimization strength-reduce-block))]
      ;; Проверяем результат оптимизаций
      )))
```

---

## 📊 Ожидаемые результаты

### Метрики улучшения:
- **Размер кода**: -15-25% (constant folding + dead code elimination)
- **Скорость выполнения**: +10-20% (strength reduction)
- **Использование памяти**: -10-15% (оптимизация временных переменных)

### Примеры оптимизаций:
```c
// Исходный код:
int x = 5 + 3;
int y = x * 8;
int unused = 10;

// После оптимизаций:
int x = 8;        // constant folding
int y = x << 3;   // strength reduction
// unused удален   // dead code elimination
```

---

*Документ создан для планирования IR и простых оптимизаций компилятора C51*  
*Фокус: AT89S4051, простые оптимизации, размер кода*  
*Автор: AI Assistant*  
*Дата: 2024* 