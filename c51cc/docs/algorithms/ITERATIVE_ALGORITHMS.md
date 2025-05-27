# Итеративные алгоритмы: Анализ живости и доминаторы

## 🎯 Обзор

Итеративные алгоритмы в компиляторах используются для анализа потоков данных, где результат вычисляется через повторные итерации до достижения неподвижной точки. Основные применения:

- **Анализ живости переменных** - определение, где переменные используются
- **Анализ доминаторов** - поиск блоков, через которые обязательно проходит выполнение
- **Анализ достижимых определений** - отслеживание присваиваний переменным

---

## 📊 Блок-схемы алгоритмов

### **Общая схема итеративного анализа**

```
┌─────────────────┐
│  Инициализация  │
│  начального     │
│  состояния      │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Применение     │
│  передаточной   │
│  функции        │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Сравнение с    │
│  предыдущим     │
│  состоянием     │
└─────────┬───────┘
          │
          ▼
      ┌───────┐
      │Равны? │
      └───┬───┘
          │
    ┌─────┴─────┐
    │ Да        │ Нет
    ▼           ▼
┌───────┐   ┌───────────┐
│СТОП   │   │Итерация + 1│
│Результ│   │Продолжить │
└───────┘   └─────┬─────┘
                  │
                  ▼
            ┌─────────────┐
            │Превышен     │
            │лимит        │
            │итераций?    │
            └─────┬───────┘
                  │
            ┌─────┴─────┐
            │ Да        │ Нет
            ▼           ▼
        ┌───────┐   ┌───────────┐
        │СТОП   │   │Вернуться  │
        │Таймаут│   │к применению│
        └───────┘   │функции    │
                    └─────┬─────┘
                          │
                          ▲─────────┘
```

### **Схема анализа живости переменных**

```
┌─────────────────────────────────────────────────────────────┐
│                    АНАЛИЗ ЖИВОСТИ                           │
└─────────────────────────────────────────────────────────────┘

┌─────────────────┐
│  Инициализация: │
│  live-in = {}   │
│  live-out = {}  │
│  для всех блоков│
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Для каждого    │
│  блока B в CFG  │
└─────────┬───────┘
          │
          ▼
┌─────────────────────────────────────┐
│  live-out[B] = ⋃ live-in[S]        │
│  для всех преемников S блока B      │
└─────────┬───────────────────────────┘
          │
          ▼
┌─────────────────────────────────────┐
│  live-in[B] = use[B] ⋃             │
│  (live-out[B] - def[B])            │
└─────────┬───────────────────────────┘
          │
          ▼
┌─────────────────┐
│  Состояние      │
│  изменилось?    │
└─────────┬───────┘
          │
    ┌─────┴─────┐
    │ Да        │ Нет
    ▼           ▼
┌───────────┐ ┌─────────────┐
│Продолжить │ │ СХОДИМОСТЬ  │
│итерацию   │ │ ДОСТИГНУТА  │
└─────┬─────┘ └─────────────┘
      │
      ▲───────────┘

Где:
• use[B] - переменные, используемые в B до определения
• def[B] - переменные, определяемые в B
• live-in[B] - живые переменные на входе в B
• live-out[B] - живые переменные на выходе из B
```

### **Схема анализа доминаторов**

```
┌─────────────────────────────────────────────────────────────┐
│                   АНАЛИЗ ДОМИНАТОРОВ                        │
└─────────────────────────────────────────────────────────────┘

┌─────────────────┐
│  Инициализация: │
│  Dom(entry) =   │
│  {entry}        │
│  Dom(остальные) │
│  = все узлы     │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Для каждого    │
│  блока B ≠ entry│
└─────────┬───────┘
          │
          ▼
┌─────────────────────────────────────┐
│  Есть                               │
│  предшественники?                   │
└─────────┬───────────────────────────┘
          │
    ┌─────┴─────┐
    │ Да        │ Нет
    ▼           ▼
┌───────────────────────┐ ┌─────────────┐
│ Dom(B) = {B} ⋃        │ │ Dom(B) = {} │
│ (⋂ Dom(P) для всех    │ │             │
│ предшественников P)   │ │             │
└─────────┬─────────────┘ └─────┬───────┘
          │                     │
          └─────────┬───────────┘
                    │
                    ▼
          ┌─────────────────┐
          │  Состояние      │
          │  изменилось?    │
          └─────────┬───────┘
                    │
              ┌─────┴─────┐
              │ Да        │ Нет
              ▼           ▼
          ┌───────────┐ ┌─────────────┐
          │Продолжить │ │ СХОДИМОСТЬ  │
          │итерацию   │ │ ДОСТИГНУТА  │
          └─────┬─────┘ └─────────────┘
                │
                ▲───────────┘

Результат: Dom(B) содержит все блоки, доминирующие B
```

### **Схема построения дерева доминаторов**

```
┌─────────────────────────────────────────────────────────────┐
│              ПОСТРОЕНИЕ ДЕРЕВА ДОМИНАТОРОВ                  │
└─────────────────────────────────────────────────────────────┘

┌─────────────────┐
│  Результат      │
│  анализа        │
│  доминаторов    │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Для каждого    │
│  узла N         │
└─────────┬───────┘
          │
          ▼
┌─────────────────────────────────────┐
│  Найти непосредственный             │
│  доминатор idom(N):                 │
│  - Из Dom(N) \ {N}                  │
│  - Который не доминируется          │
│    другими в Dom(N) \ {N}           │
└─────────┬───────────────────────────┘
          │
          ▼
┌─────────────────┐
│  idom(N)        │
│  найден?        │
└─────────┬───────┘
          │
    ┌─────┴─────┐
    │ Да        │ Нет
    ▼           ▼
┌───────────────┐ ┌─────────────┐
│Добавить ребро │ │N - корень   │
│idom(N) → N    │ │дерева       │
│в дерево       │ │             │
└─────┬─────────┘ └─────────────┘
      │
      ▼
┌─────────────────┐
│  Все узлы       │
│  обработаны?    │
└─────────┬───────┘
          │
    ┌─────┴─────┐
    │ Да        │ Нет
    ▼           ▼
┌───────────┐ ┌─────────────┐
│ДЕРЕВО     │ │Следующий    │
│ГОТОВО     │ │узел         │
└───────────┘ └─────┬───────┘
                    │
                    ▲─────────┘
```

---

## 🔄 Анализ живости переменных

### **Математическая основа**

Анализ живости - это **обратный анализ потока данных**:

```
live-out[B] = ⋃ live-in[S] для всех преемников S блока B
live-in[B] = use[B] ⋃ (live-out[B] - def[B])
```

Где:
- `use[B]` - переменные, используемые в блоке B до их определения
- `def[B]` - переменные, определяемые в блоке B
- `live-in[B]` - живые переменные на входе в блок B
- `live-out[B]` - живые переменные на выходе из блока B

### **Реализация через `iterate` + `take-while`**

```clojure
(defn compute-liveness [cfg]
  "Итеративный анализ живости до неподвижной точки"
  (let [initial-state (initialize-liveness cfg)]
    (->> initial-state
         (iterate #(liveness-iteration % cfg))           ; Ленивая последовательность итераций
         (partition 2 1)                                 ; Пары (prev, current) для сравнения
         (take-while #(not= (first %) (second %)))       ; До сходимости
         last                                             ; Последняя пара
         second                                           ; Текущее состояние
         (tap> "Liveness analysis converged"))))

(defn liveness-iteration [state cfg]
  "Одна итерация анализа живости"
  (reduce (fn [acc block]
            (let [;; Вычисляем live-out как объединение live-in всех преемников
                  new-live-out (apply set/union 
                                     (map #(get-in acc [:live-in %]) 
                                          (:successors block)))
                  
                  ;; Вычисляем live-in по формуле: use ⋃ (live-out - def)
                  new-live-in (set/union (:use block)
                                        (set/difference new-live-out (:def block)))]
              
              (-> acc
                  (assoc-in [:live-in (:label block)] new-live-in)
                  (assoc-in [:live-out (:label block)] new-live-out))))
          state
          (:blocks cfg)))

(defn initialize-liveness [cfg]
  "Инициализация состояния для анализа живости"
  {:live-in (zipmap (map :label (:blocks cfg)) (repeat #{}))
   :live-out (zipmap (map :label (:blocks cfg)) (repeat #{}))})
```

### **Альтернативная реализация с `loop`/`recur`**

```clojure
(defn compute-liveness-loop [cfg]
  "Анализ живости с явным контролем итераций"
  (loop [state (initialize-liveness cfg)
         iteration 0
         max-iterations 100]  ; Защита от бесконечного цикла
    
    (let [new-state (liveness-iteration state cfg)]
      (cond
        ;; Достигнута неподвижная точка
        (= state new-state)
        (do (tap> (str "Liveness converged after " iteration " iterations"))
            new-state)
        
        ;; Превышен лимит итераций
        (>= iteration max-iterations)
        (do (tap> (str "Liveness: max iterations reached: " max-iterations))
            new-state)
        
        ;; Продолжаем итерацию
        :else
        (recur new-state (inc iteration) max-iterations)))))
```

### **Оптимизированная версия с transducers**

```clojure
(defn compute-liveness-optimized [cfg]
  "Анализ живости с использованием transducers для эффективности"
  (let [convergence-xf (comp
                        (map #(liveness-iteration % cfg))
                        (partition-by identity)  ; Группируем одинаковые состояния
                        (take 1)                 ; Берем первую группу (сходимость)
                        (map first))]            ; Извлекаем состояние
    
    (->> (initialize-liveness cfg)
         (iterate identity)  ; Начальная последовательность
         (sequence convergence-xf)
         first)))
```

---

## 🏛️ Анализ доминаторов

### **Математическая основа**

Блок A **доминирует** блок B, если каждый путь от входного блока к B проходит через A.

```
Dom(entry) = {entry}
Dom(B) = {B} ⋃ (⋂ Dom(P) для всех предшественников P блока B)
```

### **Реализация анализа доминаторов**

```clojure
(defn compute-dominators [cfg]
  "Итеративный анализ доминаторов"
  (let [all-nodes (set (map :label (:blocks cfg)))
        initial-state {:dominators 
                      (assoc (zipmap (map :label (:blocks cfg)) 
                                    (repeat all-nodes))  ; Все блоки изначально доминируют всеми
                             (:entry cfg) #{(:entry cfg)})}]  ; Кроме входного блока
    (->> initial-state
         (iterate #(dominators-iteration % cfg))
         (partition 2 1)
         (take-while #(not= (first %) (second %)))
         last
         second
         (tap> "Dominators analysis converged"))))

(defn dominators-iteration [state cfg]
  "Одна итерация анализа доминаторов"
  (reduce (fn [acc block]
            (if (= (:label block) (:entry cfg))
              acc  ; Входной блок не изменяется
              (let [predecessors (:predecessors block)
                    ;; Пересечение доминаторов всех предшественников
                    new-dominators (if (empty? predecessors)
                                    #{}
                                    (apply set/intersection
                                           (map #(get-in acc [:dominators %])
                                                predecessors)))]
                ;; Добавляем сам блок к его доминаторам
                (assoc-in acc [:dominators (:label block)]
                         (conj new-dominators (:label block))))))
          state
          (:blocks cfg)))
```

### **Построение дерева доминаторов**

```clojure
(defn build-dominator-tree [dominators]
  "Строит дерево доминаторов из результатов анализа"
  (let [immediate-dominators (compute-immediate-dominators dominators)]
    (reduce (fn [tree [node idom]]
              (if idom
                (update tree idom (fnil conj #{}) node)
                tree))
            {}
            immediate-dominators)))

(defn compute-immediate-dominators [dominators]
  "Вычисляет непосредственные доминаторы"
  (into {}
        (map (fn [[node doms]]
               [node (find-immediate-dominator node doms dominators)])
             dominators)))

(defn find-immediate-dominator [node dominators all-dominators]
  "Находит непосредственный доминатор для узла"
  (let [strict-dominators (disj dominators node)]
    (->> strict-dominators
         (filter (fn [candidate]
                   ;; Кандидат является непосредственным доминатором,
                   ;; если он не доминируется другими доминаторами узла
                   (not-any? #(and (not= % candidate)
                                  (contains? (get all-dominators %) candidate))
                            strict-dominators)))
         first)))
```

---

## 📊 Анализ достижимых определений

### **Математическая основа**

Анализ достижимых определений - это **прямой анализ потока данных**:

```
in[B] = ⋃ out[P] для всех предшественников P блока B
out[B] = gen[B] ⋃ (in[B] - kill[B])
```

Где:
- `gen[B]` - определения, генерируемые в блоке B
- `kill[B]` - определения, уничтожаемые в блоке B

### **Реализация**

```clojure
(defn compute-reaching-definitions [cfg]
  "Анализ достижимых определений до неподвижной точки"
  (let [initial-state (initialize-reaching-definitions cfg)]
    (->> initial-state
         (iterate #(reaching-definitions-step % cfg))
         (partition 2 1)
         (drop-while #(not= (first %) (second %)))  ; До неподвижной точки
         first
         second
         (tap> "Reaching definitions converged"))))

(defn reaching-definitions-step [state cfg]
  "Один шаг анализа достижимых определений"
  (reduce (fn [acc block]
            (let [;; in[B] = ⋃ out[P] для всех предшественников P
                  in-set (apply set/union 
                               (map #(get-in acc [:out %]) (:predecessors block)))
                  ;; out[B] = gen[B] ⋃ (in[B] - kill[B])
                  out-set (set/union (:gen block)
                                    (set/difference in-set (:kill block)))]
              (-> acc
                  (assoc-in [:in (:label block)] in-set)
                  (assoc-in [:out (:label block)] out-set))))
          state
          (:blocks cfg)))

(defn initialize-reaching-definitions [cfg]
  "Инициализация для анализа достижимых определений"
  {:in (zipmap (map :label (:blocks cfg)) (repeat #{}))
   :out (zipmap (map :label (:blocks cfg)) (repeat #{}))})
```

---

## 🎯 Практические рекомендации

### **Выбор метода реализации**

| Критерий | `iterate` + `take-while` | `loop`/`recur` | Transducers |
|----------|-------------------------|----------------|-------------|
| **Читаемость** | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ | ⭐⭐ |
| **Производительность** | ⭐⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐⭐ |
| **Ленивость** | ⭐⭐⭐⭐⭐ | ❌ | ⭐⭐⭐ |
| **Контроль** | ⭐⭐ | ⭐⭐⭐⭐⭐ | ⭐⭐⭐ |

### **Рекомендации для AT89S4051**

```clojure
;; Для небольших CFG (< 100 блоков) - используйте iterate
(defn analyze-small-cfg [cfg]
  (->> cfg
       compute-liveness
       compute-dominators
       compute-reaching-definitions))

;; Для больших CFG - используйте loop/recur с ограничениями
(defn analyze-large-cfg [cfg]
  (let [max-iter 1000]
    {:liveness (compute-liveness-loop cfg max-iter)
     :dominators (compute-dominators-loop cfg max-iter)
     :reaching-defs (compute-reaching-definitions-loop cfg max-iter)}))
```

### **Оптимизации для микроконтроллера**

```clojure
;; Специализированная версия для AT89S4051
(defn compute-liveness-at89s4051 [cfg]
  "Оптимизированный анализ живости для AT89S4051"
  (let [;; Учитываем специфику архитектуры
        register-vars #{:A :R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7}
        memory-vars (get-memory-variables cfg)]
    
    (->> (initialize-liveness cfg)
         (iterate #(liveness-iteration-optimized % cfg register-vars))
         (take-while-converged)
         last
         (tap> "AT89S4051 liveness analysis complete"))))

(defn liveness-iteration-optimized [state cfg register-vars]
  "Оптимизированная итерация с учетом архитектуры"
  ;; Приоритет регистровым переменным
  ;; Специальная обработка аккумулятора A
  ;; Учет особенностей адресации R0/R1
  )
```

---

## 🔬 Сложность и производительность

### **Временная сложность**

- **Анализ живости**: O(E × V × I), где E - рёбра, V - переменные, I - итерации
- **Анализ доминаторов**: O(V³) в худшем случае, O(V × E) в среднем
- **Достижимые определения**: O(D × V × I), где D - определения

### **Пространственная сложность**

- **Состояние анализа**: O(V × B), где B - блоки
- **Промежуточные структуры**: O(E + V)

### **Критерии сходимости**

```clojure
(defn convergence-metrics [iterations]
  "Метрики сходимости для анализа"
  {:iterations (count iterations)
   :convergence-rate (convergence-rate iterations)
   :stability-factor (stability-factor iterations)})

(defn early-termination? [prev-state curr-state iteration]
  "Критерии досрочного завершения"
  (or (= prev-state curr-state)           ; Неподвижная точка
      (> iteration 1000)                  ; Лимит итераций
      (minimal-change? prev-state curr-state)))  ; Минимальные изменения
```

---

## ✅ Заключение

**Итеративные алгоритмы** - основа анализа потоков данных в компиляторах. Для компилятора AT89S4051:

1. **Используйте `iterate` + `take-while`** для читаемости и элегантности
2. **Применяйте `loop`/`recur`** для критичной производительности
3. **Добавляйте архитектурные оптимизации** для специфики микроконтроллера
4. **Контролируйте сходимость** через метрики и ограничения

**Статус**: ✅ ГОТОВО К РЕАЛИЗАЦИИ  
**Приоритет**: ВЫСОКИЙ (критично для оптимизации)  
**Сложность**: СРЕДНЯЯ (хорошо изученные алгоритмы) 