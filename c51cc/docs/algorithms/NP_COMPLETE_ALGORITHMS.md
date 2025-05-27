# NP-полные задачи: Оптимальное распределение регистров

## 🎯 Обзор

NP-полные задачи в компиляторах требуют экспоненциального времени для точного решения, поэтому используются приближенные алгоритмы и метаэвристики. Основные применения:

- **Оптимальное распределение регистров** - минимизация spill операций
- **Планирование инструкций** - максимизация параллелизма
- **Выбор инструкций** - минимизация стоимости кода
- **Размещение данных** - оптимизация доступа к памяти

---

## 🎯 Жадные эвристики для AT89S4051

### **Базовый жадный алгоритм**

```clojure
(defn greedy-register-allocation [interference-graph]
  "Жадное распределение регистров для AT89S4051"
  (let [at89s4051-registers [:A :R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7]
        ;; Сортируем узлы по эвристике spill cost
        nodes (sort-by-spill-cost interference-graph)]
    
    (->> nodes
         (reduce (fn [allocation node]
                   (let [forbidden-regs (get-conflicting-registers node allocation interference-graph)
                         available-regs (remove forbidden-regs at89s4051-registers)
                         chosen-reg (choose-best-register node available-regs)]
                     
                     (if chosen-reg
                       (do (tap> (str "Allocated " chosen-reg " to " node))
                           (assoc allocation node chosen-reg))
                       (do (tap> (str "Spilling " node " to memory"))
                           (assoc allocation node :memory)))))
                 {})
         (tap> "Greedy allocation complete"))))

(defn sort-by-spill-cost [interference-graph]
  "Сортирует узлы по стоимости spill (эвристика)"
  (->> interference-graph
       keys
       (sort-by (fn [node]
                  (let [degree (count (get interference-graph node))
                        usage-frequency (get-usage-frequency node)
                        loop-depth (get-loop-depth node)]
                    ;; Приоритет: частота * глубина_цикла / (степень + 1)
                    (/ (* usage-frequency (inc loop-depth)) 
                       (inc degree)))))
       reverse))

(defn choose-best-register [node available-registers]
  "Выбирает лучший регистр для узла (эвристика для AT89S4051)"
  (let [node-type (classify-node-usage node)]
    (cond
      ;; Аккумулятор A - приоритет для арифметических операций
      (and (= node-type :arithmetic) 
           (some #{:A} available-registers))
      :A
      
      ;; R0, R1 - приоритет для указателей
      (and (= node-type :pointer) 
           (some #{:R0 :R1} available-registers))
      (first (filter #{:R0 :R1} available-registers))
      
      ;; R2-R4 - для счетчиков циклов
      (and (= node-type :loop-counter)
           (some #{:R2 :R3 :R4} available-registers))
      (first (filter #{:R2 :R3 :R4} available-registers))
      
      ;; Любой доступный регистр
      :else
      (first available-registers))))

(defn classify-node-usage [node]
  "Классифицирует использование узла"
  (let [operations (get-node-operations node)]
    (cond
      (some arithmetic-op? operations) :arithmetic
      (some pointer-op? operations) :pointer
      (some loop-op? operations) :loop-counter
      (short-live-range? node) :temporary
      :else :general)))
```

### **Улучшенный жадный алгоритм с look-ahead**

```clojure
(defn greedy-allocation-with-lookahead [interference-graph]
  "Жадный алгоритм с предварительным анализом"
  (let [registers [:A :R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7]
        nodes (prioritize-nodes-advanced interference-graph)]
    
    (loop [remaining-nodes nodes
           allocation {}
           spill-candidates #{}]
      
      (if (empty? remaining-nodes)
        {:allocation allocation :spilled spill-candidates}
        
        (let [node (first remaining-nodes)
              rest-nodes (rest remaining-nodes)
              forbidden (get-conflicting-registers node allocation interference-graph)
              available (remove forbidden registers)]
          
          (if (empty? available)
            ;; Нет доступных регистров - добавляем в spill
            (recur rest-nodes allocation (conj spill-candidates node))
            
            ;; Выбираем лучший регистр с учетом будущих конфликтов
            (let [best-reg (choose-register-with-lookahead 
                           node available rest-nodes interference-graph)]
              (recur rest-nodes 
                     (assoc allocation node best-reg)
                     spill-candidates))))))))

(defn choose-register-with-lookahead [node available-regs future-nodes interference-graph]
  "Выбор регистра с учетом будущих конфликтов"
  (->> available-regs
       (map (fn [reg]
              [reg (count-future-conflicts reg node future-nodes interference-graph)]))
       (sort-by second)  ; Сортируем по количеству будущих конфликтов
       first
       first))

(defn count-future-conflicts [register node future-nodes interference-graph]
  "Подсчитывает будущие конфликты при выборе регистра"
  (->> future-nodes
       (filter #(contains? (get interference-graph node #{}) %))
       (map #(estimate-register-preference % register))
       (reduce +)))

(defn prioritize-nodes-advanced [interference-graph]
  "Продвинутая приоритизация узлов"
  (->> (keys interference-graph)
       (map (fn [node]
              [node (compute-priority-score node interference-graph)]))
       (sort-by second >)
       (map first)))

(defn compute-priority-score [node interference-graph]
  "Вычисляет приоритетный счет для узла"
  (let [degree (count (get interference-graph node))
        frequency (get-usage-frequency node)
        loop-depth (get-loop-depth node)
        arch-importance (get-architectural-importance node)]
    
    ;; Комбинированная метрика
    (+ (* frequency 10)
       (* loop-depth 5)
       (* arch-importance 3)
       (- 20 degree))))  ; Меньше степень = выше приоритет
```

---

## 🧬 Генетические алгоритмы

### **Базовый генетический алгоритм**

```clojure
(defn genetic-register-allocation [interference-graph]
  "Генетический алгоритм для распределения регистров"
  (let [population-size 50
        generations 100
        mutation-rate 0.1
        crossover-rate 0.8]
    
    (->> (generate-initial-population interference-graph population-size)
         (iterate (partial evolve-population interference-graph 
                          mutation-rate crossover-rate))
         (take generations)
         (map best-individual)
         (apply max-key fitness)
         (tap> "Best solution found by GA"))))

(defn generate-initial-population [interference-graph size]
  "Генерирует начальную популяцию решений"
  (repeatedly size #(random-allocation interference-graph)))

(defn random-allocation [interference-graph]
  "Создает случайное распределение регистров"
  (let [nodes (keys interference-graph)
        registers [:A :R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7 :memory]]
    (zipmap nodes (repeatedly #(rand-nth registers)))))

(defn evolve-population [interference-graph mutation-rate crossover-rate population]
  "Одно поколение эволюции"
  (let [;; Селекция лучших особей
        selected (tournament-selection population 0.7)
        ;; Скрещивание
        offspring (crossover-population selected crossover-rate)
        ;; Мутация
        mutated (mutate-population offspring mutation-rate)]
    
    ;; Элитизм: сохраняем лучших из предыдущего поколения
    (concat (take (/ (count population) 10) 
                  (sort-by fitness > population))
            mutated)))

(defn tournament-selection [population tournament-size]
  "Турнирная селекция"
  (repeatedly (count population)
              #(let [tournament (take tournament-size (shuffle population))]
                 (apply max-key fitness tournament))))

(defn crossover-population [population crossover-rate]
  "Скрещивание популяции"
  (->> population
       (partition 2)
       (mapcat (fn [[parent1 parent2]]
                 (if (< (rand) crossover-rate)
                   (crossover parent1 parent2)
                   [parent1 parent2])))))

(defn crossover [parent1 parent2]
  "Одноточечное скрещивание"
  (let [nodes (keys parent1)
        crossover-point (rand-int (count nodes))
        [left right] (split-at crossover-point nodes)]
    
    [(merge (select-keys parent1 left) (select-keys parent2 right))
     (merge (select-keys parent2 left) (select-keys parent1 right))]))

(defn mutate-population [population mutation-rate]
  "Мутация популяции"
  (map #(mutate % mutation-rate) population))

(defn mutate [individual mutation-rate]
  "Мутация особи"
  (let [registers [:A :R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7 :memory]]
    (reduce (fn [acc [node reg]]
              (if (< (rand) mutation-rate)
                (assoc acc node (rand-nth registers))
                acc))
            individual
            individual)))

(defn fitness [allocation]
  "Функция приспособленности"
  (let [spill-cost (* 100 (count (filter #(= % :memory) (vals allocation))))
        conflict-cost (* 50 (count-register-conflicts allocation))
        arch-bonus (architectural-fitness-bonus allocation)]
    
    (- 10000 spill-cost conflict-cost (- arch-bonus))))

(defn architectural-fitness-bonus [allocation]
  "Бонус за архитектурно-правильное использование"
  (reduce (fn [bonus [node register]]
            (+ bonus
               (cond
                 ;; Бонус за использование A для арифметики
                 (and (= register :A) (arithmetic-node? node)) 20
                 ;; Бонус за использование R0/R1 для указателей
                 (and (#{:R0 :R1} register) (pointer-node? node)) 15
                 ;; Штраф за неэффективное использование A
                 (and (= register :A) (not (arithmetic-node? node))) -10
                 :else 0)))
          0
          allocation))
```

### **Островная модель генетического алгоритма**

```clojure
(defn island-genetic-algorithm [interference-graph]
  "Островная модель ГА для повышения разнообразия"
  (let [num-islands 4
        island-size 25
        migration-rate 0.1
        migration-interval 10]
    
    (loop [islands (repeatedly num-islands 
                              #(generate-initial-population interference-graph island-size))
           generation 0]
      
      (if (>= generation 100)
        ;; Возвращаем лучшую особь со всех островов
        (->> islands
             (mapcat identity)
             (apply max-key fitness))
        
        (let [;; Эволюция на каждом острове
              evolved-islands (map #(evolve-population interference-graph 0.1 0.8 %) islands)
              ;; Миграция между островами
              migrated-islands (if (zero? (mod generation migration-interval))
                                (migrate-between-islands evolved-islands migration-rate)
                                evolved-islands)]
          
          (recur migrated-islands (inc generation)))))))

(defn migrate-between-islands [islands migration-rate]
  "Миграция лучших особей между островами"
  (let [migrants (map #(take (int (* (count %) migration-rate))
                            (sort-by fitness > %)) islands)]
    
    ;; Циклическая миграция: остров i получает мигрантов с острова (i-1)
    (map-indexed (fn [i island]
                   (let [incoming-migrants (nth migrants (mod (dec i) (count islands)))
                         remaining-population (drop (count incoming-migrants) 
                                                   (sort-by fitness > island))]
                     (concat incoming-migrants remaining-population)))
                 islands)))
```

---

## 🌡️ Simulated Annealing

### **Базовый алгоритм имитации отжига**

```clojure
(defn simulated-annealing-allocation [initial-allocation interference-graph]
  "Улучшение распределения через имитацию отжига"
  (let [initial-temp 100.0
        cooling-rate 0.95
        min-temp 0.1
        max-iterations 10000]
    
    (loop [current-allocation initial-allocation
           current-cost (allocation-cost current-allocation interference-graph)
           temperature initial-temp
           iteration 0
           best-allocation initial-allocation
           best-cost current-cost]
      
      (cond
        ;; Условия остановки
        (or (< temperature min-temp) (>= iteration max-iterations))
        (do (tap> (str "Annealing complete. Best cost: " best-cost))
            best-allocation)
        
        :else
        (let [neighbor (generate-neighbor current-allocation)
              neighbor-cost (allocation-cost neighbor interference-graph)
              delta (- neighbor-cost current-cost)
              accept? (or (< delta 0)  ; Улучшение
                         (< (rand) (Math/exp (/ (- delta) temperature))))]
          
          (if accept?
            ;; Принимаем соседа
            (let [new-best-allocation (if (< neighbor-cost best-cost)
                                       neighbor
                                       best-allocation)
                  new-best-cost (min neighbor-cost best-cost)]
              (recur neighbor neighbor-cost 
                     (* temperature cooling-rate) 
                     (inc iteration)
                     new-best-allocation
                     new-best-cost))
            
            ;; Отклоняем соседа
            (recur current-allocation current-cost 
                   (* temperature cooling-rate) 
                   (inc iteration)
                   best-allocation
                   best-cost)))))))

(defn generate-neighbor [allocation]
  "Генерирует соседнее решение (малое изменение)"
  (let [nodes (keys allocation)
        registers [:A :R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7 :memory]
        ;; Выбираем случайный узел для изменения
        node (rand-nth nodes)
        ;; Выбираем новый регистр
        new-register (rand-nth registers)]
    
    (assoc allocation node new-register)))

(defn allocation-cost [allocation interference-graph]
  "Вычисляет стоимость распределения"
  (let [spill-cost (* 100 (count (filter #(= % :memory) (vals allocation))))
        conflict-cost (* 1000 (count-allocation-conflicts allocation interference-graph))
        inefficiency-cost (compute-inefficiency-cost allocation)]
    
    (+ spill-cost conflict-cost inefficiency-cost)))

(defn count-allocation-conflicts [allocation interference-graph]
  "Подсчитывает конфликты в распределении"
  (->> interference-graph
       (mapcat (fn [[node neighbors]]
                 (let [node-reg (get allocation node)]
                   (map (fn [neighbor]
                          (when (= node-reg (get allocation neighbor))
                            1))
                        neighbors))))
       (remove nil?)
       count
       (/ 2)))  ; Каждый конфликт считается дважды

(defn compute-inefficiency-cost [allocation]
  "Вычисляет стоимость неэффективного использования регистров"
  (reduce (fn [cost [node register]]
            (+ cost
               (cond
                 ;; Штраф за неиспользование A для арифметики
                 (and (arithmetic-node? node) (not= register :A)) 10
                 ;; Штраф за неиспользование R0/R1 для указателей
                 (and (pointer-node? node) (not (#{:R0 :R1} register))) 5
                 ;; Штраф за использование A не для арифметики
                 (and (= register :A) (not (arithmetic-node? node))) 15
                 :else 0)))
          0
          allocation))
```

### **Адаптивный Simulated Annealing**

```clojure
(defn adaptive-simulated-annealing [initial-allocation interference-graph]
  "Адаптивная имитация отжига с динамическими параметрами"
  (let [initial-temp 100.0
        min-temp 0.1]
    
    (loop [current-allocation initial-allocation
           current-cost (allocation-cost current-allocation interference-graph)
           temperature initial-temp
           iteration 0
           acceptance-rate 0.5
           last-improvement 0]
      
      (cond
        (< temperature min-temp)
        current-allocation
        
        ;; Адаптация параметров
        (zero? (mod iteration 100))
        (let [new-cooling-rate (adapt-cooling-rate acceptance-rate)
              new-temp (* temperature new-cooling-rate)]
          (recur current-allocation current-cost new-temp 
                 iteration acceptance-rate last-improvement))
        
        :else
        (let [neighbor (generate-smart-neighbor current-allocation iteration)
              neighbor-cost (allocation-cost neighbor interference-graph)
              delta (- neighbor-cost current-cost)
              accept-prob (if (< delta 0) 1.0 (Math/exp (/ (- delta) temperature)))
              accept? (< (rand) accept-prob)]
          
          (if accept?
            (recur neighbor neighbor-cost 
                   (* temperature 0.99) 
                   (inc iteration)
                   (update-acceptance-rate acceptance-rate true)
                   (if (< neighbor-cost current-cost) iteration last-improvement))
            
            (recur current-allocation current-cost 
                   (* temperature 0.99) 
                   (inc iteration)
                   (update-acceptance-rate acceptance-rate false)
                   last-improvement)))))))

(defn generate-smart-neighbor [allocation iteration]
  "Генерирует умного соседа в зависимости от стадии поиска"
  (if (< iteration 1000)
    ;; Ранняя стадия: большие изменения
    (generate-multi-node-neighbor allocation 3)
    ;; Поздняя стадия: малые изменения
    (generate-neighbor allocation)))

(defn generate-multi-node-neighbor [allocation num-changes]
  "Генерирует соседа с изменением нескольких узлов"
  (let [nodes (keys allocation)
        registers [:A :R0 :R1 :R2 :R3 :R4 :R5 :R6 :R7 :memory]
        nodes-to-change (take num-changes (shuffle nodes))]
    
    (reduce (fn [acc node]
              (assoc acc node (rand-nth registers)))
            allocation
            nodes-to-change)))

(defn adapt-cooling-rate [acceptance-rate]
  "Адаптирует скорость охлаждения"
  (cond
    (> acceptance-rate 0.8) 0.99  ; Медленное охлаждение
    (< acceptance-rate 0.2) 0.90  ; Быстрое охлаждение
    :else 0.95))                  ; Стандартное охлаждение

(defn update-acceptance-rate [current-rate accepted?]
  "Обновляет скорость принятия решений"
  (let [alpha 0.1]  ; Коэффициент обучения
    (+ (* (- 1 alpha) current-rate)
       (* alpha (if accepted? 1.0 0.0)))))
```

---

## 🎯 Специфичные архитектурные эвристики

### **Эвристики для AT89S4051**

```clojure
(defn at89s4051-specific-allocation [interference-graph]
  "Специализированное распределение для AT89S4051"
  (let [;; Анализируем паттерны использования
        usage-patterns (analyze-usage-patterns interference-graph)
        ;; Классифицируем переменные
        variable-classes (classify-variables usage-patterns)
        ;; Строим план распределения
        allocation-plan (build-allocation-plan variable-classes)]
    
    (execute-allocation-plan allocation-plan interference-graph)))

(defn analyze-usage-patterns [interference-graph]
  "Анализирует паттерны использования переменных"
  (reduce (fn [acc node]
            (assoc acc node {
              :arithmetic-ops (count-arithmetic-operations node)
              :pointer-ops (count-pointer-operations node)
              :loop-usage (analyze-loop-usage node)
              :call-frequency (get-call-frequency node)
              :live-range (compute-live-range node)
              :interference-degree (count (get interference-graph node))}))
          {}
          (keys interference-graph)))

(defn classify-variables [usage-patterns]
  "Классифицирует переменные по приоритету для регистров"
  {:accumulator-candidates 
   (->> usage-patterns
        (filter (fn [[node pattern]]
                  (and (> (:arithmetic-ops pattern) 3)
                       (< (:interference-degree pattern) 4))))
        (map first)
        (sort-by #(get-in usage-patterns [% :arithmetic-ops]) >))
   
   :pointer-candidates
   (->> usage-patterns
        (filter (fn [[node pattern]]
                  (and (> (:pointer-ops pattern) 2)
                       (< (:live-range pattern) 10))))
        (map first)
        (sort-by #(get-in usage-patterns [% :pointer-ops]) >))
   
   :loop-counters
   (->> usage-patterns
        (filter (fn [[node pattern]]
                  (> (:loop-usage pattern) 5)))
        (map first)
        (sort-by #(get-in usage-patterns [% :loop-usage]) >))
   
   :temporaries
   (->> usage-patterns
        (filter (fn [[node pattern]]
                  (< (:live-range pattern) 3)))
        (map first))})

(defn build-allocation-plan [variable-classes]
  "Строит план распределения регистров"
  {:phase-1 {:register :A 
             :candidates (:accumulator-candidates variable-classes)
             :max-allocations 1}
   
   :phase-2 {:registers [:R0 :R1]
             :candidates (:pointer-candidates variable-classes)
             :max-allocations 2}
   
   :phase-3 {:registers [:R2 :R3 :R4]
             :candidates (:loop-counters variable-classes)
             :max-allocations 3}
   
   :phase-4 {:registers [:R5 :R6 :R7]
             :candidates (:temporaries variable-classes)
             :max-allocations 3}})

(defn execute-allocation-plan [plan interference-graph]
  "Выполняет план распределения"
  (reduce (fn [allocation phase]
            (allocate-phase allocation phase interference-graph))
          {}
          (vals plan)))

(defn allocate-phase [current-allocation phase interference-graph]
  "Выполняет одну фазу распределения"
  (let [available-registers (if (vector? (:registers phase))
                             (:registers phase)
                             [(:register phase)])
        candidates (:candidates phase)
        max-allocs (:max-allocations phase)]
    
    (loop [allocation current-allocation
           remaining-candidates candidates
           remaining-registers available-registers
           allocated-count 0]
      
      (if (or (empty? remaining-candidates)
              (empty? remaining-registers)
              (>= allocated-count max-allocs))
        allocation
        
        (let [candidate (first remaining-candidates)
              register (first remaining-registers)]
          
          (if (can-allocate? candidate register allocation interference-graph)
            (recur (assoc allocation candidate register)
                   (rest remaining-candidates)
                   (rest remaining-registers)
                   (inc allocated-count))
            
            (recur allocation
                   (rest remaining-candidates)
                   remaining-registers
                   allocated-count)))))))

(defn can-allocate? [node register current-allocation interference-graph]
  "Проверяет, можно ли выделить регистр узлу"
  (let [neighbors (get interference-graph node #{})
        conflicting-neighbors (filter #(= (get current-allocation %) register) neighbors)]
    (empty? conflicting-neighbors)))
```

### **Архитектурные метрики качества**

```clojure
(defn evaluate-at89s4051-allocation [allocation]
  "Оценивает качество распределения для AT89S4051"
  {:accumulator-efficiency (accumulator-usage-efficiency allocation)
   :pointer-efficiency (pointer-register-efficiency allocation)
   :register-utilization (compute-register-utilization allocation)
   :spill-ratio (compute-spill-ratio allocation)
   :instruction-cost (estimate-instruction-cost allocation)})

(defn accumulator-usage-efficiency [allocation]
  "Эффективность использования аккумулятора"
  (let [a-allocated-nodes (filter #(= (get allocation %) :A) (keys allocation))
        arithmetic-nodes (filter arithmetic-node? a-allocated-nodes)
        efficiency (if (empty? a-allocated-nodes)
                    0.0
                    (/ (count arithmetic-nodes) (count a-allocated-nodes)))]
    (* efficiency 100)))

(defn pointer-register-efficiency [allocation]
  "Эффективность использования регистров указателей"
  (let [pointer-regs #{:R0 :R1}
        pointer-allocated (filter #(pointer-regs (get allocation %)) (keys allocation))
        actual-pointer-nodes (filter pointer-node? pointer-allocated)
        efficiency (if (empty? pointer-allocated)
                    0.0
                    (/ (count actual-pointer-nodes) (count pointer-allocated)))]
    (* efficiency 100)))

(defn estimate-instruction-cost [allocation]
  "Оценивает стоимость инструкций для данного распределения"
  (reduce (fn [cost [node register]]
            (+ cost
               (cond
                 (= register :memory) 50      ; Высокая стоимость spill
                 (= register :A) 1            ; Низкая стоимость для A
                 (#{:R0 :R1} register) 2      ; Средняя для R0/R1
                 :else 3)))                   ; Стандартная для остальных
          0
          allocation))
```

---

## 📊 Сравнительный анализ

### **Матрица производительности**

| Алгоритм | Время | Память | Качество | Масштабируемость |
|----------|-------|--------|----------|------------------|
| **Жадный** | O(n log n) | O(n) | 70-80% | Отличная |
| **Жадный + lookahead** | O(n²) | O(n) | 80-85% | Хорошая |
| **Генетический** | O(g × p × n) | O(p × n) | 85-95% | Средняя |
| **Simulated Annealing** | O(i × n) | O(n) | 80-90% | Хорошая |
| **Архитектурный** | O(n log n) | O(n) | 90-95% | Отличная |

Где: n - узлы, g - поколения, p - размер популяции, i - итерации

### **Рекомендации по выбору**

```clojure
(defn choose-allocation-algorithm [interference-graph constraints]
  "Выбирает оптимальный алгоритм распределения"
  (let [node-count (count (keys interference-graph))
        time-budget (:time-budget constraints)
        quality-requirement (:quality-requirement constraints)]
    
    (cond
      ;; Быстрая компиляция
      (< time-budget 100)
      :greedy-basic
      
      ;; Малые графы - можем позволить точные методы
      (< node-count 20)
      :genetic-algorithm
      
      ;; Средние графы - баланс качества и скорости
      (< node-count 100)
      :simulated-annealing
      
      ;; Большие графы - только эвристики
      :else
      :architectural-greedy)))

(defn hybrid-allocation-strategy [interference-graph]
  "Гибридная стратегия для максимального качества"
  (let [;; Начинаем с архитектурного жадного
        greedy-result (at89s4051-specific-allocation interference-graph)
        ;; Улучшаем через simulated annealing
        improved-result (simulated-annealing-allocation greedy-result interference-graph)
        ;; Финальная оптимизация через локальный поиск
        final-result (local-search-optimization improved-result interference-graph)]
    
    final-result))
```

---

## 📊 Блок-схемы алгоритмов

### **Общая схема решения NP-полных задач**

```
┌─────────────────┐
│  NP-полная      │
│  задача         │
│  (экспоненциальная│
│  сложность)     │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Выбор          │
│  стратегии:     │
└─────────┬───────┘
          │
    ┌─────┴─────┐
    │           │
    ▼           ▼
┌─────────┐ ┌─────────────┐
│Точное   │ │Приближенное │
│решение  │ │решение      │
│(малые   │ │(практичное) │
│размеры) │ │             │
└─────┬───┘ └─────┬───────┘
      │           │
      ▼           ▼
┌─────────┐ ┌─────────────┐
│Backtrack│ │Эвристики:   │
│Constraint│ │• Жадные     │
│Logic    │ │• Генетические│
└─────────┘ │• Simulated  │
            │  Annealing  │
            │• Локальный  │
            │  поиск      │
            └─────────────┘
```

### **Схема жадного алгоритма распределения регистров**

```
┌─────────────────────────────────────────────────────────────┐
│              ЖАДНОЕ РАСПРЕДЕЛЕНИЕ РЕГИСТРОВ                 │
└─────────────────────────────────────────────────────────────┘

┌─────────────────┐
│  Граф           │
│  интерференции  │
│  + доступные    │
│  регистры       │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Вычислить      │
│  приоритеты     │
│  переменных:    │
│  • Частота      │
│  • Глубина цикла│
│  • Степень      │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Сортировать    │
│  переменные     │
│  по убыванию    │
│  приоритета     │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Выбрать        │
│  следующую      │
│  переменную v   │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Все            │
│  переменные     │
│  обработаны?    │
└─────────┬───────┘
          │
    ┌─────┴─────┐
    │ Да        │ Нет
    ▼           ▼
┌───────────┐ ┌─────────────────┐
│ЗАВЕРШЕНИЕ │ │  Найти          │
│АЛГОРИТМА  │ │  конфликтующие  │
└───────────┘ │  регистры       │
              └─────────┬───────┘
                        │
                        ▼
              ┌─────────────────┐
              │  available =    │
              │  all_regs -     │
              │  conflicts      │
              └─────────┬───────┘
                        │
                        ▼
              ┌─────────────────┐
              │  available      │
              │  пустое?        │
              └─────────┬───────┘
                        │
                  ┌─────┴─────┐
                  │ Да        │ Нет
                  ▼           ▼
        ┌─────────────────┐ ┌─────────────────┐
        │  SPILL:         │ │  Выбрать        │
        │  allocation[v]  │ │  лучший         │
        │  = :memory      │ │  регистр        │
        └─────────────────┘ │  (эвристика)    │
                            └─────────┬───────┘
                                      │
                                      ▼
                            ┌─────────────────┐
                            │  allocation[v]  │
                            │  = best_reg     │
                            └─────────┬───────┘
                                      │
                                      ▼
                            ┌─────────────────┐
                            │  Следующая      │
                            │  переменная     │
                            └─────────┬───────┘
                                      │
                                      ▲─────────┘
```

### **Схема генетического алгоритма**

```
┌─────────────────────────────────────────────────────────────┐
│                 ГЕНЕТИЧЕСКИЙ АЛГОРИТМ                       │
└─────────────────────────────────────────────────────────────┘

┌─────────────────┐
│  Генерация      │
│  начальной      │
│  популяции      │
│  (случайные     │
│  решения)       │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Оценка         │
│  приспособлен-  │
│  ности каждой   │
│  особи          │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Достигнуто     │
│  максимальное   │
│  поколение?     │
└─────────┬───────┘
          │
    ┌─────┴─────┐
    │ Да        │ Нет
    ▼           ▼
┌───────────┐ ┌─────────────────┐
│ВОЗВРАТ    │ │  СЕЛЕКЦИЯ:      │
│ЛУЧШЕЙ     │ │  Турнирная      │
│ОСОБИ      │ │  селекция       │
└───────────┘ │  лучших особей  │
              └─────────┬───────┘
                        │
                        ▼
              ┌─────────────────┐
              │  СКРЕЩИВАНИЕ:   │
              │  Одноточечное   │
              │  или равномерное│
              │  скрещивание    │
              └─────────┬───────┘
                        │
                        ▼
              ┌─────────────────┐
              │  МУТАЦИЯ:       │
              │  Случайное      │
              │  изменение      │
              │  генов          │
              └─────────┬───────┘
                        │
                        ▼
              ┌─────────────────┐
              │  ЗАМЕЩЕНИЕ:     │
              │  Элитизм +      │
              │  новые особи    │
              └─────────┬───────┘
                        │
                        ▼
              ┌─────────────────┐
              │  Поколение + 1  │
              └─────────┬───────┘
                        │
                        ▲─────────┘

Операторы:
• Селекция: выбор родителей
• Скрещивание: создание потомков
• Мутация: внесение разнообразия
• Замещение: формирование нового поколения
```

### **Схема Simulated Annealing**

```
┌─────────────────────────────────────────────────────────────┐
│                  SIMULATED ANNEALING                        │
└─────────────────────────────────────────────────────────────┘

┌─────────────────┐
│  Начальное      │
│  решение        │
│  Начальная      │
│  температура T  │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Температура    │
│  T < T_min?     │
└─────────┬───────┘
          │
    ┌─────┴─────┐
    │ Да        │ Нет
    ▼           ▼
┌───────────┐ ┌─────────────────┐
│ВОЗВРАТ    │ │  Генерировать   │
│ЛУЧШЕГО    │ │  соседнее       │
│РЕШЕНИЯ    │ │  решение        │
└───────────┘ └─────────┬───────┘
                        │
                        ▼
              ┌─────────────────┐
              │  Вычислить      │
              │  ΔE = E_new -   │
              │  E_current      │
              └─────────┬───────┘
                        │
                        ▼
              ┌─────────────────┐
              │  ΔE < 0?        │
              │  (улучшение)    │
              └─────────┬───────┘
                        │
                  ┌─────┴─────┐
                  │ Да        │ Нет
                  ▼           ▼
        ┌─────────────────┐ ┌─────────────────┐
        │  ПРИНЯТЬ        │ │  Вычислить      │
        │  новое решение  │ │  вероятность    │
        └─────────┬───────┘ │  P = exp(-ΔE/T) │
                  │         └─────────┬───────┘
                  │                   │
                  │                   ▼
                  │         ┌─────────────────┐
                  │         │  random() < P?  │
                  │         └─────────┬───────┘
                  │                   │
                  │             ┌─────┴─────┐
                  │             │ Да        │ Нет
                  │             ▼           ▼
                  │   ┌─────────────────┐ ┌─────────────┐
                  │   │  ПРИНЯТЬ        │ │ОТКЛОНИТЬ    │
                  │   │  худшее решение │ │решение      │
                  │   └─────────┬───────┘ └─────┬───────┘
                  │             │               │
                  └─────────────┼───────────────┘
                                │
                                ▼
                      ┌─────────────────┐
                      │  Обновить       │
                      │  лучшее решение │
                      │  (если нужно)   │
                      └─────────┬───────┘
                                │
                                ▼
                      ┌─────────────────┐
                      │  Охладить:      │
                      │  T = T * α      │
                      │  (α < 1)        │
                      └─────────┬───────┘
                                │
                                ▲─────────┘
```

### **Схема архитектурной специализации для AT89S4051**

```
┌─────────────────────────────────────────────────────────────┐
│        АРХИТЕКТУРНАЯ СПЕЦИАЛИЗАЦИЯ AT89S4051                │
└─────────────────────────────────────────────────────────────┘

┌─────────────────┐
│  Анализ IR:     │
│  Классификация  │
│  операций и     │
│  переменных     │
└─────────┬───────┘
          │
          ▼
┌─────────────────────────────────────┐
│  Построение карты приоритетов:      │
│  • A: арифметические операции       │
│  • R0/R1: косвенная адресация       │
│  • R2-R4: счетчики циклов           │
│  • R5-R7: временные переменные      │
└─────────┬───────────────────────────┘
          │
          ▼
┌─────────────────┐
│  ФАЗА 1:        │
│  Распределение  │
│  аккумулятора A │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Найти          │
│  арифметические │
│  переменные     │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Выбрать        │
│  переменную с   │
│  макс. арифм.   │
│  операциями     │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Проверить      │
│  совместимость  │
│  с графом       │
│  интерференции  │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Можно          │
│  назначить A?   │
└─────────┬───────┘
          │
    ┌─────┴─────┐
    │ Да        │ Нет
    ▼           ▼
┌───────────┐ ┌─────────────┐
│A назначен │ │Переход к    │
│переменной │ │фазе 2       │
└─────┬─────┘ └─────┬───────┘
      │             │
      └─────────────┘
                    │
                    ▼
          ┌─────────────────┐
          │  ФАЗА 2:        │
          │  Распределение  │
          │  R0/R1          │
          └─────────┬───────┘
                    │
                    ▼
          ┌─────────────────┐
          │  Аналогично     │
          │  для указателей │
          └─────────┬───────┘
                    │
                    ▼
          ┌─────────────────┐
          │  ФАЗА 3:        │
          │  R2-R4 для      │
          │  счетчиков      │
          └─────────┬───────┘
                    │
                    ▼
          ┌─────────────────┐
          │  ФАЗА 4:        │
          │  R5-R7 для      │
          │  остальных      │
          └─────────┬───────┘
                    │
                    ▼
          ┌─────────────────┐
          │  Оценка         │
          │  качества:      │
          │  • Spill count  │
          │  • Arch efficiency│
          └─────────────────┘
```

### **Схема гибридного подхода**

```
┌─────────────────────────────────────────────────────────────┐
│                   ГИБРИДНЫЙ ПОДХОД                          │
└─────────────────────────────────────────────────────────────┘

┌─────────────────┐
│  Начальное      │
│  решение:       │
│  Жадный         │
│  алгоритм       │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Оценка         │
│  качества       │
│  решения        │
└─────────┬───────┘
          │
          ▼
┌─────────────────┐
│  Качество       │
│  достаточно?    │
└─────────┬───────┘
          │
    ┌─────┴─────┐
    │ Да        │ Нет
    ▼           ▼
┌───────────┐ ┌─────────────────┐
│ВОЗВРАТ    │ │  Выбор          │
│РЕШЕНИЯ    │ │  метаэвристики: │
└───────────┘ └─────────┬───────┘
                        │
                  ┌─────┴─────┐
                  │           │
                  ▼           ▼
        ┌─────────────┐ ┌─────────────┐
        │Simulated    │ │Генетический │
        │Annealing    │ │алгоритм     │
        │для малых    │ │для больших  │
        │изменений    │ │изменений    │
        └─────┬───────┘ └─────┬───────┘
              │               │
              └───────┬───────┘
                      │
                      ▼
            ┌─────────────────┐
            │  Локальный      │
            │  поиск для      │
            │  финальной      │
            │  оптимизации    │
            └─────────┬───────┘
                      │
                      ▼
            ┌─────────────────┐
            │  Валидация      │
            │  и оценка       │
            │  итогового      │
            │  решения        │
            └─────────────────┘
```

---

## ✅ Заключение

**NP-полные задачи** в компиляторах требуют сбалансированного подхода между качеством решения и временем выполнения. Для компилятора AT89S4051:

### **Ключевые принципы:**

1. **Используйте жадные эвристики** как базовую стратегию
2. **Применяйте метаэвристики** для улучшения качества
3. **Интегрируйте архитектурные знания** на всех уровнях
4. **Комбинируйте подходы** для достижения оптимального баланса

### **Практические рекомендации:**

- **Для прототипирования**: Жадный алгоритм
- **Для продакшена**: Архитектурный жадный + Simulated Annealing
- **Для исследований**: Генетический алгоритм + островная модель

**Статус**: ✅ ГОТОВО К РЕАЛИЗАЦИИ  
**Приоритет**: КРИТИЧЕСКИЙ (определяет качество кода)  
**Сложность**: ОЧЕНЬ ВЫСОКАЯ (требует экспертизы в оптимизации) 