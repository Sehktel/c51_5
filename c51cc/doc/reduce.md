# 🧩 Функция `reduce` в Clojure: Глубокое погружение

## 🔍 Структура `reduce`

```clojure
(reduce 
 accumulator-fn     ; Функция аккумуляции
 initial-value      ; Начальное значение
 collection)        ; Коллекция для обработки
```

## 🚀 Разбор конкретного примера FSM

```clojure
(reduce 
 (fn [current-state next-stage]
   ;; Логика обработки состояний FSM
   (let [stage-config (get-in fsm [:states current-state])
         valid-transition 
         (first 
          (filter 
           #((:validation %) item) 
           (:transitions stage-config)))]
     (if valid-transition
       (:next-state valid-transition)
       (throw (ex-info "Не прошел проверку" 
                        {:item item 
                         :stage current-state})))))
 (:initial-state fsm)   ; Начальное состояние
 (keys (:states fsm)))  ; Коллекция для итерации
```

## 🧩 Пошаговое объяснение

### 1. Начальное состояние
- `(:initial-state fsm)` - стартовое состояние конечного автомата
- В примере: `:raw-material`

### 2. Коллекция для итерации
- `(keys (:states fsm))` - ключи состояний
- Пример: `[:raw-material :processing :quality-check]`

### 3. Функция аккумуляции
```clojure
(fn [current-state next-stage]
  ;; Логика перехода между состояниями
  )
```

## 💡 Принцип работы

```
Итерация 1:
  current-state = :raw-material
  next-stage = :processing
  Результат: новое состояние

Итерация 2:
  current-state = новое состояние из первой итерации
  next-stage = следующее состояние
  Результат: еще одно новое состояние
```

## 🎓 Образовательная аналогия

`reduce` как конвейер:
- Каждый элемент проходит обработку
- На каждом этапе может измениться состояние
- Финальный результат - последнее состояние

## 📝 Простые примеры

### Суммирование
```clojure
(reduce 
 (fn [acc x] (+ acc x))  ; Функция аккумуляции
 0                       ; Начальное значение
 [1 2 3 4 5])            ; Коллекция
;; Результат: 15
```

### Умножение с выводом
```clojure
(reduce 
 (fn [acc x]
   (let [new-acc (* acc x)]
     (println "Текущий acc:" new-acc)
     new-acc))
 1 
 [2 3 4 5])
```

## 🔬 Особенности в контексте FSM

`reduce` позволяет:
- Аккумулировать состояние конечного автомата
- Проверять возможность перехода
- Генерировать исключения при невалидных переходах

## 💭 Философия подхода

`reduce` демонстрирует:
- Иммутабельность
- Функциональную композицию
- Декларативный стиль программирования

## 🎲 Практические упражнения

### Упражнение 1: Накопление
```clojure
;; Найти максимальное значение
(reduce 
 (fn [max-val current]
   (max max-val current))
 0
 [5 2 10 1 7])
;; Результат: 10
```

### Упражнение 2: Преобразование коллекции
```clojure
;; Преобразование в map
(reduce 
 (fn [result item]
   (assoc result 
          (keyword (str "key-" item)) 
          (* item 2)))
 {}
 [1 2 3 4])
;; Результат: {:key-1 2, :key-2 4, :key-3 6, :key-4 8}
```

## 🤔 Контрольные вопросы

1. Что такое `reduce`?
2. Из чего состоит вызов `reduce`?
3. Как работает функция аккумуляции?
4. В чем преимущества `reduce`?

## 📚 Дополнительные ресурсы

- [Clojure Docs: Reduce](https://clojuredocs.org/clojure.core/reduce)
- [Functional Programming in Clojure](https://www.braveclojure.com/)

---

**Версия документа**: 1.0.0
**Дата создания**: [Текущая дата] 