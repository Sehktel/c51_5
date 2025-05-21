# 🔬 Гибридный подход FSM + Spec в Clojure

## 🎓 Методика объяснения гибридного подхода FSM + Spec

### Уровень 1: Базовое введение

Аналогия: 
- FSM (Конечный автомат) - как конвейер на заводе
- Spec - как контролер качества на каждом этапе

```clojure
(def factory-fsm 
  {:stages 
   [:raw-material   ; Входящее сырье
    :processing     ; Обработка
    :quality-check  ; Проверка
    :packaging      ; Упаковка
    :shipping]})    ; Отправка
```

### Уровень 2: Интеграция проверок

```clojure
(ns learning.hybrid-parsing)

;; 1. Спецификации - "паспорт" объекта
(s/def ::raw-material 
  (s/and 
   vector?                ; Должен быть вектором
   #(every? number? %)    ; Содержать только числа
   #(> (count %) 2)))     ; Больше 2 элементов

(s/def ::processed-item 
  (s/keys :req-un [::weight ::quality]))

;; 2. FSM с встроенными проверками
(def production-fsm 
  {:initial-state :raw-material
   :states 
   {:raw-material 
    {:transitions 
     [{:next-state :processing
       ;; Встраивание проверки
       :validation 
       (fn [material]
         (s/valid? ::raw-material material))}]}
    
    :processing 
    {:transitions 
     [{:next-state :quality-check
       :validation 
       (fn [item]
         (s/valid? ::processed-item item))}]}
    
    :quality-check 
    {:transitions 
     [{:next-state :packaging
       :validation 
       (fn [item]
         (and 
          (> (:quality item) 0.7)  ; Своя логика
          (s/valid? ::processed-item item)))}]}}})

;; 3. Функция прохождения по FSM
(defn process-item [item]
  (let [fsm production-fsm]
    (reduce 
     (fn [current-state next-stage]
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
     (:initial-state fsm)
     (keys (:states fsm)))))
```

## 🔍 Методика объяснения студентам

### Этап 1: Базовая концепция
1. FSM - это последовательность состояний
2. Spec - это "паспорт" для данных
3. Гибридный подход = FSM + встроенные проверки

### Этап 2: Практическая демонстрация

```clojure
;; Пример для понимания
(def sample-material [1 2 3])
(def processed-item 
  {:weight 10 
   :quality 0.8})

;; Проверка прохождения
(process-item sample-material)
;; => :processing или исключение
```

## 🚀 Где и как встраивать проверки

### 1. В переходах между состояниями
- `:validation` функция
- Возвращает `true/false`
- Решает, можно ли перейти в следующее состояние

### 2. Типы проверок
```clojure
;; Проверка структуры
(s/valid? ::spec-name data)

;; Сложная логика
(and 
 (s/valid? ::spec-name data)
 (custom-check data))
```

### 3. Уровни проверок
- Входящие данные
- Промежуточные состояния
- Финальный результат

## 💡 Педагогические советы

1. Рисуйте схемы состояний
2. Используйте аналогии (конвейер, завод)
3. Показывайте реальные примеры
4. Демонстрируйте эволюцию подхода

## 🎲 Практическое задание для студентов

```clojure
;; Задание: Создать FSM для валидации пользователя
(s/def ::username string?)
(s/def ::age int?)
(s/def ::user 
  (s/keys :req-un [::username ::age]))

;; Homework: Реализовать FSM регистрации
;; - Проверка имени
;; - Проверка возраста
;; - Финальная валидация
```

## Контрольные вопросы
1. Зачем нужны спецификации?
2. Как устроен конечный автомат?
3. Где встраиваются проверки?
4. Какие преимущества гибридного подхода?

## 🌟 Резюме для студентов

Гибридный подход FSM + Spec - это:
- Гибкость состояний
- Строгость проверок
- Декларативность описания
- Расширяемость системы

---

**Версия документа**: 1.0.0
**Дата создания**: [Текущая дата] 