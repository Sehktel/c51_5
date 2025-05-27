# Архитектура препроцессора C51 v2.0

## Обзор

Препроцессор C51 v2.0 представляет собой полностью рефакторированную версию оригинального препроцессора, построенную на основе современных функциональных принципов Clojure с использованием **Transducers** и **Protocols**.

## Ключевые архитектурные принципы

### 1. Функциональная архитектура
- **Неизменяемые данные**: Все состояние представлено неизменяемыми структурами данных
- **Чистые функции**: Отсутствие побочных эффектов, предсказуемое поведение
- **Композиция**: Сложная функциональность строится из простых, композируемых компонентов

### 2. Protocols для полиморфизма
```clojure
(defprotocol PreprocessorDirective
  "Базовый протокол для всех директив препроцессора"
  (matches? [this line] "Проверяет соответствие строки директиве")
  (process [this line state] "Обрабатывает директиву"))

(defprotocol ConditionalDirective
  "Протокол для условных директив"
  (evaluate-condition [this condition state] "Вычисляет условие"))

(defprotocol MacroProcessor
  "Протокол для работы с макросами"
  (expand-macro [this macro-name args state] "Расширяет макрос")
  (define-macro [this name definition state] "Определяет макрос")
  (undefine-macro [this name state] "Удаляет макрос"))
```

### 3. Transducers для эффективной обработки
```clojure
;; Композиция transducers для обработки кода
(def processing-xf 
  (comp
    (remove-comments-xf)
    (normalize-whitespace-xf)
    (preprocessor-xf initial-state)))
```

## Структура компонентов

### Базовые типы данных

#### PreprocessorState
```clojure
(defrecord PreprocessorState 
  [defines           ; Map макросов
   include-stack     ; Стек включаемых файлов
   include-paths     ; Пути поиска заголовков
   line-number       ; Текущий номер строки
   current-file      ; Текущий файл
   errors            ; Собранные ошибки
   conditional-stack ; Стек условной компиляции
   ])
```

#### ProcessingResult
```clojure
(defrecord ProcessingResult 
  [content  ; Обработанное содержимое
   state    ; Финальное состояние
   ])
```

### Реализации директив

Каждая директива препроцессора реализована как отдельный record, реализующий соответствующие протоколы:

#### DefineDirective
```clojure
(defrecord DefineDirective []
  PreprocessorDirective
  (matches? [_ line] 
    (re-find (:define directive-patterns) line))
  
  (process [_ line state]
    ;; Логика обработки #define
    )
  
  MacroProcessor
  (define-macro [_ name definition state]
    (assoc-in state [:defines name] definition)))
```

#### Условные директивы
- `IfdefDirective` - обработка `#ifdef`
- `IfndefDirective` - обработка `#ifndef`
- `ElseDirective` - обработка `#else`
- `EndifDirective` - обработка `#endif`

### Transducers для обработки текста

#### remove-comments-xf
Удаляет комментарии из кода:
```clojure
(defn remove-comments-xf []
  (map (fn [line]
         (-> line
             (str/replace #"//.*$" "")
             (str/replace #"/\*.*?\*/" "")))))
```

#### normalize-whitespace-xf
Нормализует пробельные символы:
```clojure
(defn normalize-whitespace-xf []
  (map (fn [line]
         (-> line
             (str/replace #"[ \t]+" " ")
             str/trim))))
```

#### preprocessor-xf
Основной transducer для обработки директив:
```clojure
(defn preprocessor-xf [initial-state]
  (fn [rf]
    (let [state (volatile! initial-state)]
      ;; Stateful transducer с изменяемым состоянием
      )))
```

## Валидация данных с clojure.spec

### Спецификации состояния
```clojure
(s/def ::macro-name string?)
(s/def ::macro-definition (s/or :simple string? :function map?))
(s/def ::defines (s/map-of ::macro-name ::macro-definition))
(s/def ::preprocessor-state
  (s/keys :req-un [::defines ::include-stack ::include-paths 
                   ::line-number ::current-file ::errors]))
```

### Валидация при создании состояния
```clojure
(defn create-initial-state [& options]
  (let [state (->PreprocessorState ...)]
    (when-not (s/valid? ::preprocessor-state state)
      (throw (ex-info "Некорректное состояние" 
                     {:spec-explain (s/explain-data ::preprocessor-state state)})))
    state))
```

## Обработка ошибок

### Функциональный подход к ошибкам
```clojure
(defn add-error [state error-message & [error-data]]
  (let [error-info (merge {:message error-message
                          :line (:line-number state)
                          :file (:current-file state)
                          :timestamp (System/currentTimeMillis)}
                         error-data)]
    (update state :errors conj error-info)))
```

### Типы ошибок
- `:validation` - ошибки валидации данных
- `:syntax` - синтаксические ошибки директив
- `:semantic` - семантические ошибки (например, #else без #if)
- `:critical` - критические ошибки системы

## Расширение макросов

### Алгоритм расширения
1. **Поиск макросов**: Использование регулярных выражений для поиска имен макросов
2. **Рекурсивное расширение**: Многократное применение замен до стабилизации
3. **Предотвращение циклов**: Ограничение количества итераций
4. **Предопределенные макросы**: Специальная обработка `__LINE__`, `__FILE__` и др.

```clojure
(defn expand-macros-in-text [text defines state]
  (loop [result text iterations 0]
    (if (>= iterations MAX_MACRO_EXPANSION_ITERATIONS)
      result
      (let [new-result (reduce-kv expand-single-macro result all-macros)]
        (if (= new-result result)
          result  ; Стабилизация достигнута
          (recur new-result (inc iterations)))))))
```

## Производительность

### Преимущества Transducers
1. **Ленивые вычисления**: Обработка по требованию
2. **Отсутствие промежуточных коллекций**: Прямая трансформация данных
3. **Композиция**: Эффективное объединение операций

### Оптимизации
- **Компиляция регулярных выражений**: Однократная компиляция паттернов
- **Volatile состояние**: Эффективное изменение состояния в transducers
- **Ранний выход**: Прекращение обработки при критических ошибках

## Тестирование

### Уровни тестирования

#### 1. Unit-тесты
- Тестирование отдельных директив
- Валидация функций парсинга
- Проверка transducers

#### 2. Integration-тесты
- Полный цикл обработки кода
- Взаимодействие директив
- Обработка сложных случаев

#### 3. Property-based тесты
- Идемпотентность обработки
- Неизменяемость состояния
- Корректность композиции

#### 4. Performance-тесты
- Обработка больших файлов
- Сложные макросы
- Глубокая вложенность включений

### Примеры тестов
```clojure
(deftest test-define-directive
  (testing "Обработка директивы #define"
    (let [directive (->DefineDirective)
          state (create-initial-state)
          [result new-state] (process directive "#define MAX_SIZE 100" state)]
      (is (nil? result))
      (is (= "100" (get-in new-state [:defines "MAX_SIZE"]))))))
```

## Расширяемость

### Добавление новых директив
1. Создать record, реализующий `PreprocessorDirective`
2. Добавить в `directive-registry`
3. Написать тесты

```clojure
(defrecord NewDirective []
  PreprocessorDirective
  (matches? [_ line] ...)
  (process [_ line state] ...))

;; Добавление в реестр
(def directive-registry
  [(->DefineDirective)
   (->NewDirective)  ; Новая директива
   ...])
```

### Добавление новых transducers
```clojure
(defn custom-processing-xf []
  (map custom-transformation))

;; Композиция с существующими
(def extended-processing-xf
  (comp
    (remove-comments-xf)
    (custom-processing-xf)  ; Новый transducer
    (normalize-whitespace-xf)
    (preprocessor-xf initial-state)))
```

## Совместимость

### Обратная совместимость
- Сохранение публичного API
- Поддержка всех директив C89
- Идентичное поведение для корректного кода

### Миграция с v1
```clojure
;; Старый API
(preprocess code options)

;; Новый API (совместимый)
(preprocess-v2 code options)
```

## Заключение

Рефакторированный препроцессор C51 v2.0 представляет собой современную, функциональную архитектуру, которая:

1. **Улучшает производительность** за счет использования transducers
2. **Повышает тестируемость** благодаря модульной структуре
3. **Обеспечивает расширяемость** через protocols и композицию
4. **Гарантирует корректность** с помощью clojure.spec
5. **Сохраняет совместимость** с существующим кодом

Эта архитектура служит основой для дальнейшего развития препроцессора и может быть легко адаптирована для новых требований. 