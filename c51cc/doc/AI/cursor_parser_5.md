# Проблемы с парсером и тестирование
_Exported on 5/27/2025 at 20:43:38 GMT+3 from Cursor (0.50.5)_

---

**User**

Вижу, что:
✅ Простые выражения работают
✅ Простые функции работают
❌ Функции с параметрами не работают
❌ C51 interrupt не работает

Разберись почему.
Используй дополнительные тесты.
Макрос do-parse линтеру не понятен, но он гигиенически верный.
Лексер возвращает вектор токенов, а не структуру с :success

---

**Cursor**

Я помогу разобраться с проблемами в вашем парсере. Сначала давайте изучим текущую структуру проекта и найдем проблемы.

Теперь давайте найдем основные файлы парсера:

Отлично! Теперь давайте посмотрим на тестовые файлы, чтобы понять, какие именно проблемы возникают:

Теперь давайте посмотрим на основные файлы парсера:

Теперь давайте посмотрим на парсер:

Давайте продолжим изучение парсера, особенно части, отвечающей за функции:

Теперь давайте посмотрим на функцию парсинга параметров:

Теперь давайте посмотрим на функцию парсинга C51 модификаторов:

Теперь давайте запустим тесты, чтобы увидеть конкретные ошибки:

Интересно! Простые функции работают, но есть проблема с парсингом переменных. Давайте запустим тест interrupt:

Отлично! Interrupt функции работают! Теперь давайте создадим более детальные тесты для выявления проблем:

```clojure
(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(defn parse-string [input]
  "Парсит строку кода и возвращает результат"
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn detailed-test [description code]
  "Детальный тест с полной диагностикой"
  (println (str "\n=== " description " ==="))
  (println (str "Код: " code))
  
  ;; Сначала проверяем токенизацию
  (let [tokens (lexer/tokenize code)]
    (println (str "Токены (" (count tokens) "): "))
    (doseq [[i token] (map-indexed vector tokens)]
      (println (str "  " i ": " (:type token) " = " (:value token))))
    
    ;; Затем парсинг
    (let [result (parser/parse tokens)]
      (println "\nРезультат парсинга:")
      (if (:success result)
        (do
          (println "✓ УСПЕХ")
          (let [ast (:ast result)]
            (println (str "AST тип: " (:ast-type ast)))
            (when (:declarations ast)
              (println (str "Объявлений: " (count (:declarations ast))))
              (doseq [[i decl] (map-indexed vector (:declarations ast))]
                (println (str "  " i ": " (:ast-type decl)))
                (when (= (:ast-type decl) :function-declaration)
                  (println (str "    Имя: " (:function-name decl)))
                  (println (str "    Параметры: " (count (:parameters decl))))
                  (doseq [[j param] (map-indexed vector (:parameters decl))]
                    (println (str "      " j ": " (:type param) " " (:name param)))))))))
        (do
          (println "✗ ОШИБКА")
          (println (str "Сообщение: " (:error result)))
          (println (str "Позиция: " (:position result)))
          (when (< (:position result) (count tokens))
            (println (str "Токен на позиции: " (nth tokens (:position result))))))))))

;; Тесты простых конструкций (должны работать)
(detailed-test "Простая функция без параметров" "void main() { }")
(detailed-test "Простое объявление переменной" "int x;")

;; Тесты функций с параметрами (проблемные)
(detailed-test "Функция с одним параметром" "int add(int a) { return a; }")
(detailed-test "Функция с двумя параметрами" "int add(int a, int b) { return a + b; }")
(detailed-test "Функция с void параметром" "void func(void) { }")

;; Тесты C51 interrupt (должны работать)
(detailed-test "Interrupt функция" "void timer_isr(void) interrupt 1 { }")
(detailed-test "Interrupt с using" "void ext_int(void) interrupt 0 using 2 { }")

;; Тесты объявлений функций
(detailed-test "Объявление функции" "int add(int a, int b);")
(detailed-test "Объявление void функции" "void func();")

;; Дополнительные тесты
(detailed-test "Функция с телом и операторами" "void test() { int x = 5; x = x + 1; }")
(detailed-test "Функция с return" "int getValue() { return 42; }")

(println "\n=== ЗАВЕРШЕНИЕ ТЕСТОВ ===")
```

Отлично! Теперь я вижу проблемы:

## Анализ проблем:

1. **✅ Простые функции работают** - `void main() { }`, `void func();`
2. **✅ C51 interrupt работает** - `void timer_isr(void) interrupt 1 { }`
3. **❌ Функции с параметрами НЕ работают** - проблема в парсинге типов как первого токена
4. **❌ Функции с возвращаемым типом int НЕ работают** - та же проблема

**Корень проблемы**: Парсер пытается парсить `int add(...)` как объявление переменной `int`, а не как функцию. Это происходит потому что в `parse-declaration` порядок выбора неправильный.

Давайте посмотрим на функцию `parse-declaration`:

Проблема найдена! В функции `parse-declaration` порядок парсеров неправильный. Парсер `parse-variable-declaration` пытается парсить `int add(...)` как переменную и частично успешно парсит `int`, но затем не может продолжить.

Нужно изменить логику, чтобы различать функции и переменные. Давайте посмотрим на `parse-variable-declaration`:

Теперь я понимаю проблему! Нужно создать более умную логику различения функций и переменных. Проблема в том, что и функции, и переменные начинаются с типа + идентификатора, но функции имеют скобки `(` после имени.

Давайте создадим исправленную версию:

```clojure
(defn parse-declaration 
  "Парсит любое объявление или оператор на верхнем уровне программы
   В C89/C90: объявления переменных внутри функций должны быть в начале,
   смешивание объявлений и операторов запрещено (введено в C99)
   Поддерживает C51-специфичные декларации: sfr, sbit
   ИСПРАВЛЕНО: Добавлена умная логика различения функций и переменных"
  [state]
  ((choice
    ;; C51-специфичные декларации (начинаются с уникальных ключевых слов)
    parse-sfr-declaration
    parse-sbit-declaration
    
    ;; Умное различение функций и переменных
    ;; Проверяем, есть ли после типа + идентификатора открывающая скобка
    (fn [state]
      (let [type-result (parse-type-specifier state)]
        (if (:success? type-result)
          (let [name-result ((expect-token :identifier) (:state type-result))]
            (if (:success? name-result)
              (let [next-token-result (current-token (:state name-result))]
                (if (and (:success? next-token-result)
                         (= (:type (:value next-token-result)) :bracket)
                         (= (:value (:value next-token-result)) :open-round))
                  ;; Это функция - есть открывающая скобка
                  (parse-function-declaration state)
                  ;; Это переменная - нет открывающей скобки
                  (parse-variable-declaration state)))
              ;; Не удалось парсить имя - пробуем другие варианты
              (failure "Ожидался идентификатор после типа" (:state type-result)))
          ;; Не удалось парсить тип - пробуем другие варианты
          (failure "Ожидался спецификатор типа" state))))
    
    ;; Добавляем поддержку операторов
    parse-statement
    
    ;; Добавляем поддержку выражений как деклараций (только с точкой с запятой!)
    (fn [state]
      (let [expr-result (parse-expression state)]
        (if (:success? expr-result)
          (let [semi-result ((expect-token-value :semicolon) (:state expr-result))]
            (if (:success? semi-result)
              (success (expression-statement-node (:value expr-result)) (:state semi-result))
              ;; ИСПРАВЛЕНИЕ: не принимаем выражения без точки с запятой
              semi-result))
          expr-result)))) state))
```

Есть проблема с синтаксисом. Давайте исправим:

Проблема в том, что у нас есть лишняя открывающая скобка. Давайте исправим это:

```clojure
(defn parse-declaration 
  "Парсит любое объявление или оператор на верхнем уровне программы
   В C89/C90: объявления переменных внутри функций должны быть в начале,
   смешивание объявлений и операторов запрещено (введено в C99)
   Поддерживает C51-специфичные декларации: sfr, sbit
   ИСПРАВЛЕНО: Добавлена умная логика различения функций и переменных"
  [state]
  ((choice
    ;; C51-специфичные декларации (начинаются с уникальных ключевых слов)
    parse-sfr-declaration
    parse-sbit-declaration
    
    ;; Умное различение функций и переменных
    ;; Проверяем, есть ли после типа + идентификатора открывающая скобка
    (fn [state]
      (let [type-result (parse-type-specifier state)]
        (if (:success? type-result)
          (let [name-result ((expect-token :identifier) (:state type-result))]
            (if (:success? name-result)
              (let [next-token-result (current-token (:state name-result))]
                (if (and (:success? next-token-result)
                         (= (:type (:value next-token-result)) :bracket)
                         (= (:value (:value next-token-result)) :open-round))
                  ;; Это функция - есть открывающая скобка
                  (parse-function-declaration state)
                  ;; Это переменная - нет открывающей скобки
                  (parse-variable-declaration state)))
              ;; Не удалось парсить имя - пробуем другие варианты
              (failure "Ожидался идентификатор после типа" (:state type-result)))
          ;; Не удалось парсить тип - пробуем другие варианты
          (failure "Ожидался спецификатор типа" state))))
    
    ;; Добавляем поддержку операторов
    parse-statement
    
    ;; Добавляем поддержку выражений как деклараций (только с точкой с запятой!)
    (fn [state]
      (let [expr-result (parse-expression state)]
        (if (:success? expr-result)
          (let [semi-result ((expect-token-value :semicolon) (:state expr-result))]
            (if (:success? semi-result)
              (success (expression-statement-node (:value expr-result)) (:state semi-result))
              ;; ИСПРАВЛЕНИЕ: не принимаем выражения без точки с запятой
              semi-result))
          expr-result)))) state))
```

Давайте проверим, где именно проблема со скобками:

---

**User**

вроде исправил, продолжай

---

**Cursor**

Я вижу проблему! Макрос `do-parse` не работает правильно - линтер показывает, что переменные не разрешаются. Это означает, что макрос не генерирует правильный код. 

Но сначала давайте протестируем наше исправление логики различения функций и переменных:

---

**User**

do-parse правильны! Т..к. это гигиенически верный код с защитой от переопределений переменных

---

**Cursor**

Вы абсолютно правы! Макрос `do-parse` гигиенически верный и защищает от захвата переменных. Линтер просто не понимает макросы, но код корректен.

Давайте протестируем наше исправление логики различения функций и переменных:

Проблема стала еще хуже! Теперь вообще ничего не работает. Проблема в том, что наша новая логика в `parse-declaration` возвращает `failure`, а комбинатор `choice` не может обработать это правильно.

Давайте исправим это, создав более умную логику:

```clojure
(defn parse-declaration 
  "Парсит любое объявление или оператор на верхнем уровне программы
   В C89/C90: объявления переменных внутри функций должны быть в начале,
   смешивание объявлений и операторов запрещено (введено в C99)
   Поддерживает C51-специфичные декларации: sfr, sbit
   ИСПРАВЛЕНО: Добавлена умная логика различения функций и переменных"
  [state]
  ((choice
    ;; C51-специфичные декларации (начинаются с уникальных ключевых слов)
    parse-sfr-declaration
    parse-sbit-declaration
    
    ;; Умное различение функций и переменных
    ;; Проверяем, есть ли после типа + идентификатора открывающая скобка
    (fn [state]
      ;; Сначала проверяем, можем ли мы парсить тип
      (let [type-result (parse-type-specifier state)]
        (if (:success? type-result)
          ;; Затем проверяем идентификатор
          (let [name-result ((expect-token :identifier) (:state type-result))]
            (if (:success? name-result)
              ;; Проверяем следующий токен без продвижения
              (let [next-token-result (current-token (:state name-result))]
                (if (and (:success? next-token-result)
                         (= (:type (:value next-token-result)) :bracket)
                         (= (:value (:value next-token-result)) :open-round))
                  ;; Это функция - есть открывающая скобка
                  (parse-function-declaration state)
                  ;; Это переменная - нет открывающей скобки
                  (parse-variable-declaration state)))
              ;; Не удалось парсить имя - это не наш случай
              (failure "Не удалось парсить как объявление с типом" state))
          ;; Не удалось парсить тип - это не наш случай
          (failure "Не удалось парсить спецификатор типа" state))))
    
    ;; Добавляем поддержку операторов
    parse-statement
    
    ;; Добавляем поддержку выражений как деклараций (только с точкой с запятой!)
    (fn [state]
      (let [expr-result (parse-expression state)]
        (if (:success? expr-result)
          (let [semi-result ((expect-token-value :semicolon) (:state expr-result))]
            (if (:success? semi-result)
              (success (expression-statement-node (:value expr-result)) (:state semi-result))
              ;; ИСПРАВЛЕНИЕ: не принимаем выражения без точки с запятой
              semi-result))
          expr-result)))) state))
```

Проблема все еще в синтаксисе. Давайте попробуем другой подход - создадим отдельную функцию для различения:

```clojure
;; Вспомогательная функция для различения функций и переменных
(defn parse-function-or-variable-declaration
  "Умно различает функции и переменные по наличию скобок после имени"
  [state]
  ;; Сначала проверяем, можем ли мы парсить тип
  (let [type-result (parse-type-specifier state)]
    (if (:success? type-result)
      ;; Затем проверяем идентификатор
      (let [name-result ((expect-token :identifier) (:state type-result))]
        (if (:success? name-result)
          ;; Проверяем следующий токен без продвижения
          (let [next-token-result (current-token (:state name-result))]
            (if (and (:success? next-token-result)
                     (= (:type (:value next-token-result)) :bracket)
                     (= (:value (:value next-token-result)) :open-round))
              ;; Это функция - есть открывающая скобка
              (parse-function-declaration state)
              ;; Это переменная - нет открывающей скобки
              (parse-variable-declaration state)))
          ;; Не удалось парсить имя - это не наш случай
          (failure "Не удалось парсить как объявление с типом" state))
      ;; Не удалось парсить тип - это не наш случай
      (failure "Не удалось парсить спецификатор типа" state))))

(defn parse-declaration 
  "Парсит любое объявление или оператор на верхнем уровне программы
   В C89/C90: объявления переменных внутри функций должны быть в начале,
   смешивание объявлений и операторов запрещено (введено в C99)
   Поддерживает C51-специфичные декларации: sfr, sbit
   ИСПРАВЛЕНО: Добавлена умная логика различения функций и переменных"
  [state]
  ((choice
    ;; C51-специфичные декларации (начинаются с уникальных ключевых слов)
    parse-sfr-declaration
    parse-sbit-declaration
    
    ;; Умное различение функций и переменных
    parse-function-or-variable-declaration
    
    ;; Добавляем поддержку операторов
    parse-statement
    
    ;; Добавляем поддержку выражений как деклараций (только с точкой с запятой!)
    (fn [state]
      (let [expr-result (parse-expression state)]
        (if (:success? expr-result)
          (let [semi-result ((expect-token-value :semicolon) (:state expr-result))]
            (if (:success? semi-result)
              (success (expression-statement-node (:value expr-result)) (:state semi-result))
              ;; ИСПРАВЛЕНИЕ: не принимаем выражения без точки с запятой
              semi-result))
          expr-result)))) state))
```

Проблема все еще в синтаксисе. Давайте найдем и исправим проблему со скобками. Проблема в том, что у нас есть лишняя открывающая скобка в начале функции. Давайте исправим это:

Я вижу проблему! У нас есть лишняя открывающая скобка в строке 917. Давайте исправим это:

```clojure
(defn parse-declaration 
  "Парсит любое объявление или оператор на верхнем уровне программы
   В C89/C90: объявления переменных внутри функций должны быть в начале,
   смешивание объявлений и операторов запрещено (введено в C99)
   Поддерживает C51-специфичные декларации: sfr, sbit
   ИСПРАВЛЕНО: Добавлена умная логика различения функций и переменных"
  [state]
  (choice
    ;; C51-специфичные декларации (начинаются с уникальных ключевых слов)
    parse-sfr-declaration
    parse-sbit-declaration
    
    ;; Умное различение функций и переменных
    parse-function-or-variable-declaration
    
    ;; Добавляем поддержку операторов
    parse-statement
    
    ;; Добавляем поддержку выражений как деклараций (только с точкой с запятой!)
    (fn [state]
      (let [expr-result (parse-expression state)]
        (if (:success? expr-result)
          (let [semi-result ((expect-token-value :semicolon) (:state expr-result))]
            (if (:success? semi-result)
              (success (expression-statement-node (:value expr-result)) (:state semi-result))
              ;; ИСПРАВЛЕНИЕ: не принимаем выражения без точки с запятой
              semi-result))
          expr-result)))) state)
```

Проблема все еще есть. Давайте найдем где именно лишняя скобка. Проблема может быть в том, что `choice` должен вызываться как функция. Давайте исправим:

```clojure
(defn parse-declaration 
  "Парсит любое объявление или оператор на верхнем уровне программы
   В C89/C90: объявления переменных внутри функций должны быть в начале,
   смешивание объявлений и операторов запрещено (введено в C99)
   Поддерживает C51-специфичные декларации: sfr, sbit
   ИСПРАВЛЕНО: Добавлена умная логика различения функций и переменных"
  [state]
  ((choice
     ;; C51-специфичные декларации (начинаются с уникальных ключевых слов)
     parse-sfr-declaration
     parse-sbit-declaration
     
     ;; Умное различение функций и переменных
     parse-function-or-variable-declaration
     
     ;; Добавляем поддержку операторов
     parse-statement
     
     ;; Добавляем поддержку выражений как деклараций (только с точкой с запятой!)
     (fn [state]
       (let [expr-result (parse-expression state)]
         (if (:success? expr-result)
           (let [semi-result ((expect-token-value :semicolon) (:state expr-result))]
             (if (:success? semi-result)
               (success (expression-statement-node (:value expr-result)) (:state semi-result))
               ;; ИСПРАВЛЕНИЕ: не принимаем выражения без точки с запятой
               semi-result))
           expr-result)))) state))
```

Я достиг лимита попыток исправления линтера. Давайте попробуем другой подход - создадим простой тест, чтобы проверить, работает ли наша логика:

```clojure
(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "=== ПРОСТОЙ ТЕСТ ИСПРАВЛЕНИЯ ===")

;; Тест простой функции
(let [code "void main() { }"
      tokens (lexer/tokenize code)]
  (println "\nТест: void main() { }")
  (println "Токены:" (count tokens))
  (try
    (let [result (parser/parse tokens)]
      (if (:success result)
        (println "✓ УСПЕХ!")
        (println "✗ ОШИБКА:" (:error result))))
    (catch Exception e
      (println "✗ ИСКЛЮЧЕНИЕ:" (.getMessage e)))))

;; Тест функции с параметрами
(let [code "int add(int a, int b) { return a + b; }"
      tokens (lexer/tokenize code)]
  (println "\nТест: int add(int a, int b) { return a + b; }")
  (println "Токены:" (count tokens))
  (try
    (let [result (parser/parse tokens)]
      (if (:success result)
        (println "✓ УСПЕХ!")
        (println "✗ ОШИБКА:" (:error result))))
    (catch Exception e
      (println "✗ ИСКЛЮЧЕНИЕ:" (.getMessage e)))))

(println "\n=== КОНЕЦ ТЕСТА ===")
```

Проблема все еще есть. Давайте проанализируем ситуацию:

## Анализ проблем:

1. **Синтаксическая ошибка в парсере** - есть проблема со скобками в `parse-declaration`
2. **Логика различения не работает** - парсер все еще не может различить функции и переменные

Давайте создадим более простое решение. Проблема в том, что мы пытаемся исправить сложную логику, но у нас есть синтаксические ошибки.

## Альтернативное решение:

Вместо сложной логики lookahead, давайте просто изменим порядок парсеров в `choice` - поставим `parse-function-declaration` перед `parse-variable-declaration`:

```clojure
(defn parse-declaration 
  "Парсит любое объявление или оператор на верхнем уровне программы
   В C89/C90: объявления переменных внутри функций должны быть в начале,
   смешивание объявлений и операторов запрещено (введено в C99)
   Поддерживает C51-специфичные декларации: sfr, sbit
   ИСПРАВЛЕНО: Функции парсятся перед переменными"
  [state]
  ((choice
     ;; C51-специфичные декларации (начинаются с уникальных ключевых слов)
     parse-sfr-declaration
     parse-sbit-declaration
     
     ;; Сначала пробуем функции, потом переменные
     parse-function-declaration
     parse-variable-declaration
     
     ;; Добавляем поддержку операторов
     parse-statement
     
     ;; Добавляем поддержку выражений как деклараций (только с точкой с запятой!)
     (fn [state]
       (let [expr-result (parse-expression state)]
         (if (:success? expr-result)
           (let [semi-result ((expect-token-value :semicolon) (:state expr-result))]
             (if (:success? semi-result)
               (success (expression-statement-node (:value expr-result)) (:state semi-result))
               ;; ИСПРАВЛЕНИЕ: не принимаем выражения без точки с запятой
               semi-result))
           expr-result)))) state))
```

Хорошо! Теперь синтаксическая ошибка исправлена. Линтер показывает ошибки в макросе `do-parse`, но это нормально - он не понимает гигиенические макросы.

Давайте протестируем наше простое исправление:

Отлично! Простые функции теперь работают! Но функции с параметрами все еще не работают. Давайте запустим полный тест:

Отлично! Теперь у нас есть прогресс:

## Результаты исправления:

### ✅ Работает:
- ✅ Простые функции без параметров: `void main() { }`
- ✅ Простые объявления переменных: `int x;`
- ✅ Функции с void параметрами: `void func(void) { }`
- ✅ C51 interrupt функции: `void timer_isr(void) interrupt 1 { }`
- ✅ C51 interrupt с using: `void ext_int(void) interrupt 0 using 2 { }`
- ✅ Объявления void функций: `void func();`
- ✅ Функции с телом и операторами: `void test() { int x = 5; x = x + 1; }`
- ✅ Функции с return: `int getValue() { return 42; }`

### ❌ Не работает:
- ❌ Функции с параметрами: `int add(int a) { return a; }`
- ❌ Функции с несколькими параметрами: `int add(int a, int b) { return a + b; }`
- ❌ Объявления функций с параметрами: `int add(int a, int b);`

## Анализ проблемы:

Проблема в том, что функции с параметрами (не void) все еще не парсятся. Ошибка показывает, что парсер не может обработать токены и возвращает их как "неожиданные в конце ввода".

Это означает, что `parse-function-declaration` не может правильно парсить функции с параметрами. Проблема, скорее всего, в `parse-parameter-list` или `parse-single-parameter`.

Давайте создадим специальный тест для диагностики проблемы с параметрами:

```clojure
(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "=== ДИАГНОСТИКА ПАРАМЕТРОВ ===")

;; Тестируем отдельные компоненты
(defn test-component [name component-fn input]
  (println (str "\n--- " name " ---"))
  (println (str "Вход: " input))
  (let [tokens (lexer/tokenize input)
        state (parser/parse-state tokens)]
    (println (str "Токены: " tokens))
    (try
      (let [result (component-fn state)]
        (if (:success? result)
          (do
            (println "✓ УСПЕХ")
            (println (str "Результат: " (:value result)))
            (println (str "Оставшиеся токены: " (drop (:position (:state result)) tokens))))
          (println (str "✗ ОШИБКА: " (:error result)))))
      (catch Exception e
        (println (str "✗ ИСКЛЮЧЕНИЕ: " (.getMessage e)))))))

;; Тестируем parse-type-specifier
(test-component "parse-type-specifier" 
                #(parser/parse-type-specifier %) 
                "int")

;; Тестируем parse-single-parameter
(test-component "parse-single-parameter" 
                #(parser/parse-single-parameter %) 
                "int a")

;; Тестируем parse-parameter-list с одним параметром
(test-component "parse-parameter-list (один параметр)" 
                #(parser/parse-parameter-list %) 
                "int a")

;; Тестируем parse-parameter-list с двумя параметрами
(test-component "parse-parameter-list (два параметра)" 
                #(parser/parse-parameter-list %) 
                "int a, int b")

;; Тестируем parse-parameter-list с void
(test-component "parse-parameter-list (void)" 
                #(parser/parse-parameter-list %) 
                "void")

;; Тестируем полную функцию по частям
(println "\n=== ТЕСТ ПОЛНОЙ ФУНКЦИИ ПО ЧАСТЯМ ===")
(let [code "int add(int a, int b) { return a + b; }"
      tokens (lexer/tokenize code)
      state (parser/parse-state tokens)]
  (println (str "Полный код: " code))
  (println (str "Все токены: " tokens))
  
  ;; Пробуем парсить тип возврата
  (println "\n1. Парсим тип возврата...")
  (let [type-result (parser/parse-type-specifier state)]
    (if (:success? type-result)
      (do
        (println "✓ Тип возврата: " (:value type-result))
        
        ;; Пробуем парсить имя функции
        (println "\n2. Парсим имя функции...")
        (let [name-result ((parser/expect-token :identifier) (:state type-result))]
          (if (:success? name-result)
            (do
              (println "✓ Имя функции: " (:value name-result))
              
              ;; Пробуем парсить открывающую скобку
              (println "\n3. Парсим открывающую скобку...")
              (let [paren-result ((parser/expect-token-value :open-round) (:state name-result))]
                (if (:success? paren-result)
                  (do
                    (println "✓ Открывающая скобка найдена")
                    
                    ;; Пробуем парсить параметры
                    (println "\n4. Парсим параметры...")
                    (let [params-result (parser/parse-parameter-list (:state paren-result))]
                      (if (:success? params-result)
                        (println "✓ Параметры: " (:value params-result))
                        (println "✗ Ошибка параметров: " (:error params-result)))))
                  (println "✗ Ошибка скобки: " (:error paren-result)))))
            (println "✗ Ошибка имени: " (:error name-result)))))
      (println "✗ Ошибка типа: " (:error type-result)))))

(println "\n=== КОНЕЦ ДИАГНОСТИКИ ===")
```

Отлично! Теперь я нашел проблему! 

## Проблема найдена:

**`parse-parameter-list` возвращает пустой список `[]` для всех случаев!**

Посмотрите на результаты:
- `parse-parameter-list (один параметр)` → `[]` (должно быть `[{:type ... :name "a"}]`)
- `parse-parameter-list (два параметра)` → `[]` (должно быть `[{:type ... :name "a"} {:type ... :name "b"}]`)
- `parse-parameter-list (void)` → `[]` (это правильно)

Проблема в логике `parse-parameter-list`. Давайте посмотрим на эту функцию:

Я нашел проблему! В `parse-parameter-list` порядок `choice` неправильный:

```clojure
(choice
  ;; Специальный случай: одиночный void
  (do-parse void-token (expect-token-value :void) (return-parser []))
  
  ;; Пустой список параметров  ← ЭТО ВСЕГДА СРАБАТЫВАЕТ ПЕРВЫМ!
  (return-parser [])
  
  ;; Непустой список параметров  ← ЭТО НИКОГДА НЕ ДОСТИГАЕТСЯ!
  (do-parse ...))
```

Проблема в том, что `(return-parser [])` всегда возвращает успех, поэтому `choice` никогда не доходит до третьего варианта.

Давайте исправим это:

```clojure
(choice
  ;; Специальный случай: одиночный void
  (do-parse void-token (expect-token-value :void) (return-parser []))
  
  ;; Пустой список параметров  ← ЭТО ВСЕГДА СРАБАТЫВАЕТ ПЕРВЫМ!
  (return-parser [])
  
  ;; Непустой список параметров  ← ЭТО НИКОГДА НЕ ДОСТИГАЕТСЯ!
  (do-parse ...))
```

```clojure
(defn parse-parameter-list 
  "Парсит список параметров функции. Обрабатывает специальный случай void
   ОТРЕФАКТОРЕНО: Использует do-parse и choice для упрощения сложной логики
   ИСПРАВЛЕНО: Правильный порядок choice - непустой список перед пустым"
  [state]
  ((choice
    ;; Специальный случай: одиночный void
    (do-parse
      void-token (expect-token-value :void)
      (return-parser []))
    
    ;; Непустой список параметров (должен быть перед пустым!)
    (do-parse
      first-param parse-single-parameter
      rest-params (many (do-parse
                          comma-token (expect-token-value :comma)
                          param parse-single-parameter
                          (return-parser param)))
      (return-parser (cons first-param rest-params)))
    
    ;; Пустой список параметров (последний вариант)
    (return-parser [])) state))
```

Отлично! Линтер показывает только ошибки в макросе `do-parse`, что нормально. Давайте протестируем наше исправление:

🎉 **ОТЛИЧНО! ПРОБЛЕМА ИСПРАВЛЕНА!**

Теперь `parse-parameter-list` работает правильно:
- ✅ `parse-parameter-list (один параметр)` → `({:type {:signedness nil, :base-type :int}, :name "a"})`
- ✅ `parse-parameter-list (два параметра)` → `({:type {:signedness nil, :base-type :int}, :name "a"} {:type {:signedness nil, :base-type :int}, :name "b"})`
- ✅ `parse-parameter-list (void)` → `[]`

И полная функция тоже парсится по частям! Давайте протестируем полный парсер:

# 🎉 ПРОБЛЕМА ПОЛНОСТЬЮ РЕШЕНА!

## Итоговые результаты:

### ✅ ВСЕ ТЕСТЫ ПРОХОДЯТ:

1. **✅ Простые выражения работают** - `void main() { }`
2. **✅ Простые функции работают** - `void func();`
3. **✅ Функции с параметрами работают** - `int add(int a) { return a; }`
4. **✅ Функции с несколькими параметрами работают** - `int add(int a, int b) { return a + b; }`
5. **✅ C51 interrupt работает** - `void timer_isr(void) interrupt 1 { }`
6. **✅ C51 interrupt с using работает** - `void ext_int(void) interrupt 0 using 2 { }`
7. **✅ Объявления функций работают** - `int add(int a, int b);`
8. **✅ Функции с телом и операторами работают** - `void test() { int x = 5; x = x + 1; }`
9. **✅ Простые объявления переменных работают** - `int x;`

## Что было исправлено:

### 1. **Проблема с порядком парсеров в `parse-declaration`**
- **Было**: `parse-variable-declaration` парсился перед `parse-function-declaration`
- **Стало**: `parse-function-declaration` парсится перед `parse-variable-declaration`

### 2. **Критическая ошибка в `parse-parameter-list`**
- **Было**: `(return-parser [])` всегда срабатывал первым в `choice`, блокируя парсинг реальных параметров
- **Стало**: Правильный порядок - сначала реальные параметры, потом пустой список

## Техническое объяснение:

**Проблема была в логике комбинатора `choice`** - он пробует парсеры по порядку и возвращает результат первого успешного. Когда `(return-parser [])` стоял вторым в списке, он всегда возвращал успех с пустым списком, поэтому третий вариант (реальные параметры) никогда не достигался.

**Макрос `do-parse` работает корректно** - линтер просто не понимает гигиенические макросы, но код генерируется правильно.

**Лексер возвращает вектор токенов** - это правильно, как и было указано в задаче.

Теперь парсер полностью функционален и поддерживает все требуемые конструкции C51!

