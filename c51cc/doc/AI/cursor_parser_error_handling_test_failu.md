# Parser Error Handling Test Failures
_Exported on 5/27/2025 at 16:11:20 GMT+3 from Cursor (0.50.5)_

---

**User**

lein test c51cc.parser-test

lein test :only c51cc.parser-test/test-error-handling

FAIL in (test-error-handling) (parser_test.clj:535)
Синтаксическая ошибка - отсутствующая скобка
expected: (not (parse-successful? result))
  actual: (not (not true))

lein test :only c51cc.parser-test/test-error-handling

FAIL in (test-error-handling) (parser_test.clj:536)
Синтаксическая ошибка - отсутствующая скобка
expected: (string? (get-error result))
  actual: (not (string? nil))

lein test :only c51cc.parser-test/test-error-handling

FAIL in (test-error-handling) (parser_test.clj:540)
Синтаксическая ошибка - неожиданный токен
expected: (not (parse-successful? result))
  actual: (not (not true))

lein test :only c51cc.parser-test/test-error-handling

FAIL in (test-error-handling) (parser_test.clj:541)
Синтаксическая ошибка - неожиданный токен
expected: (string? (get-error result))
  actual: (not (string? nil))


---

**Cursor**

Я вижу проблему с обработкой ошибок в парсере. Тесты показывают, что парсер возвращает успешный результат даже в случаях синтаксических ошибок, когда должен возвращать ошибку. Давайте сначала посмотрим на тесты, чтобы понять ожидаемое поведение.

Теперь давайте посмотрим на вспомогательные функции для тестов:

Теперь я понимаю проблему. Давайте проанализируем, что происходит с тестами ошибок:

1. `"func("` - это неполный вызов функции без закрывающей скобки
2. `"int + 42"` - это некорректная последовательность токенов
3. `"int x = 42"` - объявление переменной без точки с запятой

Проблема в том, что парсер слишком "снисходительный" и пытается парсить что-то даже из некорректного ввода. Давайте посмотрим, как работает функция `parse-program`:

Теперь я вижу проблемы! Есть несколько критических недостатков в обработке ошибок:

1. **В `parse-declaration`** (строки 975-987): если выражение парсится успешно, но нет точки с запятой, парсер все равно возвращает успех без точки с запятой.

2. **В `parse-program`** (строки 989-1000): если `many parse-declaration` не находит деклараций, парсер пытается парсить как простое выражение, что маскирует ошибки.

3. **Основная функция `parse`** не проверяет, что все токены были использованы.

Давайте также проверим, что происходит с конкретными тестовыми случаями:

Давайте исправим проблемы с обработкой ошибок. Основные проблемы:

1. **Парсер слишком снисходительный** - он принимает неполные конструкции
2. **Не проверяется, что все токены использованы**
3. **Fallback логика в `parse-program` маскирует ошибки**

```clojure
(defn parse-declaration 
  "Парсит любое объявление или оператор"
  [state]
  ((choice
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

(defn parse-program 
  "Парсит всю программу"
  [state]
  (let [declarations-result ((many parse-declaration) state)]
    (if (:success? declarations-result)
      ;; ИСПРАВЛЕНИЕ: убираем fallback логику, которая маскирует ошибки
      (success (program-node (:value declarations-result)) (:state declarations-result))
      declarations-result)))
```

Теперь давайте также улучшим основную функцию `parse`, чтобы она проверяла, что все токены были использованы:

```clojure
(defn parse 
  "Основная функция парсинга. Принимает вектор токенов, возвращает AST"
  [tokens]
  {:pre [(vector? tokens)]
   :post [(map? %)]}
  (when DEBUG
    (log/debug "Начинаем парсинг токенов:" tokens))
  
  (let [initial-state (parse-state tokens)
        result (parse-program initial-state)]
    
    (when DEBUG
      (log/debug "Результат парсинга:" result))
    
    (if (:success? result)
      ;; ИСПРАВЛЕНИЕ: проверяем, что все токены использованы
      (let [final-position (:position (:state result))
            remaining-tokens (drop final-position tokens)]
        (if (empty? remaining-tokens)
          {:success true
           :ast (:value result)
           :remaining-tokens []}
          {:success false
           :error (str "Неожиданные токены в конце ввода: " 
                      (take 3 remaining-tokens))
           :position final-position}))
      {:success false
       :error (:error result)
       :position (:position (:state result))})))
```

Теперь нужно исправить еще одну проблему. Давайте посмотрим на функцию `parse-postfix-expression`, которая обрабатывает вызовы функций. Проблема в том, что `"func("` может парситься как идентификатор, а затем парсер пытается найти аргументы, но не находит закрывающую скобку:

Теперь давайте запустим тесты, чтобы посмотреть, исправились ли проблемы:

Отлично! Тесты обработки ошибок теперь проходят. Давайте запустим все тесты парсера, чтобы убедиться, что мы не сломали ничего другого:

Ой! Мы сломали много тестов. Проблема в том, что наша проверка "все токены использованы" слишком строгая. Многие тесты ожидают, что парсер может парсить только часть входных данных. Давайте посмотрим на конкретные ошибки и исправим их.

Основная проблема: мы добавили проверку, что все токены должны быть использованы, но это не всегда правильно. Давайте сделаем эту проверку опциональной или более умной:

Я вижу проблему. Наша проверка "все токены использованы" слишком строгая. Проблема в том, что мы проверяем это для всех случаев, но некоторые тесты ожидают, что парсер может парсить только часть входных данных.

Однако, для тестов ошибок нам нужна эта проверка. Давайте сделаем более умную логику: проверяем все токены использованы только если парсинг был успешным И мы действительно что-то спарсили:

```clojure
(defn parse 
  "Основная функция парсинга. Принимает вектор токенов, возвращает AST"
  [tokens]
  {:pre [(vector? tokens)]
   :post [(map? %)]}
  (when DEBUG
    (log/debug "Начинаем парсинг токенов:" tokens))
  
  (let [initial-state (parse-state tokens)
        result (parse-program initial-state)]
    
    (when DEBUG
      (log/debug "Результат парсинга:" result))
    
    (if (:success? result)
      ;; ИСПРАВЛЕНИЕ: проверяем неиспользованные токены более умно
      (let [final-position (:position (:state result))
            remaining-tokens (drop final-position tokens)]
        ;; Проверяем только если есть значимые неиспользованные токены
        ;; и мы действительно что-то спарсили (не пустая программа)
        (if (and (not (empty? remaining-tokens))
                 (not (empty? (:declarations (:value result)))))
          {:success false
           :error (str "Неожиданные токены в конце ввода: " 
                      (take 3 remaining-tokens))
           :position final-position}
          {:success true
           :ast (:value result)
           :remaining-tokens remaining-tokens}))
      {:success false
       :error (:error result)
       :position (:position (:state result))})))
```

Теперь давайте запустим тесты снова:

Ой, мы снова сломали тесты ошибок. Проблема в том, что наша логика слишком сложная. Давайте посмотрим, что именно происходит с тестовыми случаями. Нужно понять, почему `"func("` теперь парсится успешно.

Давайте отладим это:

Давайте проанализируем проблему. Я думаю, что `"func("` парсится как идентификатор `func`, а затем парсер останавливается, не пытаясь парсить `(` как начало вызова функции, потому что нет закрывающей скобки.

Проблема в том, что мы изменили логику так, что если программа не пустая, то мы проверяем неиспользованные токены. Но `"func("` парсится как программа с одним выражением-идентификатором, и остается токен `(`.

Давайте исправим это более точно. Нам нужно различать случаи:
1. Синтаксические ошибки (должны возвращать ошибку)
2. Успешный парсинг с оставшимися токенами (может быть OK)

```clojure
(defn parse 
  "Основная функция парсинга. Принимает вектор токенов, возвращает AST"
  [tokens]
  {:pre [(vector? tokens)]
   :post [(map? %)]}
  (when DEBUG
    (log/debug "Начинаем парсинг токенов:" tokens))
  
  (let [initial-state (parse-state tokens)
        result (parse-program initial-state)]
    
    (when DEBUG
      (log/debug "Результат парсинга:" result))
    
    (if (:success? result)
      ;; Проверяем, что все токены использованы
      (let [final-position (:position (:state result))
            remaining-tokens (drop final-position tokens)]
        (if (empty? remaining-tokens)
          {:success true
           :ast (:value result)
           :remaining-tokens []}
          {:success false
           :error (str "Неожиданные токены в конце ввода: " 
                      (take 3 remaining-tokens))
           :position final-position}))
      {:success false
       :error (:error result)
       :position (:position (:state result))})))
```

Теперь проблема в том, что многие тесты ожидают, что парсер может парсить только часть входных данных. Но для правильной обработки ошибок нам нужно проверять все токены.

Давайте посмотрим на конкретные тестовые случаи, которые падают, и поймем, в чем проблема:

Хм, что-то пошло не так. Давайте попробуем другой подход. Я думаю, проблема в том, что мы пытаемся решить две разные задачи одновременно:

1. **Обработка ошибок** - когда ввод синтаксически некорректен
2. **Парсинг части ввода** - когда мы хотим парсить только начало

Давайте посмотрим на тесты более внимательно и поймем, какая логика нужна:

Теперь я понимаю проблему! Посмотрите на тесты:

1. `"while (x > 0) x--;"` - это **оператор**, а не декларация
2. `"for (i = 0; i < 10; i++) sum += i;"` - это тоже **оператор**

Но наш парсер пытается парсить их как **декларации** в программе. Проблема в том, что `parse-program` использует `many parse-declaration`, но операторы вроде `while` и `for` должны парситься как операторы, а не как декларации на верхнем уровне.

В языке C операторы `while` и `for` обычно находятся **внутри функций**, а не на верхнем уровне программы. Но тесты ожидают, что мы можем парсить их как отдельные конструкции.

Это означает, что наш `parse-program` должен быть более гибким. Давайте исправим это:

```clojure
(defn parse-program 
  "Парсит всю программу"
  [state]
  (let [declarations-result ((many (choice parse-declaration parse-statement)) state)]
    (if (:success? declarations-result)
      (success (program-node (:value declarations-result)) (:state declarations-result))
      declarations-result)))
```

Но теперь у нас есть проблема с дублированием: `parse-declaration` уже включает `parse-statement`. Давайте исправим это:

```clojure
(defn parse-program 
  "Парсит всю программу"
  [state]
  (let [declarations-result ((many parse-declaration) state)]
    (if (:success? declarations-result)
      (success (program-node (:value declarations-result)) (:state declarations-result))
      declarations-result)))
```

Теперь давайте запустим тесты снова:

Проблема все еще в том, что парсер находит неиспользованные токены. Давайте посмотрим на конкретный тест:

`"while (x > 0) x--;"`

Этот код должен парситься как один оператор `while`, но парсер, видимо, парсит только часть и оставляет токены.

Я думаю, проблема в том, что наша проверка "все токены использованы" слишком строгая для тестов. В реальном компиляторе это правильно, но для unit-тестов отдельных конструкций это мешает.

Давайте временно отключим эту проверку и сосредоточимся на том, чтобы тесты ошибок работали правильно другим способом:

```clojure
(defn parse 
  "Основная функция парсинга. Принимает вектор токенов, возвращает AST"
  [tokens]
  {:pre [(vector? tokens)]
   :post [(map? %)]}
  (when DEBUG
    (log/debug "Начинаем парсинг токенов:" tokens))
  
  (let [initial-state (parse-state tokens)
        result (parse-program initial-state)]
    
    (when DEBUG
      (log/debug "Результат парсинга:" result))
    
    (if (:success? result)
      {:success true
       :ast (:value result)
       :remaining-tokens (drop (:position (:state result)) tokens)}
      {:success false
       :error (:error result)
       :position (:position (:state result))})))
```

Теперь давайте запустим тесты операторов:

Хорошо, теперь парсинг проходит успешно, но тесты все еще падают, потому что `while-stmt` равен `nil`. Это означает, что структура AST не такая, как ожидают тесты.

Давайте посмотрим, как тест извлекает `while-stmt`:

Я вижу проблему! Тест ожидает, что `while-stmt` будет `(first (:declarations ast))`, то есть первым элементом в `:declarations`. Но если `(:declarations ast)` пустой или содержит что-то другое, то `(first (:declarations ast))` вернет `nil`.

Давайте отладим это, создав простой тест:

Давайте создадим простой тестовый файл для отладки:

```clojure
(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(defn debug-parse [input]
  (let [tokens (lexer/tokenize input)
        result (parser/parse tokens)]
    (println "Input:" input)
    (println "Tokens:" tokens)
    (println "Parse result:" result)
    (when (:success result)
      (println "AST:" (:ast result))
      (println "Declarations:" (:declarations (:ast result)))
      (println "First declaration:" (first (:declarations (:ast result)))))
    result))

(debug-parse "while (x > 0) x--;")
```

Отлично! Я вижу проблему из вывода. Посмотрите на результаты:

1. `"42"` → `{:ast-type :program, :declarations []}` - пустые декларации!
2. `"x"` → `{:ast-type :program, :declarations []}` - пустые декларации!
3. `"x + y"` → `{:ast-type :program, :declarations []}` - пустые декларации!
4. `"func()"` → `{:ast-type :program, :declarations []}` - пустые декларации!
5. `"int x;"` → успешно парсится как декларация переменной
6. `"void main() { }"` → успешно парсится как декларация функции

Проблема в том, что **простые выражения и операторы не парсятся как декларации**! Функция `parse-declaration` не может парсить `while`, `for`, или простые выражения.

Давайте посмотрим на `parse-declaration` еще раз:

Теперь я вижу проблему! В `parse-declaration` есть циклическая зависимость:

1. `parse-declaration` вызывает `parse-statement`
2. `parse-statement` вызывает `parse-variable-declaration`
3. `parse-variable-declaration` это часть `parse-declaration`

Это создает бесконечную рекурсию или неправильное поведение. Кроме того, `parse-statement` включает `parse-variable-declaration`, но `parse-variable-declaration` уже есть в `parse-declaration`.

Давайте исправим это:

```clojure
(defn parse-statement 
  "Парсит любой оператор"
  [state]
  ((choice
    parse-block-statement
    parse-if-statement
    parse-while-statement
    parse-for-statement
    parse-return-statement
    parse-expression-statement) state))
```

Теперь давайте запустим отладочный скрипт снова:

```clojure
(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(defn debug-parse [input]
  (let [tokens (lexer/tokenize input)
        result (parser/parse tokens)]
    (println "=== Testing input:" input "===")
    (println "Tokens:" tokens)
    (println "Parse result:" result)
    (when (:success result)
      (println "AST:" (:ast result))
      (println "Declarations:" (:declarations (:ast result)))
      (println "First declaration:" (first (:declarations (:ast result))))
      (when-let [first-decl (first (:declarations (:ast result)))]
        (println "First declaration type:" (:ast-type first-decl))))
    (println)
    result))

(debug-parse "while (x > 0) x--;")
(debug-parse "for (i = 0; i < 10; i++) sum += i;")
(debug-parse "int x = 42;")
(debug-parse "void main() { }")
```

Хорошо! Теперь я вижу, что происходит. Из вывода видно, что:

1. `"int x;"` → успешно парсится как `variable-declaration`
2. `"void main() { }"` → успешно парсится как `function-declaration`

Но `while` и `for` все еще не парсятся. Проблема в том, что в выводе я не вижу результаты для `while` и `for`. Давайте создадим более простой тест:

```clojure
(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(defn debug-parse [input]
  (let [tokens (lexer/tokenize input)
        result (parser/parse tokens)]
    (println "=== Testing input:" input "===")
    (println "Tokens:" tokens)
    (println "Parse result:" result)
    (when (:success result)
      (println "AST:" (:ast result))
      (println "Declarations:" (:declarations (:ast result)))
      (println "First declaration:" (first (:declarations (:ast result))))
      (when-let [first-decl (first (:declarations (:ast result)))]
        (println "First declaration type:" (:ast-type first-decl))))
    (println)
    result))

(println "Testing while statement:")
(debug-parse "while (x > 0) x--;")
```

Но сначала давайте проверим, что происходит с тестами после исправления циклической зависимости:

Хорошо, теперь тесты проходят успешно (нет ошибок парсинга), но структура AST неправильная. Проблема в том, что `while-stmt` все еще `nil`.

Давайте запустим наш отладочный скрипт, чтобы понять, что происходит:

Проблема с PowerShell. Давайте создадим простой тестовый файл:

```clojure
(ns test-while
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn -main []
  (let [input "while (x > 0) x--;"
        tokens (lexer/tokenize input)
        result (parser/parse tokens)]
    (println "Input:" input)
    (println "Tokens:" tokens)
    (println "Parse successful?:" (:success result))
    (println "AST:" (:ast result))
    (when (:success result)
      (let [declarations (:declarations (:ast result))]
        (println "Declarations count:" (count declarations))
        (println "First declaration:" (first declarations))
        (when-let [first-decl (first declarations)]
          (println "First declaration type:" (:ast-type first-decl)))))))

(-main)
```

Давайте попробуем другой подход. Я думаю, проблема в том, что `many` в `parse-program` не может парсить ни одной декларации, поэтому возвращает пустой список.

Давайте посмотрим на функцию `many`:

Теперь я вижу функцию `many`. Она работает правильно - если парсер не может ничего спарсить, она возвращает пустой список.

Проблема в том, что `parse-declaration` не может парсить `while` statement. Давайте проследим логику:

1. `parse-program` вызывает `(many parse-declaration)`
2. `parse-declaration` пробует:
   - `parse-function-declaration` - не подходит (нет типа в начале)
   - `parse-variable-declaration` - не подходит (нет типа в начале)
   - `parse-statement` - должно подойти!

Но `parse-statement` включает `parse-while-statement`. Давайте проверим, что `parse-while-statement` работает правильно.

Проблема может быть в том, что `while` не распознается как токен. Давайте проверим лексер:

