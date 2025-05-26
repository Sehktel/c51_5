# AST (Abstract Syntax Tree) Module для компилятора C51

## Обзор

Модуль AST представляет собой полнофункциональную систему для работы с абстрактными синтаксическими деревьями в компиляторе C51 для микроконтроллера AT89S4051. Этот модуль обеспечивает мощную основу для анализа, трансформации и оптимизации кода.

## Ключевые возможности

### 🏗️ Полная система типов узлов AST
- **Объявления**: программы, функции, переменные, параметры
- **Операторы**: блоки, условия (if), циклы (while, for), return
- **Выражения**: бинарные, унарные, присваивания, вызовы функций
- **Специфичные для C51**: interrupt, sfr, sbit объявления

### 🔍 Валидация и анализ
- Рекурсивная валидация всех узлов AST
- Проверка корректности типов операторов
- Анализ структуры программы
- Сбор статистики по узлам

### 🚶‍♂️ Visitor Pattern
- Гибкий механизм обхода AST
- Поддержка пользовательских visitor'ов
- Анализ сложности кода
- Поиск потенциальных проблем

### ⚡ Оптимизация
- Свертывание константных выражений
- Упрощение логических выражений
- Удаление мертвого кода
- Пользовательские трансформации

### 🎨 Красивый вывод
- Структурированное отображение AST
- Настраиваемые параметры форматирования
- Отладочная информация
- Сводки и статистика

## Структура файлов

```
c51cc/
├── src/c51cc/
│   └── ast.clj              # Основной модуль AST
├── test_ast.clj             # Комплексные тесты
├── simple_ast_test.clj      # Простые тесты
├── demo_ast_integration.clj # Демонстрация интеграции
├── run_ast_tests.clj        # Скрипт запуска тестов
└── AST_README.md           # Этот файл
```

## Быстрый старт

### Создание простого узла
```clojure
(require '[c51cc.ast :as ast])

;; Создание идентификатора
(def id-node (ast/identifier-node "variable"))

;; Создание литерала
(def lit-node (ast/literal-node 42 :number))

;; Создание бинарного выражения
(def expr-node (ast/binary-expression-node :plus id-node lit-node))
```

### Валидация AST
```clojure
(def errors (ast/validate-ast expr-node))
(if (empty? errors)
  (println "✓ AST корректен")
  (println "✗ Найдены ошибки:" errors))
```

### Красивый вывод
```clojure
(ast/pretty-print expr-node)
;; Выводит:
;; (binary-expression
;;   operator: :plus
;;   left: (identifier
;;       name: "variable")
;;   right: (literal
;;       value: 42
;;       literal-type: :number))
```

### Анализ AST
```clojure
;; Сбор всех идентификаторов
(def identifiers (ast/collect-identifiers expr-node))

;; Подсчет узлов по типам
(def node-counts (ast/count-nodes-by-type expr-node))

;; Вычисление глубины дерева
(def depth (ast/calculate-ast-depth expr-node))
```

### Оптимизация
```clojure
;; Создание константного выражения
(def const-expr (ast/binary-expression-node 
                  :plus 
                  (ast/literal-node 5 :number) 
                  (ast/literal-node 10 :number)))

;; Оптимизация (5 + 10 → 15)
(def optimized (ast/optimize-ast const-expr))
```

## Типы узлов AST

### Объявления верхнего уровня
- `:program` - корневой узел программы
- `:function-declaration` - объявление функции
- `:variable-declaration` - объявление переменной
- `:parameter-declaration` - объявление параметра

### Операторы (Statements)
- `:block-statement` - блок операторов в {}
- `:expression-statement` - оператор-выражение
- `:if-statement` - условный оператор
- `:while-statement` - цикл while
- `:for-statement` - цикл for
- `:return-statement` - оператор return

### Выражения (Expressions)
- `:binary-expression` - бинарное выражение (a + b)
- `:unary-expression` - унарное выражение (++a)
- `:assignment-expression` - присваивание (a = b)
- `:call-expression` - вызов функции
- `:identifier` - идентификатор
- `:literal` - литерал (число, строка)
- `:array-access` - доступ к массиву

### Специфичные для C51
- `:interrupt-declaration` - объявление обработчика прерывания
- `:sfr-declaration` - объявление SFR регистра
- `:sbit-declaration` - объявление специального бита

## Visitor Pattern

Модуль поддерживает паттерн Visitor для гибкого обхода AST:

```clojure
(defrecord MyVisitor [state]
  ast/ASTVisitor
  (visit-node [visitor node]
    ;; Обработка узла
    node)
  
  (enter-node [visitor node]
    ;; Вход в узел
    visitor)
  
  (exit-node [visitor node]
    ;; Выход из узла
    visitor))

;; Использование
(def visitor (->MyVisitor (atom {})))
(ast/walk-ast visitor my-ast)
```

## Примеры использования

### Анализ сложности кода
```clojure
(defrecord ComplexityVisitor [complexity]
  ast/ASTVisitor
  (visit-node [visitor node]
    (case (:ast-type node)
      :if-statement (swap! (:complexity visitor) inc)
      :while-statement (swap! (:complexity visitor) inc)
      :for-statement (swap! (:complexity visitor) inc)
      nil)
    node)
  
  (enter-node [visitor node] visitor)
  (exit-node [visitor node] visitor))
```

### Трансформация кода
```clojure
;; Замена магических чисел на константы
(defn replace-magic-numbers [ast]
  (ast/transform-ast
    (fn [node]
      (if (and (= (:ast-type node) :literal)
               (= (:value node) 500))
        (ast/identifier-node "DELAY_MS")
        node))
    ast))
```

### Создание AST для C51 программы
```clojure
(def led-program
  (ast/program-node
    [(ast/sfr-declaration-node "P1" 0x90)
     (ast/sbit-declaration-node "LED" 0x90)
     
     (ast/function-declaration-node
       {:ast-type :type-keyword :base-type :void}
       "main"
       []
       (ast/block-statement-node
         [(ast/while-statement-node
            (ast/literal-node 1 :number)
            (ast/block-statement-node
              [(ast/assignment-expression-node
                 :assign
                 (ast/identifier-node "LED")
                 (ast/unary-expression-node
                   :not
                   (ast/identifier-node "LED")))]))]))]))
```

## Интеграция с компилятором

AST модуль интегрируется с другими компонентами компилятора:

1. **Лексер** → токены
2. **Парсер** → AST (используя функции из ast.clj)
3. **Семантический анализ** → анализ AST
4. **Оптимизация** → трансформация AST
5. **Генерация кода** → обход AST

## Запуск тестов

```bash
# Простой тест
clojure -M simple_ast_test.clj

# Комплексные тесты
clojure -M run_ast_tests.clj

# Демонстрация интеграции
clojure -M demo_ast_integration.clj
```

## Производительность

Модуль оптимизирован для:
- Быстрого создания узлов с валидацией
- Эффективного обхода больших AST
- Минимального потребления памяти
- Ленивых вычислений где возможно

## Расширение функциональности

### Добавление нового типа узла
1. Добавить тип в `ast-node-types`
2. Создать конструктор узла
3. Добавить обработку в `walk-ast`
4. Добавить валидацию если нужно

### Создание нового visitor'а
1. Реализовать протокол `ASTVisitor`
2. Определить логику в `visit-node`
3. Использовать `walk-ast` для обхода

## Отладка

Модуль предоставляет богатые возможности отладки:

```clojure
;; Подробная информация об AST
(ast/print-ast-summary my-ast)

;; Вывод с метаданными
(ast/pretty-print my-ast :show-metadata true)

;; Валидация с детальными ошибками
(def errors (ast/validate-ast my-ast))
(doseq [error errors]
  (println (:message error)))
```

## Заключение

Модуль AST предоставляет мощную и гибкую основу для работы с абстрактными синтаксическими деревьями в компиляторе C51. Он поддерживает все необходимые операции для анализа, трансформации и оптимизации кода, специфичного для микроконтроллеров семейства 8051.

**Основные преимущества:**
- ✅ Полная типизация узлов AST
- ✅ Валидация и проверка корректности
- ✅ Гибкий visitor pattern
- ✅ Встроенные оптимизации
- ✅ Красивый вывод и отладка
- ✅ Специфичные для C51 конструкции
- ✅ Интеграция с лексером и парсером
- ✅ Расширяемая архитектура

Модуль готов к использованию в производственном компиляторе C51! 