# Running Tests from Command Line
_Exported on 5/27/2025 at 20:45:19 GMT+3 from Cursor (0.50.5)_

---

**User**

how to run test for current file? 
test_25_singleton_guard.c for example? @simple_pipeline_test.clj 
from command line?

---

**Cursor**

Looking at your test file, I can see you have a comprehensive testing system for your C51 compiler pipeline. To run a test for a specific file like `test_25_singleton_guard.c` from the command line, you have a few options:

Perfect! Now I can see the structure. Here are several ways to run the test for `test_25_singleton_guard.c` from the command line:

## Option 1: Using Clojure CLI (Recommended)

```bash
clj -M -e "(load-file \"c51cc/simple_pipeline_test.clj\") (quick-test \"test_25_singleton_guard.c\")"
```

## Option 2: Using Clojure REPL interactively

```bash
clj
```
Then in the REPL:
```clojure
(load-file "c51cc/simple_pipeline_test.clj")
(quick-test "test_25_singleton_guard.c")
```

## Option 3: Using Leiningen (if you have project.clj)

```bash
lein repl
```
Then:
```clojure
(load-file "c51cc/simple_pipeline_test.clj")
(quick-test "test_25_singleton_guard.c")
```

## Option 4: Create a dedicated script

```shellscript
clj -M -e "(load-file \"c51cc/simple_pipeline_test.clj\") (quick-test \"test_25_singleton_guard.c\")"
```

```shellscript
clj
```

```clojure
(load-file "c51cc/simple_pipeline_test.clj")
(quick-test "test_25_singleton_guard.c")
```

```shellscript
lein repl
```

```clojure
(load-file "c51cc/simple_pipeline_test.clj")
(quick-test "test_25_singleton_guard.c")
```

```clojure
#!/usr/bin/env clj

;; Скрипт для запуска одного теста из командной строки
;; Использование: clj run_single_test.clj test_25_singleton_guard.c

(load-file "c51cc/simple_pipeline_test.clj")

(defn main [& args]
  (if (empty? args)
    (do
      (println "❌ Ошибка: Не указан файл для тестирования")
      (println "Использование: clj run_single_test.clj <имя_файла.c>")
      (println "Пример: clj run_single_test.clj test_25_singleton_guard.c")
      (System/exit 1))
    (let [filename (first args)]
      (println (str "🎯 Запуск теста для файла: " filename))
      (quick-test filename))))

;; Запуск main функции с аргументами командной строки
(apply main *command-line-args*)
```

The linter error is expected since `quick-test` is defined in the loaded file. Now you can use

---

**User**

PS D:\Work\c51_5\c51cc> clj
Clojure 1.12.0
user=> (load-file "c51cc/simple_pipeline_test.clj")
Execution error (FileNotFoundException) at java.io.FileInputStream/open0 (FileInputStream.java:-2).
c51cc\simple_pipeline_test.clj (Системе не удается найти указанный путь)
user=> (quick-test "test_25_singleton_guard.c")
Syntax error compiling at (REPL:1:1).
Unable to resolve symbol: quick-test in this context

---

**Cursor**

Я вижу проблему! Вы находитесь в директории `c51cc`, но пытаетесь загрузить файл с путем `c51cc/simple_pipeline_test.clj`. Давайте исправим это:

---

**User**

?

---

**Cursor**

Я вижу несколько проблем:

1. **Опечатка в имени файла**: Вы написали `test_25_singletone_guard.c`, но правильно `test_25_singleton_guard.c` (с "ton", а не "tone")

2. **Неправильный путь**: Система ищет файл в `test\ccode\`, но файл находится в `c51cc\test\ccode\`

Давайте исправим это:

---

**User**

is it correct? 

---

**Cursor**

**НЕТ, это НЕ корректно!** 🚨

Результат показывает серьезную проблему в вашем парсере. Давайте проанализируем:

## Проблемы в результате:

1. **AST практически пустой**: `(program declarations: [])` - это означает, что парсер не распознал никаких объявлений
2. **48 токенов → 0 объявлений**: Лексер нашел 48 токенов, но парсер ничего не построил

## Давайте исследуем что происходит:

Теперь я вижу проблему! В файле есть реальный C-код:
- Директивы `#include`
- Функция `main(void)`
- Объявления переменных (`char double_var`)
- Вызовы функций
- Функция `test_func(void)`

Но парсер возвращает пустой AST. Это указывает на **критические проблемы в парсере**.

## Возможные причины:

1. **Парсер не обрабатывает директивы препроцессора** (`#include`)
2. **Парсер не распознает функции** 
3. **Парсер останавливается на первой ошибке** и возвращает пустой результат
4. **Проблемы с обработкой токенов**

## Рекомендации для отладки:

```clojure
;; Отладочный скрипт для анализа проблем с test_25_singleton_guard.c
;; Автор: Senior Developer

(require '[c51cc.preprocessor :as preprocessor])
(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])
(require '[clojure.pprint :as pprint])

(defn debug-test-25 []
  (println "🔍 ДЕТАЛЬНАЯ ОТЛАДКА test_25_singleton_guard.c")
  (println "=" (apply str (repeat 60 "=")))
  
  (let [file-content (slurp "c51cc/test/ccode/test_25_singleton_guard.c")]
    
    ;; Шаг 1: Показываем исходный код
    (println "\n📄 ИСХОДНЫЙ КОД:")
    (println "-" (apply str (repeat 58 "-")))
    (println file-content)
    
    ;; Шаг 2: Препроцессор
    (println "\n📝 ПОСЛЕ ПРЕПРОЦЕССОРА:")
    (println "-" (apply str (repeat 58 "-")))
    (let [clean-content (-> file-content
                           preprocessor/remove-comments
                           preprocessor/normalize-whitespace)]
      (println clean-content)
      
      ;; Шаг 3: Токены
      (println "\n🔤 ТОКЕНЫ:")
      (println "-" (apply str (repeat 58 "-")))
      (let [tokens (lexer/tokenize clean-content)]
        (println (str "Всего токенов: " (count tokens)))
        (doseq [[i token] (map-indexed vector tokens)]
          (println (format "%2d: %s" (inc i) token)))
        
        ;; Шаг 4: Попытка парсинга с детальной диагностикой
        (println "\n🌳 ПАРСИНГ:")
        (println "-" (apply str (repeat 58 "-")))
        (try
          (let [parse-state (parser/parse-state tokens)]
            (println "Начальное состояние парсера:")
            (pprint/pprint (select-keys parse-state [:position :tokens-count]))
            
            (let [result (parser/parse-program parse-state)]
              (println "\nРезультат парсинга:")
              (if (:success? result)
                (do
                  (println "✅ Парсинг успешен")
                  (println "AST:")
                  (pprint/pprint (:value result)))
                (do
                  (println "❌ Ошибка парсинга:")
                  (pprint/pprint (:error result))))))
          (catch Exception e
            (println "💥 ИСКЛЮЧЕНИЕ при парсинге:")
            (println (.getMessage e))
            (.printStackTrace e)))))))

;; Запуск отладки
(debug-test-25)
```

Теперь запустите этот отладочный скрипт в REPL:

```clojure
(load-file "debug_test_25.clj")
```

Это покажет:
1. **Исходный код** файла
2. **Результат препроцессора** 
3. **Все 48 токенов** с их типами
4. **Детальную информацию об ошибке

```clojure
(load-file "debug_test_25.clj")
```

