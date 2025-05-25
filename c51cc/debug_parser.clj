(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

;; Функции-помощники из тестов
(defn parse-string [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn parse-successful? [result]
  (:success result))

(defn get-ast [result]
  (:ast result))

(defn get-error [result]
  (:error result))

(println "🔍 ОТЛАДКА ПАРСЕРА")
(println "==================")

;; Тест 1: Простое выражение
(println "\n1. Тестируем простое выражение:")
(let [result (parse-string "42")]
  (println (str "Ввод: '42'"))
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (do
      (println (str "AST: " (get-ast result)))
      (parser/pretty-print-ast (get-ast result)))
    (println (str "Ошибка: " (get-error result)))))

;; Тест 2: Объявление переменной
(println "\n2. Тестируем объявление переменной:")
(let [result (parse-string "int x = 42;")]
  (println (str "Ввод: 'int x = 42;'"))
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (do
      (println (str "AST: " (get-ast result)))
      (parser/pretty-print-ast (get-ast result)))
    (println (str "Ошибка: " (get-error result)))))

;; Тест 3: Простая функция
(println "\n3. Тестируем простую функцию:")
(let [result (parse-string "void main() { }")]
  (println (str "Ввод: 'void main() { }'"))
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (do
      (println (str "AST: " (get-ast result)))
      (parser/pretty-print-ast (get-ast result)))
    (println (str "Ошибка: " (get-error result)))))

;; Тест 4: Функция с параметрами
(println "\n4. Тестируем функцию с параметрами:")
(let [result (parse-string "int add(int a, int b) { return a + b; }")]
  (println (str "Ввод: 'int add(int a, int b) { return a + b; }'"))
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (do
      (println (str "AST: " (get-ast result)))
      (parser/pretty-print-ast (get-ast result)))
    (println (str "Ошибка: " (get-error result)))))

;; Тест 5: Блок операторов
(println "\n5. Тестируем блок операторов:")
(let [result (parse-string "{ int x = 1; int y = 2; }")]
  (println (str "Ввод: '{ int x = 1; int y = 2; }'"))
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (do
      (println (str "AST: " (get-ast result)))
      (parser/pretty-print-ast (get-ast result)))
    (println (str "Ошибка: " (get-error result)))))

(println "\n✅ Отладка завершена")

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

(debug-parse "for (i = 0; i < 10; i++) sum += i;")
(debug-parse "int x = 42;")
(debug-parse "void main() { }") 