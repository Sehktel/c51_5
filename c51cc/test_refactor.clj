(require '[c51cc.parser :as p])
(require '[c51cc.lexer :as l])

(println "=== ТЕСТ РЕФАКТОРИНГА С МАКРОСОМ do-parse ===")

;; Тест parse-expression-statement
(println "\n1. Тест parse-expression-statement:")
(try
  (let [tokens (l/tokenize "x = 5;")
        state (p/parse-state tokens)
        result (p/parse-expression-statement state)]
    (println "Результат парсинга 'x = 5;':" (:success? result))
    (if (:success? result)
      (println "✅ parse-expression-statement работает")
      (println "❌ parse-expression-statement провалился:" (:error result))))
  (catch Exception e
    (println "❌ Ошибка:" (.getMessage e))))

;; Тест parse-return-statement
(println "\n2. Тест parse-return-statement:")
(try
  (let [tokens (l/tokenize "return x + 1;")
        state (p/parse-state tokens)
        result (p/parse-return-statement state)]
    (println "Результат парсинга 'return x + 1;':" (:success? result))
    (if (:success? result)
      (println "✅ parse-return-statement работает")
      (println "❌ parse-return-statement провалился:" (:error result))))
  (catch Exception e
    (println "❌ Ошибка:" (.getMessage e))))

;; Тест parse-while-statement
(println "\n3. Тест parse-while-statement:")
(try
  (let [tokens (l/tokenize "while (x > 0) x--;")
        state (p/parse-state tokens)
        result (p/parse-while-statement state)]
    (println "Результат парсинга 'while (x > 0) x--;':" (:success? result))
    (if (:success? result)
      (println "✅ parse-while-statement работает")
      (println "❌ parse-while-statement провалился:" (:error result))))
  (catch Exception e
    (println "❌ Ошибка:" (.getMessage e))))

;; Тест комплексной программы
(println "\n4. Тест комплексной программы:")
(try
  (let [code "int main() { int x = 5; while (x > 0) { x--; } return 0; }"
        tokens (l/tokenize code)
        result (p/parse tokens)]
    (println "Результат парсинга комплексной программы:" (:success result))
    (if (:success result)
      (println "✅ Комплексная программа работает")
      (println "❌ Комплексная программа провалилась:" (:error result))))
  (catch Exception e
    (println "❌ Ошибка:" (.getMessage e))))

(println "\n=== ТЕСТЫ ЗАВЕРШЕНЫ ===") 