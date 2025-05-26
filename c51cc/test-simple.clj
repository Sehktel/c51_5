(require '[c51cc.lexer :as lexer]
         '[c51cc.parser :as parser])

(println "🧪 Тестирование рефакторинга парсера...")

;; Тест лексера
(println "\n1. Тестирование лексера:")
(let [tokens (lexer/tokenize "int x")]
  (if (vector? tokens)
    (do
      (println "✅ Лексер работает")
      (println "Токены:" tokens))
    (println "❌ Ошибка лексера:" tokens)))

;; Тест парсера типов
(println "\n2. Тестирование parse-type-specifier:")
(let [tokens (lexer/tokenize "unsigned int")
      state (parser/parse-state tokens)
      result (parser/parse-type-specifier state)]
  (if (:success? result)
    (do
      (println "✅ parse-type-specifier работает")
      (println "Результат:" (:value result)))
    (println "❌ Ошибка parse-type-specifier:" (:error result))))

;; Тест парсера выражений
(println "\n3. Тестирование parse-unary-expression:")
(let [tokens (lexer/tokenize "++x")
      state (parser/parse-state tokens)
      result (parser/parse-unary-expression state)]
  (if (:success? result)
    (do
      (println "✅ parse-unary-expression работает")
      (println "Результат:" (:value result)))
    (println "❌ Ошибка parse-unary-expression:" (:error result))))

;; Тест парсера постфиксных выражений
(println "\n4. Тестирование parse-postfix-expression:")
(let [tokens (lexer/tokenize "func()")
      state (parser/parse-state tokens)
      result (parser/parse-postfix-expression state)]
  (if (:success? result)
    (do
      (println "✅ parse-postfix-expression работает")
      (println "Результат:" (:value result)))
    (println "❌ Ошибка parse-postfix-expression:" (:error result))))

;; Тест парсера мультипликативных выражений
(println "\n5. Тестирование parse-multiplicative-expression:")
(let [tokens (lexer/tokenize "a * b")
      state (parser/parse-state tokens)
      result (parser/parse-multiplicative-expression state)]
  (if (:success? result)
    (do
      (println "✅ parse-multiplicative-expression работает")
      (println "Результат:" (:value result)))
    (println "❌ Ошибка parse-multiplicative-expression:" (:error result))))

(println "\n🎉 Тестирование завершено!") 