(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "=== Тестирование парсера C51 ===")

;; Тест 1: Простое выражение
(println "\n1. Тест простого выражения:")
(let [code "42"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Код:" code)
  (println "Токены:" tokens)
  (println "Результат:" (:success result))
  (when (:success result)
    (println "AST:" (:ast result))))

;; Тест 2: Объявление переменной
(println "\n2. Тест объявления переменной:")
(let [code "int x;"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Код:" code)
  (println "Токены:" tokens)
  (println "Результат:" (:success result))
  (when (:success result)
    (println "AST:" (:ast result)))
  (when (not (:success result))
    (println "Ошибка:" (:error result))))

;; Тест 3: Простая функция
(println "\n3. Тест простой функции:")
(let [code "void main() { return; }"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Код:" code)
  (println "Токены:" tokens)
  (println "Результат:" (:success result))
  (when (:success result)
    (println "AST:" (:ast result)))
  (when (not (:success result))
    (println "Ошибка:" (:error result))))

(println "\n=== Тестирование завершено ===") 