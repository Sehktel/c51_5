(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ТЕСТ ПЕРЕМЕННОЙ В ФУНКЦИИ ===")
(def var-in-function "void test() { int i; }")
(let [result (parser/parse (lexer/tokenize var-in-function))]
  (if (:success result)
    (println "✅ УСПЕХ! Переменная в функции парсится")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result)))))

(println "\n=== ТЕСТ ДВУХ ПЕРЕМЕННЫХ В ФУНКЦИИ ===")
(def two-vars "void test() { int i; int j; }")
(let [result (parser/parse (lexer/tokenize two-vars))]
  (if (:success result)
    (println "✅ УСПЕХ! Две переменные в функции парсятся")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result))))) 