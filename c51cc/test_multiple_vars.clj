(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ТЕСТ МНОЖЕСТВЕННЫХ ПЕРЕМЕННЫХ ===")
(def multiple-vars "void test() { unsigned int i, j; }")
(let [result (parser/parse (lexer/tokenize multiple-vars))]
  (if (:success result)
    (println "✅ УСПЕХ! Множественные переменные парсятся")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result)))))

(println "\n=== ТЕСТ ОТДЕЛЬНЫХ ПЕРЕМЕННЫХ ===")
(def separate-vars "void test() { unsigned int i; unsigned int j; }")
(let [result (parser/parse (lexer/tokenize separate-vars))]
  (if (:success result)
    (println "✅ УСПЕХ! Отдельные переменные парсятся")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result))))) 