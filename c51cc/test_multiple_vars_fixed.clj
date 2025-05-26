(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ТЕСТ ИСПРАВЛЕННЫХ МНОЖЕСТВЕННЫХ ПЕРЕМЕННЫХ ===")
(def multiple-vars "void test() { unsigned int i, j; }")
(let [result (parser/parse (lexer/tokenize multiple-vars))]
  (if (:success result)
    (do
      (println "✅ УСПЕХ! Множественные переменные парсятся")
      (println "AST:" (:ast result)))
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result)))))

(println "\n=== ТЕСТ ФУНКЦИИ DELAY С МНОЖЕСТВЕННЫМИ ПЕРЕМЕННЫМИ ===")
(def delay-with-vars "void delay(unsigned int ms) using 1 { unsigned int i, j; }")
(let [result (parser/parse (lexer/tokenize delay-with-vars))]
  (if (:success result)
    (println "✅ УСПЕХ! Delay с множественными переменными парсится")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result))))) 