(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ТЕСТ ПРОСТОГО FOR ===")
(def simple-for "for (int i = 0; i < 10; i++) { }")
(let [result (parser/parse (lexer/tokenize simple-for))]
  (if (:success result)
    (println "✅ УСПЕХ! Простой for парсится")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result)))))

(println "\n=== ТЕСТ FOR В ФУНКЦИИ ===")
(def for-in-function "void test() { for (int i = 0; i < 10; i++) { } }")
(let [result (parser/parse (lexer/tokenize for-in-function))]
  (if (:success result)
    (println "✅ УСПЕХ! For в функции парсится")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result))))) 