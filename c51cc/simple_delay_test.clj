(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(def test-program "sfr P0 = 0x80; void delay(unsigned int ms) using 1 { }")

(let [result (parser/parse (lexer/tokenize test-program))]
  (println "Программа:" test-program)
  (if (:success result)
    (println "✅ УСПЕХ!")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result))))) 