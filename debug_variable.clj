(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ТЕСТ ПЕРЕМЕННОЙ UNSIGNED CHAR ===")

(def var-code "unsigned char timer_count = 0;")

(let [tokens (lexer/tokenize var-code)
      result (parser/parse tokens)]
  (println "Код:" var-code)
  (println "Токены:")
  (doseq [[i token] (map-indexed vector tokens)]
    (println (format "%2d: %s" i token)))
  
  (if (:success result)
    (do
      (println "✅ УСПЕХ!")
      (println "AST:" (:ast result)))
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result))))) 