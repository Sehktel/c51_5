(require '[c51cc.lexer :as lexer])

(println "=== ТЕСТ ТОКЕНИЗАЦИИ FOR ===")
(def for-code "for (int i = 0; i < 10; i++) { }")
(let [tokens (lexer/tokenize for-code)]
  (println "Код:" for-code)
  (println "Токены:")
  (doseq [[i token] (map-indexed vector tokens)]
    (println (format "%2d: %s" i token)))) 