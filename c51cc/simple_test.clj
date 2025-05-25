(require '[c51cc.lexer :as lexer])

(println "Testing lexer...")

;; Test simple number
(let [tokens (lexer/tokenize "42")]
  (println "Input: 42")
  (println "Tokens:" tokens)
  (println "First token:" (first tokens))
  (println "Token type:" (:type (first tokens)))
  (println "Token value:" (:value (first tokens))))

(println)

;; Test identifier
(let [tokens (lexer/tokenize "x")]
  (println "Input: x")
  (println "Tokens:" tokens)
  (println "First token:" (first tokens))
  (println "Token type:" (:type (first tokens)))
  (println "Token value:" (:value (first tokens))))

(println)

;; Test simple declaration
(let [tokens (lexer/tokenize "int x")]
  (println "Input: int x")
  (println "Tokens:" tokens)
  (doseq [[i token] (map-indexed vector tokens)]
    (println (str "Token " i ": " token))))

(println "Lexer test complete.") 