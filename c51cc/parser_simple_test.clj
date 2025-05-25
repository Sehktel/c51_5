(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "Testing parser...")

;; Test 1: Simple number
(println "\n=== Test 1: Simple number ===")
(let [tokens (lexer/tokenize "42")
      result (parser/parse tokens)]
  (println "Input: 42")
  (println "Tokens:" tokens)
  (println "Parse success:" (:success result))
  (if (:success result)
    (do
      (println "AST:" (:ast result))
      (println "Declarations count:" (count (:declarations (:ast result)))))
    (println "Error:" (:error result))))

;; Test 2: Simple variable declaration
(println "\n=== Test 2: Variable declaration ===")
(let [tokens (lexer/tokenize "int x;")
      result (parser/parse tokens)]
  (println "Input: int x;")
  (println "Tokens:" tokens)
  (println "Parse success:" (:success result))
  (if (:success result)
    (do
      (println "AST:" (:ast result))
      (println "Declarations count:" (count (:declarations (:ast result)))))
    (println "Error:" (:error result))))

;; Test 3: Simple function
(println "\n=== Test 3: Simple function ===")
(let [tokens (lexer/tokenize "void main() { return; }")
      result (parser/parse tokens)]
  (println "Input: void main() { return; }")
  (println "Tokens count:" (count tokens))
  (println "Parse success:" (:success result))
  (if (:success result)
    (do
      (println "AST:" (:ast result))
      (println "Declarations count:" (count (:declarations (:ast result)))))
    (println "Error:" (:error result))))

(println "\nParser test complete.") 