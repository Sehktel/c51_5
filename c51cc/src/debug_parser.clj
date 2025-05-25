(ns debug-parser
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]
            [clojure.pprint :as pp]))

(defn test-parse [input]
  (println "=== Testing input:" input "===")
  (let [tokens (lexer/tokenize input)]
    (println "Tokens:")
    (pp/pprint tokens)
    (let [result (parser/parse tokens)]
      (println "Parse result:")
      (pp/pprint result)
      (when (:success result)
        (println "AST:")
        (pp/pprint (:ast result)))
      (println))))

;; Test simple cases
(test-parse "42")
(test-parse "x")
(test-parse "x + y")
(test-parse "func()")
(test-parse "int x;")
(test-parse "void main() { }") 