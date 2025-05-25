(ns debug-expression-statement
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn -main []
  (let [input "x--;"
        tokens (lexer/tokenize input)
        initial-state (parser/parse-state tokens)]
    
    (println "=== ТЕСТИРОВАНИЕ ВЫРАЖЕНИЯ x--; ===")
    (println "Input:" input)
    (println "Tokens:" tokens)
    
    (println "\n=== ТЕСТ 1: parse-expression ===")
    (let [expr-result (parser/parse-expression initial-state)]
      (println "Success?:" (:success? expr-result))
      (if (:success? expr-result)
        (do
          (println "AST:" (:value expr-result))
          (println "Final position:" (:position (:state expr-result))))
        (println "Error:" (:error expr-result))))
    
    (println "\n=== ТЕСТ 2: parse-expression-statement ===")
    (let [stmt-result (parser/parse-expression-statement initial-state)]
      (println "Success?:" (:success? stmt-result))
      (if (:success? stmt-result)
        (do
          (println "AST:" (:value stmt-result))
          (println "Final position:" (:position (:state stmt-result))))
        (println "Error:" (:error stmt-result))))
    
    (println "\n=== ТЕСТ 3: parse-statement ===")
    (let [stmt-result (parser/parse-statement initial-state)]
      (println "Success?:" (:success? stmt-result))
      (if (:success? stmt-result)
        (do
          (println "AST:" (:value stmt-result))
          (println "Final position:" (:position (:state stmt-result))))
        (println "Error:" (:error stmt-result))))))

(-main) 