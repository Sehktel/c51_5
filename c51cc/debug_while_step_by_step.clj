(ns debug-while-step-by-step
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn -main []
  (let [input "while (x > 0) x--;"
        tokens (lexer/tokenize input)
        initial-state (parser/parse-state tokens)]
    
    (println "=== ВХОДНЫЕ ДАННЫЕ ===")
    (println "Input:" input)
    (println "Tokens:" tokens)
    (println "Initial state position:" (:position initial-state))
    
    (println "\n=== ТЕСТ 1: parse-while-statement ===")
    (let [while-result (parser/parse-while-statement initial-state)]
      (println "Success?:" (:success? while-result))
      (if (:success? while-result)
        (do
          (println "AST:" (:value while-result))
          (println "Final position:" (:position (:state while-result))))
        (println "Error:" (:error while-result))))
    
    (println "\n=== ТЕСТ 2: parse-statement ===")
    (let [stmt-result (parser/parse-statement initial-state)]
      (println "Success?:" (:success? stmt-result))
      (if (:success? stmt-result)
        (do
          (println "AST:" (:value stmt-result))
          (println "Final position:" (:position (:state stmt-result))))
        (println "Error:" (:error stmt-result))))
    
    (println "\n=== ТЕСТ 3: parse-declaration ===")
    (let [decl-result (parser/parse-declaration initial-state)]
      (println "Success?:" (:success? decl-result))
      (if (:success? decl-result)
        (do
          (println "AST:" (:value decl-result))
          (println "Final position:" (:position (:state decl-result))))
        (println "Error:" (:error decl-result))))
    
    (println "\n=== ТЕСТ 4: many parse-declaration ===")
    (let [many-result ((parser/many parser/parse-declaration) initial-state)]
      (println "Success?:" (:success? many-result))
      (println "Count:" (count (:value many-result)))
      (println "Final position:" (:position (:state many-result)))
      (when (> (count (:value many-result)) 0)
        (println "First declaration:" (first (:value many-result)))))
    
    (println "\n=== ТЕСТ 5: parse (полный парсинг) ===")
    (let [full-result (parser/parse tokens)]
      (println "Success?:" (:success full-result))
      (if (:success full-result)
        (do
          (println "AST type:" (:ast-type (:ast full-result)))
          (println "Declarations count:" (count (:declarations (:ast full-result))))
          (when (> (count (:declarations (:ast full-result))) 0)
            (println "First declaration:" (first (:declarations (:ast full-result))))))
        (println "Error:" (:error full-result))))))

(-main) 