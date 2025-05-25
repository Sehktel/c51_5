(ns test-while
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn -main []
  (let [input "while (x > 0) x--;"
        tokens (lexer/tokenize input)
        result (parser/parse tokens)]
    (println "Input:" input)
    (println "Tokens:" tokens)
    (println "Parse successful?:" (:success result))
    (println "AST:" (:ast result))
    (when (:success result)
      (let [declarations (:declarations (:ast result))]
        (println "Declarations count:" (count declarations))
        (println "First declaration:" (first declarations))
        (when-let [first-decl (first declarations)]
          (println "First declaration type:" (:ast-type first-decl)))))))

(-main) 