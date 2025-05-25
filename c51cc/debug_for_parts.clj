(ns debug-for-parts
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn -main []
  (println "=== ТЕСТИРОВАНИЕ ЧАСТЕЙ FOR-ЦИКЛА ===")
  
  (println "\n1. Инициализация: i = 0")
  (let [tokens (lexer/tokenize "i = 0")
        initial-state (parser/parse-state tokens)
        expr-result (parser/parse-expression initial-state)]
    (println "Success?:" (:success? expr-result))
    (if (:success? expr-result)
      (do
        (println "AST:" (:value expr-result))
        (println "Final position:" (:position (:state expr-result))))
      (println "Error:" (:error expr-result))))
  
  (println "\n2. Условие: i < 10")
  (let [tokens (lexer/tokenize "i < 10")
        initial-state (parser/parse-state tokens)
        expr-result (parser/parse-expression initial-state)]
    (println "Success?:" (:success? expr-result))
    (if (:success? expr-result)
      (do
        (println "AST:" (:value expr-result))
        (println "Final position:" (:position (:state expr-result))))
      (println "Error:" (:error expr-result))))
  
  (println "\n3. Обновление: i++")
  (let [tokens (lexer/tokenize "i++")
        initial-state (parser/parse-state tokens)
        expr-result (parser/parse-expression initial-state)]
    (println "Success?:" (:success? expr-result))
    (if (:success? expr-result)
      (do
        (println "AST:" (:value expr-result))
        (println "Final position:" (:position (:state expr-result))))
      (println "Error:" (:error expr-result))))
  
  (println "\n4. Тело: sum += i")
  (let [tokens (lexer/tokenize "sum += i")
        initial-state (parser/parse-state tokens)
        expr-result (parser/parse-expression initial-state)]
    (println "Success?:" (:success? expr-result))
    (if (:success? expr-result)
      (do
        (println "AST:" (:value expr-result))
        (println "Final position:" (:position (:state expr-result))))
      (println "Error:" (:error expr-result))))
  
  (println "\n5. Тело как statement: sum += i;")
  (let [tokens (lexer/tokenize "sum += i;")
        initial-state (parser/parse-state tokens)
        stmt-result (parser/parse-statement initial-state)]
    (println "Success?:" (:success? stmt-result))
    (if (:success? stmt-result)
      (do
        (println "AST:" (:value stmt-result))
        (println "Final position:" (:position (:state stmt-result))))
      (println "Error:" (:error stmt-result)))))

(-main) 