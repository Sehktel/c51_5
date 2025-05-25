(ns debug-while-detailed
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn test-parse-while-statement []
  (println "=== Тестирование parse-while-statement напрямую ===")
  (let [input "while (x > 0) x--;"
        tokens (lexer/tokenize input)
        initial-state (parser/parse-state tokens)]
    (println "Tokens:" tokens)
    (println "Initial state:" initial-state)
    
    ;; Тестируем parse-while-statement напрямую
    (let [while-result (parser/parse-while-statement initial-state)]
      (println "parse-while-statement result:" while-result)
      (when (:success? while-result)
        (println "While AST:" (:value while-result))))))

(defn test-parse-statement []
  (println "\n=== Тестирование parse-statement ===")
  (let [input "while (x > 0) x--;"
        tokens (lexer/tokenize input)
        initial-state (parser/parse-state tokens)]
    
    ;; Тестируем parse-statement
    (let [stmt-result (parser/parse-statement initial-state)]
      (println "parse-statement result:" stmt-result)
      (when (:success? stmt-result)
        (println "Statement AST:" (:value stmt-result))))))

(defn test-parse-declaration []
  (println "\n=== Тестирование parse-declaration ===")
  (let [input "while (x > 0) x--;"
        tokens (lexer/tokenize input)
        initial-state (parser/parse-state tokens)]
    
    ;; Тестируем parse-declaration
    (let [decl-result (parser/parse-declaration initial-state)]
      (println "parse-declaration result:" decl-result)
      (when (:success? decl-result)
        (println "Declaration AST:" (:value decl-result))))))

(defn test-many-parse-declaration []
  (println "\n=== Тестирование many parse-declaration ===")
  (let [input "while (x > 0) x--;"
        tokens (lexer/tokenize input)
        initial-state (parser/parse-state tokens)]
    
    ;; Тестируем many parse-declaration
    (let [many-result ((parser/many parser/parse-declaration) initial-state)]
      (println "many parse-declaration result:" many-result)
      (when (:success? many-result)
        (println "Many declarations AST:" (:value many-result))
        (println "Count:" (count (:value many-result)))))))

(defn -main []
  (test-parse-while-statement)
  (test-parse-statement)
  (test-parse-declaration)
  (test-many-parse-declaration))

(-main) 