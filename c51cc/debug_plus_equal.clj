(ns debug-plus-equal
  (:require [c51cc.lexer :as lexer]))

(defn -main []
  (println "=== ТЕСТИРОВАНИЕ ТОКЕНИЗАЦИИ += ===")
  
  (println "\n1. Токенизация: sum += i")
  (let [tokens (lexer/tokenize "sum += i")]
    (println "Tokens:" tokens))
  
  (println "\n2. Токенизация: sum + = i")
  (let [tokens (lexer/tokenize "sum + = i")]
    (println "Tokens:" tokens))
  
  (println "\n3. Токенизация: sum = sum + i")
  (let [tokens (lexer/tokenize "sum = sum + i")]
    (println "Tokens:" tokens)))

(-main) 