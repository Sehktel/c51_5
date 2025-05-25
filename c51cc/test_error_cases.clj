(ns test-error-cases
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn parse-string [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn parse-successful? [result]
  (:success result))

(defn get-error [result]
  (:error result))

(defn -main []
  (println "=== ТЕСТИРОВАНИЕ СЛУЧАЕВ ОШИБОК ===")
  
  (println "\n1. Отсутствующая скобка: func(")
  (let [result (parse-string "func(")]
    (println "Success?:" (parse-successful? result))
    (println "Error:" (get-error result)))
  
  (println "\n2. Неожиданный токен: int + 42")
  (let [result (parse-string "int + 42")]
    (println "Success?:" (parse-successful? result))
    (println "Error:" (get-error result)))
  
  (println "\n3. Отсутствующая точка с запятой: int x = 42")
  (let [result (parse-string "int x = 42")]
    (println "Success?:" (parse-successful? result))
    (println "Error:" (get-error result)))
  
  (println "\n4. Пустой ввод:")
  (let [result (parse-string "")]
    (println "Success?:" (parse-successful? result))
    (println "Error:" (get-error result))))

(-main) 