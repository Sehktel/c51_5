(require '[c51cc.parser :as p])
(require '[c51cc.lexer :as l])

(println "=== ПРОСТОЙ ТЕСТ МАКРОСА ===")

;; Тест простейшего случая
(try
  (let [tokens (l/tokenize "int")
        state (p/parse-state tokens)
        ;; Создаем парсер вручную для сравнения
        manual-parser (fn [state]
                        (let [result ((p/expect-token :int) state)]
                          (if (:success? result)
                            (p/success (:value result) (:state result))
                            result)))
        manual-result (manual-parser state)]
    (println "Ручной парсер работает:" (:success? manual-result))
    
    ;; Теперь тестируем макрос
    (let [macro-parser (p/do-parse 
                         token (p/expect-token :int)
                         (p/return-parser token))
          macro-result (macro-parser state)]
      (println "Макрос работает:" (:success? macro-result))))
  (catch Exception e
    (println "Ошибка:" (.getMessage e)))) 