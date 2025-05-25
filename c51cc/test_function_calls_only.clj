(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(defn parse-string [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn parse-successful? [result]
  (:success result))

(defn get-ast [result]
  (:ast result))

;; Тест 1: Вызов функции без аргументов
(println "=== Тест 1: func() ===")
(let [result (parse-string "func()")]
  (println "Успешен:" (parse-successful? result))
  (when (parse-successful? result)
    (let [ast (get-ast result)
          expr (:expression (first (:declarations ast)))]
      (println "Тип выражения:" (:ast-type expr))
      (println "Имя функции:" (:name (:callee expr)))
      (println "Количество аргументов:" (count (:arguments expr))))))

;; Тест 2: Вызов функции с одним аргументом
(println "\n=== Тест 2: func(42) ===")
(let [result (parse-string "func(42)")]
  (println "Успешен:" (parse-successful? result))
  (when (parse-successful? result)
    (let [ast (get-ast result)
          expr (:expression (first (:declarations ast)))]
      (println "Тип выражения:" (:ast-type expr))
      (println "Имя функции:" (:name (:callee expr)))
      (println "Количество аргументов:" (count (:arguments expr)))
      (println "Значение первого аргумента:" (:value (first (:arguments expr)))))))

;; Тест 3: Вызов функции с несколькими аргументами
(println "\n=== Тест 3: func(a, b, 42) ===")
(let [result (parse-string "func(a, b, 42)")]
  (println "Успешен:" (parse-successful? result))
  (when (parse-successful? result)
    (let [ast (get-ast result)
          expr (:expression (first (:declarations ast)))]
      (println "Тип выражения:" (:ast-type expr))
      (println "Имя функции:" (:name (:callee expr)))
      (println "Количество аргументов:" (count (:arguments expr))))))

(println "\nВсе тесты вызовов функций завершены.") 