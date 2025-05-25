(ns debug-test
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn parse-string [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn parse-successful? [result]
  (:success result))

(defn get-ast [result]
  (:ast result))

;; Тестируем вызов функции
(let [result (parse-string "func()")]
  (println "Результат парсинга func():")
  (println result)
  (println "Успешен?" (parse-successful? result))
  (when (parse-successful? result)
    (let [ast (get-ast result)]
      (println "AST:" ast)
      (println "Первая декларация:" (first (:declarations ast)))
      (let [first-decl (first (:declarations ast))]
        (println "Тип первой декларации:" (:ast-type first-decl))
        (when (= :expression-statement (:ast-type first-decl))
          (let [expr (:expression first-decl)]
            (println "Выражение:" expr)
            (println "Тип выражения:" (:ast-type expr))
            (when (= :call-expression (:ast-type expr))
              (println "Callee:" (:callee expr))
              (println "Arguments:" (:arguments expr)))))))))

(require '[c51cc.parser :as p])
(require '[c51cc.lexer :as l])

(println "=== Отладочный тест макроса do-parse ===")

;; Проверим макроэкспансию простого случая
(println "\nМакроэкспансия простого случая:")
(try
  (let [expanded (macroexpand '(p/do-parse 
                                 (p/return-parser :test)))]
    (println "Результат:" expanded))
  (catch Exception e
    (println "Ошибка:" (.getMessage e))))

;; Проверим макроэкспансию с одной парой
(println "\nМакроэкспансия с одной парой:")
(try
  (let [expanded (macroexpand '(p/do-parse 
                                 x (p/expect-token :int)
                                 (p/return-parser x)))]
    (println "Результат:" (take 100 (str expanded))))
  (catch Exception e
    (println "Ошибка:" (.getMessage e))))

;; Проверим работу return-parser
(println "\nПроверка return-parser:")
(try
  (let [tokens (l/tokenize "int")
        state (p/parse-state tokens)
        parser (p/return-parser :test)
        result (parser state)]
    (println "Результат return-parser:" result))
  (catch Exception e
    (println "Ошибка return-parser:" (.getMessage e))))

;; Проверим работу expect-token
(println "\nПроверка expect-token:")
(try
  (let [tokens (l/tokenize "int")
        state (p/parse-state tokens)
        parser (p/expect-token :int)
        result (parser state)]
    (println "Результат expect-token:" result))
  (catch Exception e
    (println "Ошибка expect-token:" (.getMessage e)))) 