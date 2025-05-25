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