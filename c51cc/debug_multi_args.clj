(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])
(require '[clojure.pprint :as pp])

(println "=== Отладка func(a, b, 42) ===")
(let [tokens (lexer/tokenize "func(a, b, 42)")]
  (println "Токены:")
  (pp/pprint tokens)
  (let [result (parser/parse tokens)]
    (println "\nРезультат:")
    (pp/pprint result)
    (when (:success result)
      (let [ast (:ast result)]
        (println "\nAST:")
        (pp/pprint ast)
        (when (seq (:declarations ast))
          (let [expr (:expression (first (:declarations ast)))]
            (println "\nВыражение:")
            (pp/pprint expr)
            (when (= :call-expression (:ast-type expr))
              (println "\nАргументы:")
              (pp/pprint (:arguments expr))))))))) 