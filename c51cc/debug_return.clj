(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])
(require '[clojure.pprint :as pp])

(println "=== Отладка return; ===")
(let [tokens (lexer/tokenize "return;")]
  (println "Токены:")
  (pp/pprint tokens)
  (let [result (parser/parse tokens)]
    (println "\nРезультат:")
    (pp/pprint result)
    (when (:success result)
      (println "\nAST:")
      (pp/pprint (:ast result)))))

(println "\n=== Отладка return 42; ===")
(let [tokens (lexer/tokenize "return 42;")]
  (println "Токены:")
  (pp/pprint tokens)
  (let [result (parser/parse tokens)]
    (println "\nРезультат:")
    (pp/pprint result)
    (when (:success result)
      (println "\nAST:")
      (pp/pprint (:ast result))))) 