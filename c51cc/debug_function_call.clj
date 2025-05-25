(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])
(require '[clojure.pprint :as pp])

(defn debug-parse [input]
  (println "=== Отладка парсинга:" input "===")
  (let [tokens (lexer/tokenize input)]
    (println "Токены:")
    (pp/pprint tokens)
    (let [result (parser/parse tokens)]
      (println "\nРезультат парсинга:")
      (pp/pprint result)
      (when (:success result)
        (println "\nAST:")
        (pp/pprint (:ast result))
        (let [ast (:ast result)]
          (when (= :program (:ast-type ast))
            (println "\nДекларации:")
            (doseq [[i decl] (map-indexed vector (:declarations ast))]
              (println "Декларация" i ":" (:ast-type decl))
              (when (= :expression-statement (:ast-type decl))
                (println "  Выражение:" (:expression decl))
                (when (:expression decl)
                  (println "  Тип выражения:" (:ast-type (:expression decl)))))))))
      (println))))

;; Тестируем различные случаи
(debug-parse "42")
(debug-parse "func")
(debug-parse "func()")
(debug-parse "func(42)")
