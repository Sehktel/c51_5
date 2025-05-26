(require '[c51cc.lexer :as lexer])

(println "Тестирование лексера...")
(let [result (lexer/tokenize "int x")]
  (println "Результат:" result))

(println "Готово!") 