(ns test-function-access
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn test-function-availability []
  (println "Проверяем доступность функций парсера:")
  (println "parse:" (type parser/parse))
  (println "parse-state:" (try (type parser/parse-state) (catch Exception e "Недоступна")))
  (println "parse-while-statement:" (try (type parser/parse-while-statement) (catch Exception e "Недоступна")))
  (println "parse-statement:" (try (type parser/parse-statement) (catch Exception e "Недоступна")))
  (println "parse-declaration:" (try (type parser/parse-declaration) (catch Exception e "Недоступна")))
  (println "many:" (try (type parser/many) (catch Exception e "Недоступна"))))

(defn -main []
  (test-function-availability))

(-main) 