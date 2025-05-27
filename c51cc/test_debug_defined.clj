(require '[c51cc.preprocessor :as pp])

(println "=== Тест с определенным DEBUG ===")

(let [code "#define DEBUG\n#ifdef DEBUG\n#define LED P1_7\n#else\n#define LED P1_0\n#endif\nLED = 1;"
      result (pp/preprocess code)]
  (println "Код:" code)
  (println "Результат:" (pr-str result))
  (println "Содержит P1_7:" (clojure.string/includes? result "P1_7"))
  (println "Содержит P1_0:" (clojure.string/includes? result "P1_0"))
  (println "Содержит LED:" (clojure.string/includes? result "LED")))

(println "\n=== Тест завершен ===") 