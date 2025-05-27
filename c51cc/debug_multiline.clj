(println "Тест многострочных строк:")

(println "Тест 1: Простая строка")
(println "Одна строка")

(println "Тест 2: Строка с \\n")
(println "Строка с\nпереносом")

(println "Тест 3: Код как в тесте")
(def test-code "#include \"test.h\"\nint x = VAL;")
(println "Код:")
(println test-code)

(println "Тест 4: Построчный вывод")
(doseq [line (clojure.string/split-lines test-code)]
  (println "  " line))

(println "КОНЕЦ") 