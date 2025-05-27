(require '[c51cc.preprocessor-v2 :as pp])

(println "НАЧАЛО ТЕСТА")

(spit "test.h" "#define VAL 42")
(println "Файл создан")

(def test-code "#include \"test.h\"\nint x = VAL;")
(println "Код подготовлен:")
(println test-code)

(println "Вызываем препроцессор...")
(def result (pp/preprocess-v2 test-code {:include-paths ["."]}))
(println "Препроцессор завершен")

(println "Результат:")
(println (:result result))

(println "КОНЕЦ ТЕСТА") 