(require '[c51cc.preprocessor-v2 :as pp])

(println "=== ДЕТАЛЬНЫЙ ТЕСТ ДИРЕКТИВЫ #INCLUDE ===\n")

;; Тест 1: Базовая функциональность
(println "ТЕСТ 1: Базовая обработка макросов")
(let [result (pp/preprocess-v2 "#define TEST 123\nint x = TEST;")]
  (println "  Код:" "#define TEST 123\\nint x = TEST;")
  (println "  Результат:" (:result result))
  (println "  Успех:" (:success result))
  (println "  Ошибки:" (count (:errors result)))
  (println))

;; Тест 2: Include простого файла
(println "ТЕСТ 2: Include простого файла")
(spit "simple.h" "#define SIMPLE_VALUE 42")
(let [code "#include \"simple.h\"\nint x = SIMPLE_VALUE;"
      result (pp/preprocess-v2 code {:include-paths ["."]})]
  (println "  Заголовок simple.h:" "#define SIMPLE_VALUE 42")
  (println "  Код:" code)
  (println "  Результат:" (:result result))
  (println "  Успех:" (:success result))
  (println "  Ошибки:" (:errors result))
  (println))

;; Тест 3: Include guards (двойное включение)
(println "ТЕСТ 3: Include guards")
(spit "protected.h" "#ifndef PROTECTED_H\n#define PROTECTED_H\n#define PROTECTED_VALUE 99\n#endif")
(let [code "#include \"protected.h\"\n#include \"protected.h\"\nint y = PROTECTED_VALUE;"
      result (pp/preprocess-v2 code {:include-paths ["."]})]
  (println "  Заголовок protected.h с include guards")
  (println "  Код с двойным включением:" code)
  (println "  Результат:" (:result result))
  (println "  Успех:" (:success result))
  (println "  Ошибки:" (:errors result))
  (println))

;; Тест 4: Несуществующий файл
(println "ТЕСТ 4: Несуществующий файл")
(let [code "#include \"nonexistent.h\"\nint z = 1;"
      result (pp/preprocess-v2 code {:include-paths ["."]})]
  (println "  Код:" code)
  (println "  Результат:" (:result result))
  (println "  Успех:" (:success result))
  (println "  Ошибки:" (count (:errors result)))
  (when (seq (:errors result))
    (println "  Первая ошибка:" (:message (first (:errors result)))))
  (println))

;; Тест 5: Вложенные включения
(println "ТЕСТ 5: Вложенные включения")
(spit "inner.h" "#define INNER_VALUE 123")
(spit "outer.h" "#include \"inner.h\"\n#define OUTER_VALUE INNER_VALUE")
(let [code "#include \"outer.h\"\nint result = OUTER_VALUE;"
      result (pp/preprocess-v2 code {:include-paths ["."]})]
  (println "  inner.h:" "#define INNER_VALUE 123")
  (println "  outer.h:" "#include \"inner.h\"\\n#define OUTER_VALUE INNER_VALUE")
  (println "  Код:" code)
  (println "  Результат:" (:result result))
  (println "  Успех:" (:success result))
  (println "  Ошибки:" (:errors result))
  (println))

(println "=== ВСЕ ТЕСТЫ ЗАВЕРШЕНЫ ===") 