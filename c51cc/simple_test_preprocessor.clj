(require '[c51cc.preprocessor :as pp])

(println "=== Тест препроцессора ===")

;; Тест 1: Простые макросы
(println "\n1. Тест простых макросов:")
(let [code "#define MAX_SIZE 100\nint array[MAX_SIZE];"
      result (pp/preprocess code)]
  (println "Исходный код:" code)
  (println "Результат:" result)
  (println "Успех:" (clojure.string/includes? result "array[100]")))

;; Тест 2: Условная компиляция
(println "\n2. Тест условной компиляции:")
(let [code "#define DEBUG\n#ifdef DEBUG\nprintf(\"debug\");\n#endif"
      result (pp/preprocess code)]
  (println "Исходный код:" code)
  (println "Результат:" result)
  (println "Успех:" (clojure.string/includes? result "printf")))

;; Тест 3: Предопределенные макросы
(println "\n3. Тест предопределенных макросов:")
(let [code "printf(__FILE__);"
      result (pp/preprocess code {:current-file "test.c"})]
  (println "Исходный код:" code)
  (println "Результат:" result)
  (println "Успех:" (clojure.string/includes? result "test.c")))

;; Тест 4: Обработка ошибок
(println "\n4. Тест обработки ошибок:")
(try
  (pp/preprocess "#ifdef DEBUG\ncode\n") ; Незакрытый блок
  (println "ОШИБКА: Должно было выбросить исключение!")
  (catch Exception e
    (println "Успех: Поймано ожидаемое исключение:" (.getMessage e))))

(println "\n=== Тесты завершены ===") 