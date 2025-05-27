(require 'c51cc.preprocessor)

(println "=== ТЕСТ ОБРАБОТКИ ОШИБОК ===\n")

;; 1. Тест успешного случая
(println "1. Успешный случай:")
(let [result (c51cc.preprocessor/preprocess "#define MAX 100\nint x = MAX;")]
  (println "Результат:" (:result result))
  (println "Успех:" (:success result))
  (println "Ошибки:" (:errors result))
  (println "Тип возврата:" (type result))
  (println))

;; 2. Тест с ошибкой #else без #if
(println "2. Ошибка #else без #if:")
(let [result (c51cc.preprocessor/preprocess "#else\nint x = 1;")]
  (println "Результат:" (:result result))
  (println "Успех:" (:success result))
  (println "Ошибки:" (:errors result))
  (println "Количество ошибок:" (count (:errors result)))
  (println))

;; 3. Тест с ошибкой #endif без #if
(println "3. Ошибка #endif без #if:")
(let [result (c51cc.preprocessor/preprocess "#endif\nint x = 1;")]
  (println "Результат:" (:result result))
  (println "Успех:" (:success result))
  (println "Ошибки:" (:errors result))
  (println))

;; 4. Тест с незакрытым условным блоком
(println "4. Незакрытый условный блок:")
(let [result (c51cc.preprocessor/preprocess "#ifdef DEBUG\nint x = 1;")]
  (println "Результат:" (:result result))
  (println "Успех:" (:success result))
  (println "Ошибки:" (:errors result))
  (println))

;; 5. Тест с директивой #error
(println "5. Директива #error:")
(let [result (c51cc.preprocessor/preprocess "#error Это тестовая ошибка")]
  (println "Результат:" (:result result))
  (println "Успех:" (:success result))
  (println "Ошибки:" (:errors result))
  (println))

;; 6. Тест с несуществующим include
(println "6. Несуществующий include:")
(let [result (c51cc.preprocessor/preprocess "#include \"nonexistent.h\"")]
  (println "Результат:" (:result result))
  (println "Успех:" (:success result))
  (println "Ошибки:" (:errors result))
  (println))

;; 7. Проверяем, что исключения НЕ выбрасываются
(println "7. Проверка отсутствия исключений:")
(try
  (let [result (c51cc.preprocessor/preprocess "#else\n#endif\n#error test")]
    (println "Никаких исключений не выброшено!")
    (println "Успех:" (:success result))
    (println "Количество ошибок:" (count (:errors result))))
  (catch Exception e
    (println "ВНИМАНИЕ: Выброшено исключение!" (.getMessage e))))

(println "\n=== АНАЛИЗ ЗАВЕРШЕН ===") 