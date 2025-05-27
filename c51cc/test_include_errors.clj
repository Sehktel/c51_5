(require 'c51cc.preprocessor)

(println "=== ТЕСТ ОШИБОК INCLUDE ===\n")

;; Проверяем, что происходит с несуществующими файлами
(println "Тест несуществующего файла:")
(try
  (let [result (c51cc.preprocessor/preprocess "#include \"nonexistent.h\"\nint x = 1;")]
    (println "Результат:" (:result result))
    (println "Успех:" (:success result))
    (println "Ошибки:" (:errors result))
    (println "Исключение НЕ выброшено - хорошо!"))
  (catch Exception e
    (println "ПРОБЛЕМА: Выброшено исключение:" (.getMessage e))
    (println "Тип исключения:" (type e))))

(println "\n=== АНАЛИЗ INCLUDE ЗАВЕРШЕН ===") 