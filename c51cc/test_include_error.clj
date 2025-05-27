(require '[c51cc.preprocessor :as pp])

(println "=== Тест обработки несуществующих файлов ===")

(try
  (let [result (pp/preprocess "#include \"nonexistent.h\"" {:include-paths ["test/ccode"]})]
    (println "НЕОЖИДАННО: Файл обработался без ошибки!")
    (println "Результат:" (pr-str result)))
  (catch Exception e
    (println "ОЖИДАЕМО: Поймано исключение")
    (println "Сообщение:" (.getMessage e))
    (println "Тип:" (type e))))

(println "\n=== Тест завершен ===") 