(require '[c51cc.preprocessor-v2 :as pp])

(println "=== ИЗОЛИРОВАННАЯ ОТЛАДКА ТЕСТА 2 ===\n")

;; Точно воспроизводим тест 2
(println "Шаг 1: Создаем файл simple.h")
(spit "simple.h" "#define SIMPLE_VALUE 42")
(println "Файл создан. Содержимое:")
(println (slurp "simple.h"))

(println "\nШаг 2: Подготавливаем код для обработки")
(def code "#include \"simple.h\"\nint x = SIMPLE_VALUE;")
(println "Код для обработки:")
(println code)

(println "\nШаг 3: Вызываем preprocess-v2")
(try
  (let [result (pp/preprocess-v2 code {:include-paths ["."]})]
    (println "Результат получен успешно:")
    (println "  result:" (:result result))
    (println "  success:" (:success result))
    (println "  errors:" (:errors result))
    (println "  errors count:" (count (:errors result))))
  (catch Exception e
    (println "ИСКЛЮЧЕНИЕ при вызове preprocess-v2:")
    (println "  Тип:" (type e))
    (println "  Сообщение:" (.getMessage e))
    (println "  Стек:" (take 5 (.getStackTrace e)))))

(println "\nШаг 4: Проверяем expand-includes отдельно")
(try
  (let [initial-state (pp/create-initial-state :include-paths ["."])
        expand-result (pp/expand-includes code initial-state)]
    (println "expand-includes результат:")
    (println "  content:" (:content expand-result))
    (println "  defines:" (:defines (:state expand-result)))
    (println "  errors:" (:errors expand-result)))
  (catch Exception e
    (println "ИСКЛЮЧЕНИЕ в expand-includes:")
    (println "  Тип:" (type e))
    (println "  Сообщение:" (.getMessage e))))

(println "\n=== ОТЛАДКА ЗАВЕРШЕНА ===") 