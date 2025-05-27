(require '[clojure.string :as str])
(require '[c51cc.preprocessor-v2 :as pp])

;; Используем наш ФП конвейер
(defn safe-println
  "Безопасный вывод многострочных строк"
  [label text]
  (->> text
       str/split-lines
       (map #(str "  " %))
       (cons label)
       (str/join "\n")
       println)
  text)

(println "=== ПРОВЕРКА ПРОБЛЕМНОГО СЛУЧАЯ ===")

;; Воспроизводим точно тот же случай, что был проблемным
(spit "simple.h" "#define SIMPLE_VALUE 42")

(def problematic-code "#include \"simple.h\"\nint x = SIMPLE_VALUE;")

(println "Проблемный код:")
(safe-println "Код:" problematic-code)

(println "\nОбрабатываем через препроцессор:")
(let [result (pp/preprocess-v2 problematic-code {:include-paths ["."]})]
  (safe-println "Результат:" (:result result))
  (println "Успех:" (:success result))
  (println "Ошибки:" (count (:errors result)))
  (println "Длина результата:" (count (:result result)))
  
  ;; Проверим, что макрос действительно заменился
  (if (str/includes? (:result result) "42")
    (println "✅ МАКРОС ЗАМЕНИЛСЯ ПРАВИЛЬНО!")
    (println "❌ Макрос не заменился"))
  
  ;; Проверим построчно
  (println "\nПострочный анализ результата:")
  (doseq [line (str/split-lines (:result result))]
    (println "  Строка:" (pr-str line))))

(println "\n=== ПРОВЕРКА ЗАВЕРШЕНА ===") 