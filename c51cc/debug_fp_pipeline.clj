(require '[clojure.string :as str])
(require '[c51cc.preprocessor-v2 :as pp])

;; ============================================================================
;; ФУНКЦИОНАЛЬНЫЙ КОНВЕЙЕР ДЛЯ БЕЗОПАСНОГО ВЫВОДА
;; ============================================================================

(defn safe-println
  "Безопасный вывод строки с обработкой многострочности через ФП конвейер"
  [label text]
  (->> text
       str/split-lines           ; Разбиваем на строки
       (map #(str "  " %))      ; Добавляем отступ к каждой строке
       (cons label)             ; Добавляем заголовок в начало
       (str/join "\n")          ; Собираем обратно в строку
       println)                 ; Выводим безопасно
  text)                         ; Возвращаем исходный текст для конвейера

(defn debug-preprocess
  "Отладочная функция с ФП конвейером для препроцессора"
  [code options]
  (->> code
       (#(pp/preprocess-v2 % options))  ; Исправлен порядок аргументов
       ((fn [result]                    ; Выводим результат через конвейер
          (safe-println "Результат:" (:result result))
          (println "Успех:" (:success result))
          (println "Ошибки:" (count (:errors result)))
          result))))                   ; Возвращаем результат

;; ============================================================================
;; ТЕСТИРОВАНИЕ КОНВЕЙЕРА
;; ============================================================================

(println "=== ТЕСТ ФУНКЦИОНАЛЬНОГО КОНВЕЙЕРА ===\n")

;; Создаем тестовый файл
(spit "pipeline_test.h" "#define PIPELINE_VALUE 123")

;; Тестируем безопасный вывод
(println "Тест 1: Безопасный вывод многострочной строки")
(safe-println "Многострочный код:" "#include \"pipeline_test.h\"\nint x = PIPELINE_VALUE;\nreturn x;")

(println "\nТест 2: Конвейер препроцессора")
(def test-code "#include \"pipeline_test.h\"\nint result = PIPELINE_VALUE;")

;; Используем ФП конвейер для полной обработки
(->> test-code
     (safe-println "Исходный код:")
     (#(debug-preprocess % {:include-paths ["."]}))
     :result
     (safe-println "Финальный результат:"))

(println "\nТест 3: Композиция функций")
(def process-and-display
  (comp #(safe-println "Обработанный код:" (:result %))
        #(pp/preprocess-v2 % {:include-paths ["."]})))

(process-and-display "#include \"pipeline_test.h\"\nint final = PIPELINE_VALUE * 2;")

(println "\n=== КОНВЕЙЕР ЗАВЕРШЕН ===") 