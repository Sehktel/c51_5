(require '[c51cc.preprocessor :as pp])
(require '[c51cc.preprocessor-v2 :as pp2])

(println "🔍 === ОТЛАДКА МАКРОСОВ С ПАРАМЕТРАМИ ===")

(def test-code "#define SQUARE(x) ((x) * (x))\nint result = SQUARE(5);")

(println "Тестовый код:")
(println test-code)

(println "\n--- СТАРЫЙ ПРЕПРОЦЕССОР ---")
(try
  (let [old-result (pp/preprocess test-code)]
    (println "Результат:" (pr-str old-result))
    (println "Длина:" (count old-result)))
  (catch Exception e
    (println "ОШИБКА:" (.getMessage e))))

(println "\n--- НОВЫЙ ПРЕПРОЦЕССОР ---")
(try
  (let [new-result (pp2/preprocess-v2 test-code)]
    (println "Результат:" (pr-str (:result new-result)))
    (println "Успех:" (:success new-result))
    (println "Ошибки:" (count (:errors new-result)))
    (println "Длина:" (count (:result new-result))))
  (catch Exception e
    (println "ОШИБКА:" (.getMessage e))))

(println "\n--- ПРОВЕРКА ПОДДЕРЖКИ МАКРОСОВ С ПАРАМЕТРАМИ ---")
(println "Ожидаемый результат: int result = ((5) * (5));")
(println "Проблема: Макросы с параметрами не реализованы в новом препроцессоре!")

(println "\n🚨 КРИТИЧЕСКАЯ ПРОБЛЕМА СОВМЕСТИМОСТИ ОБНАРУЖЕНА!") 