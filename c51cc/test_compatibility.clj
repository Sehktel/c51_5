(require '[c51cc.preprocessor :as pp])
(require '[c51cc.preprocessor-v2 :as pp2])

(println "=== ТЕСТ ОБРАТНОЙ СОВМЕСТИМОСТИ ===")

;; Простой тест кода
(def test-code "#define MAX 100\nint x = MAX;")

(try
  ;; Тестируем старый API (через wrapper)
  (let [old-result (pp/preprocess test-code)
        new-result (pp2/preprocess-v2 test-code)
        new-result-string (:result new-result)]
    
    (println "Старый API результат:")
    (println old-result)
    (println "\nНовый API результат:")
    (println new-result-string)
    (println "\nСовместимость:" (= old-result new-result-string))
    
    (if (= old-result new-result-string)
      (println "✅ ОБРАТНАЯ СОВМЕСТИМОСТЬ РАБОТАЕТ!")
      (println "❌ ПРОБЛЕМА С СОВМЕСТИМОСТЬЮ!")))
  
  (catch Exception e
    (println "❌ ОШИБКА:" (.getMessage e))))

(println "\n=== ТЕСТ ЗАВЕРШЕН ===") 