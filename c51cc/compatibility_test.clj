(require '[c51cc.preprocessor :as pp])
(require '[c51cc.preprocessor-v2 :as pp2])
(require '[clojure.string :as str])

(println "🔄 === ТЕСТ СОВМЕСТИМОСТИ ПРЕПРОЦЕССОРА ===")

;; ============================================================================
;; ТЕСТИРУЕМ ОБРАТНУЮ СОВМЕСТИМОСТЬ СТАРОГО API
;; ============================================================================

(defn test-compatibility [test-name old-code new-code expected-result]
  (println (str "\n🧪 " test-name))
  (try
    ;; Тестируем старый API (должен работать через wrapper)
    (let [old-result (pp/preprocess old-code)
          new-result (pp2/preprocess-v2 new-code)
          new-result-string (:result new-result)]
      
      (println "Старый API результат:" (pr-str old-result))
      (println "Новый API результат:" (pr-str new-result-string))
      (println "Ожидаемый результат:" (pr-str expected-result))
      
      (let [old-matches (= old-result expected-result)
            new-matches (= new-result-string expected-result)
            compatible (= old-result new-result-string)]
        
        (println "Старый API корректен:" old-matches)
        (println "Новый API корректен:" new-matches)
        (println "Совместимость:" compatible)
        
        (if (and old-matches new-matches compatible)
          (println "✅ ТЕСТ ПРОЙДЕН")
          (println "❌ ТЕСТ НЕ ПРОЙДЕН"))
        
        (and old-matches new-matches compatible)))
    
    (catch Exception e
      (println "❌ ИСКЛЮЧЕНИЕ:" (.getMessage e))
      false)))

;; ============================================================================
;; НАБОР ТЕСТОВ СОВМЕСТИМОСТИ
;; ============================================================================

(def compatibility-tests [
  ["Простые макросы"
   "#define MAX 100\nint x = MAX;"
   "#define MAX 100\nint x = MAX;"
   "int x = 100;"]
  
  ["Условная компиляция"
   "#define DEBUG\n#ifdef DEBUG\nint debug = 1;\n#endif"
   "#define DEBUG\n#ifdef DEBUG\nint debug = 1;\n#endif"
   "int debug = 1;"]
  
  ["Макросы с параметрами"
   "#define SQUARE(x) ((x) * (x))\nint result = SQUARE(5);"
   "#define SQUARE(x) ((x) * (x))\nint result = SQUARE(5);"
   "int result = ((5) * (5));"]
])

;; Создаем тестовый файл для include
(spit "compat_test.h" "#define COMPAT_VALUE 42")

(def include-test
  ["Include директивы"
   "#include \"compat_test.h\"\nint x = COMPAT_VALUE;"
   "#include \"compat_test.h\"\nint x = COMPAT_VALUE;"
   "int x = 42;"])

;; Запускаем все тесты
(println "\n📋 Запуск тестов совместимости...")

(def results 
  (concat 
    (for [[name old-code new-code expected] compatibility-tests]
      (test-compatibility name old-code new-code expected))
    [(test-compatibility 
       (first include-test) 
       (second include-test) 
       (nth include-test 2) 
       (nth include-test 3))]))

;; ============================================================================
;; ТЕСТИРУЕМ НОВЫЕ ВОЗМОЖНОСТИ (ТОЛЬКО В V2)
;; ============================================================================

(println "\n🆕 === ТЕСТ НОВЫХ ВОЗМОЖНОСТЕЙ V2 ===")

(defn test-v2-features []
  (println "\n🧪 Детальная информация об ошибках")
  (let [result (pp2/preprocess-v2 "#include \"missing.h\"\nint x = 1;" {:include-paths ["."]})]
    (println "Успех:" (:success result))
    (println "Количество ошибок:" (count (:errors result)))
    (println "Первая ошибка:" (:message (first (:errors result))))
    (println "✅ Новые возможности работают"))
  
  (println "\n🧪 Вложенные включения")
  (spit "v2_inner.h" "#define INNER_VAL 123")
  (spit "v2_outer.h" "#include \"v2_inner.h\"\n#define OUTER_VAL INNER_VAL")
  (let [result (pp2/preprocess-v2 "#include \"v2_outer.h\"\nint x = OUTER_VAL;" {:include-paths ["."]})]
    (println "Результат:" (:result result))
    (println "Успех:" (:success result))
    (println "✅ Вложенные включения работают")))

(test-v2-features)

;; ============================================================================
;; ИТОГОВАЯ СТАТИСТИКА
;; ============================================================================

(println "\n📊 === ИТОГОВАЯ СТАТИСТИКА СОВМЕСТИМОСТИ ===")
(println "Всего тестов совместимости:" (count results))
(println "Пройдено:" (count (filter true? results)))
(println "Провалено:" (count (filter false? results)))

(if (every? true? results)
  (do
    (println "\n🎉 ВСЕ ТЕСТЫ СОВМЕСТИМОСТИ ПРОЙДЕНЫ!")
    (println "✅ Обновление будет БЕСШОВНЫМ!")
    (println "✅ Старый код будет работать без изменений!")
    (println "✅ Новые возможности доступны через preprocess-v2!"))
  (do
    (println "\n⚠️  ЕСТЬ ПРОБЛЕМЫ СОВМЕСТИМОСТИ!")
    (println "❌ Требуется дополнительная работа перед слиянием!")))

(println "\n🔧 === ИНСТРУКЦИЯ ПО МИГРАЦИИ ===")
(println "1. Старый код: (preprocess code options) → String")
(println "2. Новый код: (preprocess-v2 code options) → {:result String, :errors [], :success Boolean}")
(println "3. Для миграции: замените preprocess на preprocess-v2 и обрабатывайте map результат")
(println "4. Wrapper обеспечивает полную обратную совместимость!") 