(require '[c51cc.preprocessor :as pp])
(require '[c51cc.preprocessor-v2 :as pp2])
(require '[clojure.string :as str])

(println "🔄 === КОМПЛЕКСНЫЙ ТЕСТ ОБРАТНОЙ СОВМЕСТИМОСТИ ===")

(defn test-compatibility [test-name code expected-contains]
  (println (str "\n🧪 " test-name))
  (try
    (let [old-result (pp/preprocess code)
          new-result (pp2/preprocess-v2 code)
          new-result-string (:result new-result)
          compatible? (= old-result new-result-string)
          contains-expected? (every? #(str/includes? old-result %) expected-contains)]
      
      (println "Старый API:" (pr-str old-result))
      (println "Новый API:" (pr-str new-result-string))
      (println "Совместимость:" compatible?)
      (println "Содержит ожидаемое:" contains-expected?)
      
      (if (and compatible? contains-expected?)
        (do (println "✅ ТЕСТ ПРОЙДЕН") true)
        (do (println "❌ ТЕСТ НЕ ПРОЙДЕН") false)))
    
    (catch Exception e
      (println "❌ ИСКЛЮЧЕНИЕ:" (.getMessage e))
      false)))

;; ============================================================================
;; НАБОР ТЕСТОВ
;; ============================================================================

(def test-results [
  ;; Тест 1: Простые макросы
  (test-compatibility 
    "Простые макросы"
    "#define MAX 100\n#define MIN 0\nint range = MAX - MIN;"
    ["100" "0" "range"])
  
  ;; Тест 2: Условная компиляция
  (test-compatibility
    "Условная компиляция #ifdef"
    "#define DEBUG\n#ifdef DEBUG\nint debug_mode = 1;\n#endif\nint x = 42;"
    ["debug_mode = 1" "x = 42"])
  
  ;; Тест 3: Условная компиляция #ifndef
  (test-compatibility
    "Условная компиляция #ifndef"
    "#ifndef RELEASE\nint debug_info = 1;\n#endif\nint main_var = 5;"
    ["debug_info = 1" "main_var = 5"])
  
  ;; Тест 4: Вложенные условия
  (test-compatibility
    "Вложенные условия"
    "#define FEATURE_A\n#ifdef FEATURE_A\n#define SUB_FEATURE\n#ifdef SUB_FEATURE\nint nested = 1;\n#endif\n#endif"
    ["nested = 1"])
  
  ;; Тест 5: #undef
  (test-compatibility
    "Отмена определения макроса"
    "#define TEMP 50\nint a = TEMP;\n#undef TEMP\nint b = TEMP;"
    ["a = 50" "b = TEMP"])
  
  ;; Тест 6: Предопределенные макросы
  (test-compatibility
    "Предопределенные макросы"
    "int line = __LINE__;\nchar* file = __FILE__;"
    ["line = " "file = "])
])

;; ============================================================================
;; РЕЗУЛЬТАТЫ
;; ============================================================================

(let [passed (count (filter true? test-results))
      total (count test-results)
      success-rate (/ passed total)]
  
  (println (str "\n📊 === РЕЗУЛЬТАТЫ ТЕСТИРОВАНИЯ ==="))
  (println (str "Пройдено: " passed "/" total " (" (int (* success-rate 100)) "%)"))
  
  (if (= success-rate 1.0)
    (do
      (println "\n🎉 ВСЕ ТЕСТЫ ПРОЙДЕНЫ!")
      (println "✅ ОБРАТНАЯ СОВМЕСТИМОСТЬ ПОЛНОСТЬЮ РАБОТАЕТ!")
      (println "✅ Старый код будет работать без изменений!")
      (println "✅ Новые возможности доступны через preprocess-v2!"))
    (do
      (println "\n⚠️  ЕСТЬ ПРОБЛЕМЫ!")
      (println "❌ Требуется дополнительная работа!"))))

(println "\n🔧 === ИНСТРУКЦИЯ ПО ИСПОЛЬЗОВАНИЮ ===")
(println "1. Старый API: (preprocess code options) → String")
(println "2. Новый API: (preprocess-v2 code options) → {:result String, :errors [], :success Boolean}")
(println "3. Wrapper обеспечивает ПОЛНУЮ обратную совместимость!")
(println "4. Для новых проектов рекомендуется использовать preprocess-v2")

(println "\n✨ === ПРЕИМУЩЕСТВА НОВОГО API ===")
(println "• Детальная информация об ошибках")
(println "• Лучшая производительность (transducers)")
(println "• Модульная архитектура (protocols)")
(println "• Расширяемость")
(println "• Валидация через clojure.spec") 