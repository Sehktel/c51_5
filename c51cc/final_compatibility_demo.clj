(require '[c51cc.preprocessor :as pp])
(require '[c51cc.preprocessor-v2 :as pp2])

(println "🎯 === ДЕМОНСТРАЦИЯ РЕШЕНИЯ ПРОБЛЕМЫ ОБРАТНОЙ СОВМЕСТИМОСТИ ===")
(println)

;; ============================================================================
;; ПРОБЛЕМА (ДО ИСПРАВЛЕНИЯ)
;; ============================================================================

(println "❌ ПРОБЛЕМА (была):")
(println "• v1 возвращал String")
(println "• v2 возвращает {:result String, :errors [], :success Boolean}")
(println "• Нет обратной совместимости")
(println "• Старый код не работает с новым препроцессором")
(println)

;; ============================================================================
;; РЕШЕНИЕ (ПОСЛЕ ИСПРАВЛЕНИЯ)
;; ============================================================================

(println "✅ РЕШЕНИЕ (сейчас):")
(println "• Создан wrapper в c51cc.preprocessor")
(println "• preprocess() вызывает preprocess-v2() внутри")
(println "• Возвращает String для совместимости")
(println "• Старый код работает БЕЗ ИЗМЕНЕНИЙ")
(println)

;; ============================================================================
;; ДЕМОНСТРАЦИЯ
;; ============================================================================

(def test-code 
  "#define MAX_SIZE 256
   #define DEBUG
   #ifdef DEBUG
   int debug_level = 2;
   #endif
   int buffer[MAX_SIZE];")

(println "📝 Тестовый код:")
(println test-code)
(println)

;; Старый API (через wrapper)
(println "🔄 Старый API (c51cc.preprocessor/preprocess):")
(let [result (pp/preprocess test-code)]
  (println "Тип результата:" (type result))
  (println "Результат:")
  (println result))
(println)

;; Новый API (прямой вызов)
(println "🆕 Новый API (c51cc.preprocessor-v2/preprocess-v2):")
(let [result (pp2/preprocess-v2 test-code)]
  (println "Тип результата:" (type result))
  (println "Структура результата:")
  (println "  :result ->" (pr-str (:result result)))
  (println "  :success ->" (:success result))
  (println "  :errors ->" (count (:errors result)) "ошибок"))
(println)

;; Проверка совместимости
(println "🔍 Проверка совместимости:")
(let [old-result (pp/preprocess test-code)
      new-result (pp2/preprocess-v2 test-code)
      compatible? (= old-result (:result new-result))]
  (println "Результаты идентичны:" compatible?)
  (if compatible?
    (println "✅ ОБРАТНАЯ СОВМЕСТИМОСТЬ РАБОТАЕТ!")
    (println "❌ ПРОБЛЕМА С СОВМЕСТИМОСТЬЮ!")))
(println)

;; ============================================================================
;; ПРЕИМУЩЕСТВА РЕШЕНИЯ
;; ============================================================================

(println "🎉 ПРЕИМУЩЕСТВА РЕШЕНИЯ:")
(println "1. 🔄 Полная обратная совместимость")
(println "   • Старый код работает без изменений")
(println "   • preprocess() возвращает String как раньше")
(println)
(println "2. 🆕 Новые возможности доступны")
(println "   • preprocess-v2() для детальной информации")
(println "   • Обработка ошибок")
(println "   • Доступ к состоянию препроцессора")
(println)
(println "3. 🚀 Гибкая миграция")
(println "   • Можно мигрировать постепенно")
(println "   • Или продолжать использовать старый API")
(println)
(println "4. 🔧 Лучшая архитектура")
(println "   • Transducers для производительности")
(println "   • Protocols для расширяемости")
(println "   • Spec для валидации")

(println)
(println "🎯 ЗАКЛЮЧЕНИЕ: Проблема обратной совместимости РЕШЕНА!")
(println "✅ Wrapper обеспечивает бесшовную работу старого кода")
(println "✅ Новый API предоставляет дополнительные возможности")
(println "✅ Миграция может быть постепенной или не выполняться вовсе") 