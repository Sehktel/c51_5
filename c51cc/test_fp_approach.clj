(require 'c51cc.preprocessor)

(println "=== АНАЛИЗ ФУНКЦИОНАЛЬНОГО ПОДХОДА ===\n")

;; Проверяем текущий подход к обработке ошибок
(println "1. Успешный случай:")
(let [result (c51cc.preprocessor/preprocess "#define MAX 100\nint x = MAX;")]
  (println "Тип результата:" (type result))
  (println "Ключи:" (keys result))
  (println "Успех:" (:success result))
  (println "Ошибки:" (:errors result))
  (println))

(println "2. Ошибка #else без #if:")
(let [result (c51cc.preprocessor/preprocess "#else\nint x = 1;")]
  (println "Успех:" (:success result))
  (println "Количество ошибок:" (count (:errors result)))
  (println "Первая ошибка:" (first (:errors result)))
  (println))

(println "3. Несуществующий include:")
(let [result (c51cc.preprocessor/preprocess "#include \"nonexistent.h\"")]
  (println "Успех:" (:success result))
  (println "Ошибки:" (:errors result))
  (println))

(println "4. Проверка отсутствия исключений:")
(try
  (let [result (c51cc.preprocessor/preprocess "#else\n#endif\n#include \"bad.h\"")]
    (println "✓ Исключений НЕ выброшено!")
    (println "Успех:" (:success result))
    (println "Количество ошибок:" (count (:errors result))))
  (catch Exception e
    (println "✗ ПРОБЛЕМА: Выброшено исключение:" (.getMessage e))))

(println "\n=== ВЫВОДЫ ===")
(println "Текущий подход:")
(println "- Возвращает {:result :errors :success}")
(println "- Накапливает ошибки в векторе")
(println "- Не выбрасывает исключения на уровне API")
(println "- Соответствует канонам ФП ✓") 