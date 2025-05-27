(require '[c51cc.preprocessor :as pp])

(println "=== Отладка проблем в тестах ===")

;; Тест 1: Проверка обработки комментариев
(println "\n1. Тест обработки комментариев:")
(let [code "/* комментарий */ int x = 42; // еще комментарий"
      result (pp/preprocess code)]
  (println "Исходный код:" (pr-str code))
  (println "Результат:" (pr-str result))
  (println "Ожидается: \"int x = 42;\"")
  (println "Совпадает:" (= "int x = 42;" result)))

;; Тест 2: Проверка нормализации пробелов
(println "\n2. Тест нормализации пробелов:")
(let [code "  #define   MAX   100  \n   int x = MAX;   "
      result (pp/preprocess code)]
  (println "Исходный код:" (pr-str code))
  (println "Результат:" (pr-str result))
  (println "Ожидается: \"int x = 100;\"")
  (println "Совпадает:" (= "int x = 100;" result)))

;; Тест 3: Проверка условной компиляции без DEBUG_MODE
(println "\n3. Тест условной компиляции (без DEBUG_MODE):")
(let [code "#ifdef DEBUG_MODE\n#define DEBUG_LED P1_7\n#else\n#define DEBUG_LED P1_0\n#endif\nDEBUG_LED = 1;"
      result (pp/preprocess code)]
  (println "Исходный код:" (pr-str code))
  (println "Результат:" (pr-str result))
  (println "Содержит P1_0:" (clojure.string/includes? result "P1_0"))
  (println "Содержит DEBUG_LED:" (clojure.string/includes? result "DEBUG_LED")))

;; Тест 4: Проверка условной компиляции с DEBUG_MODE
(println "\n4. Тест условной компиляции (с DEBUG_MODE):")
(let [code "#define DEBUG_MODE\n#ifdef DEBUG_MODE\n#define DEBUG_LED P1_7\n#else\n#define DEBUG_LED P1_0\n#endif\nDEBUG_LED = 1;"
      result (pp/preprocess code)]
  (println "Исходный код:" (pr-str code))
  (println "Результат:" (pr-str result))
  (println "Содержит P1_7:" (clojure.string/includes? result "P1_7"))
  (println "Содержит DEBUG_LED:" (clojure.string/includes? result "DEBUG_LED")))

(println "\n=== Отладка завершена ===") 