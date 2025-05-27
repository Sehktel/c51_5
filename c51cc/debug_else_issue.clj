(require '[c51cc.preprocessor :as pp])

(println "=== Диагностика проблемы с #else ===")

;; Тест 1: Простой #ifdef без #else
(println "\n1. Простой #ifdef (должен работать):")
(let [code "#define DEBUG\n#ifdef DEBUG\nint debug_var = 1;\n#endif"
      result (pp/preprocess code)]
  (println "Код:" code)
  (println "Результат:" (pr-str result))
  (println "Содержит debug_var:" (clojure.string/includes? result "debug_var")))

;; Тест 2: #ifdef с #else - DEBUG определен
(println "\n2. #ifdef с #else (DEBUG определен):")
(let [code "#define DEBUG\n#ifdef DEBUG\nint debug_var = 1;\n#else\nint release_var = 1;\n#endif"
      result (pp/preprocess code)]
  (println "Код:" code)
  (println "Результат:" (pr-str result))
  (println "Содержит debug_var:" (clojure.string/includes? result "debug_var"))
  (println "Содержит release_var:" (clojure.string/includes? result "release_var")))

;; Тест 3: #ifdef с #else - DEBUG НЕ определен
(println "\n3. #ifdef с #else (DEBUG НЕ определен):")
(let [code "#ifdef DEBUG\nint debug_var = 1;\n#else\nint release_var = 1;\n#endif"
      result (pp/preprocess code)]
  (println "Код:" code)
  (println "Результат:" (pr-str result))
  (println "Содержит debug_var:" (clojure.string/includes? result "debug_var"))
  (println "Содержит release_var:" (clojure.string/includes? result "release_var")))

;; Тест 4: Проблемный случай с макросами в #else
(println "\n4. Макросы в блоках #ifdef/#else:")
(let [code "#define DEBUG\n#ifdef DEBUG\n#define LED P1_7\n#else\n#define LED P1_0\n#endif\nLED = 1;"
      result (pp/preprocess code)]
  (println "Код:" code)
  (println "Результат:" (pr-str result))
  (println "Содержит P1_7:" (clojure.string/includes? result "P1_7"))
  (println "Содержит P1_0:" (clojure.string/includes? result "P1_0"))
  (println "Содержит LED:" (clojure.string/includes? result "LED")))

(println "\n=== Диагностика завершена ===") 