(require '[c51cc.preprocessor :as pp])

(println "=== Отладка определения макросов ===")

;; Тест 1: Простое определение макроса
(println "\n1. Простое определение макроса:")
(let [result (pp/preprocess "#define LED P1_7\nLED = 1;")]
  (println "Результат:" (pr-str result)))

;; Тест 2: Определение макроса в #ifdef (DEBUG определен)
(println "\n2. Макрос в #ifdef (DEBUG определен):")
(let [result (pp/preprocess "#define DEBUG\n#ifdef DEBUG\n#define LED P1_7\n#endif\nLED = 1;")]
  (println "Результат:" (pr-str result)))

;; Тест 3: Определение макроса в #else (DEBUG НЕ определен)
(println "\n3. Макрос в #else (DEBUG НЕ определен):")
(let [result (pp/preprocess "#ifdef DEBUG\n#define LED P1_7\n#else\n#define LED P1_0\n#endif\nLED = 1;")]
  (println "Результат:" (pr-str result)))

;; Тест 4: Проверим, что происходит с макросами в неактивных блоках
(println "\n4. Макрос в неактивном блоке:")
(let [result (pp/preprocess "#ifdef UNDEFINED\n#define LED P1_7\n#endif\nLED = 1;")]
  (println "Результат:" (pr-str result)))

(println "\n=== Отладка завершена ===") 