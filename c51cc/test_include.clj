(require '[c51cc.preprocessor-v2 :as pp])

;; Тест базовой функциональности
(println "=== ТЕСТ 1: Базовая обработка ===")
(let [result (pp/preprocess-v2 "#define TEST 123\nint x = TEST;")]
  (println "Результат:" (:result result))
  (println "Успех:" (:success result))
  (println "Ошибки:" (:errors result)))

;; Тест include с простым содержимым
(println "\n=== ТЕСТ 2: Include с простым содержимым ===")
(spit "test_header.h" "#define MAX_SIZE 100\n#define VERSION \"1.0\"")
(let [result (pp/preprocess-v2 "#include \"test_header.h\"\nint buffer[MAX_SIZE];" 
                              {:include-paths ["."]})]
  (println "Результат:" (:result result))
  (println "Успех:" (:success result))
  (println "Ошибки:" (:errors result)))

;; Тест include guards
(println "\n=== ТЕСТ 3: Include guards ===")
(spit "guarded_header.h" "#ifndef GUARD_H\n#define GUARD_H\n#define PROTECTED 42\n#endif")
(let [result (pp/preprocess-v2 "#include \"guarded_header.h\"\n#include \"guarded_header.h\"\nint x = PROTECTED;" 
                              {:include-paths ["."]})]
  (println "Результат:" (:result result))
  (println "Успех:" (:success result))
  (println "Ошибки:" (:errors result)))

(println "\n=== ТЕСТЫ ЗАВЕРШЕНЫ ===") 