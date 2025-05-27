(require '[c51cc.preprocessor :as pp])

;; Тест условной компиляции
(def test-code "#include <reg2051.h>
#define DEBUG_MODE

#ifdef DEBUG_MODE
    #define DEBUG_LED P1_0
#else
    #define DEBUG_LED P1_7
#endif

#ifndef RELEASE_MODE
    #define EXTRA_FEATURES
#endif

int main() { 
    DEBUG_LED = 1;
    #ifdef EXTRA_FEATURES
    P1 = 0xFF;
    #endif
    return 0; 
}")

(println "=== Тестирование условной компиляции ===")
(println "Исходный код:")
(println test-code)
(println "\n=== Результат ===")

(try
  (let [result (pp/preprocess test-code {:include-paths ["test/ccode" "include"]})]
    (println result)
    (println "\n=== Анализ ===")
    (println "Содержит P1 = 0xFF:" (boolean (re-find #"P1 = 0xFF" result)))
    (println "Содержит DEBUG_LED:" (boolean (re-find #"DEBUG_LED" result))))
  (catch Exception e
    (println "ОШИБКА:" (.getMessage e)))) 