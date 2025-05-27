(ns test-preprocessor
  (:require [c51cc.preprocessor :as pp]))

;; Тест базовой функциональности
(def test-code "
#define MAX_SIZE 100
#define DEBUG

#ifdef DEBUG
  int debug_mode = 1;
#else
  int debug_mode = 0;
#endif

int array[MAX_SIZE];
")

(println "=== Тест улучшенного препроцессора ===")
(try
  (let [result (pp/preprocess test-code)]
    (println "Результат обработки:")
    (println result))
  (catch Exception e
    (println "Ошибка:" (.getMessage e))
    (println "Данные:" (ex-data e))))

;; Тест обработки ошибок
(println "\n=== Тест обработки ошибок ===")
(try
  (pp/preprocess "#ifdef UNDEFINED\ncode\n") ; Незакрытый блок
  (catch Exception e
    (println "Ожидаемая ошибка:" (.getMessage e)))) 