(require '[c51cc.preprocessor :as pp])

;; Тест простого include
(def test-code "#include <reg2051.h>
int main() { 
    P1 = 0xFF; 
    return 0; 
}")

(println "=== Тестирование препроцессора ===")
(println "Исходный код:")
(println test-code)
(println "\n=== Результат препроцессирования ===")

(try
  (let [result (pp/preprocess test-code {:include-paths ["test/ccode" "include"]})]
    (println result))
  (catch Exception e
    (println "ОШИБКА:" (.getMessage e))
    (println "Детали:" (ex-data e)))) 