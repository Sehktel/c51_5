(require '[c51cc.preprocessor :as pp])

;; Тест макросов после include
(def test-code "#include <reg2051.h>
#define LED_PIN 1
#define LED_ON() (P1 |= (1 << LED_PIN))

int main() { 
    LED_ON();
    P1_1 = 1;
    return 0; 
}")

(println "=== Тестирование макросов ===")
(println "Исходный код:")
(println test-code)
(println "\n=== Результат ===")

(try
  (let [result (pp/preprocess test-code {:include-paths ["test/ccode" "include"]})]
    (println result)
    (println "\n=== Анализ ===")
    (println "Содержит P1_1:" (boolean (re-find #"P1_1" result)))
    (println "Содержит LED_ON:" (boolean (re-find #"LED_ON" result)))
    (println "Содержит макрос расширение:" (boolean (re-find #"\(P1 \|=" result))))
  (catch Exception e
    (println "ОШИБКА:" (.getMessage e)))) 