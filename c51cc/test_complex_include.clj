(require '[c51cc.preprocessor :as pp])

;; Тест с вложенными include и макросами
(def test-code "#include <reg2051.h>
#define LED_PIN 1
#define LED_ON() (P1 |= (1 << LED_PIN))
#define LED_OFF() (P1 &= ~(1 << LED_PIN))

int main() { 
    LED_ON();
    if (P1_1) {
        LED_OFF();
    }
    return 0; 
}")

(println "=== Тестирование сложного препроцессирования ===")
(println "Исходный код:")
(println test-code)
(println "\n=== Результат препроцессирования ===")

(try
  (let [result (pp/preprocess test-code {:include-paths ["test/ccode" "include"]})]
    (println result)
    (println "\n=== Анализ результата ===")
    (println "Длина результата:" (count result))
    (println "Содержит P1_1:" (boolean (re-find #"P1_1" result)))
    (println "Содержит LED_ON:" (boolean (re-find #"LED_ON" result))))
  (catch Exception e
    (println "ОШИБКА:" (.getMessage e))
    (println "Детали:" (ex-data e)))) 