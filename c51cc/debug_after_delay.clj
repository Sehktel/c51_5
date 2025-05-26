(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ТЕСТ: DELAY + READ_ADC ===")
(def delay-plus-read "
void delay(unsigned int ms) using 1 {
  unsigned int i, j;
  for (i = 0; i < ms; i++) {
    for (j = 0; j < 1000; j++) {
      i = i + 1;
    }
  }
}

unsigned char read_adc() {
  return P1;
}")

(let [result (parser/parse (lexer/tokenize delay-plus-read))]
  (if (:success result)
    (println "✅ УСПЕХ! delay + read_adc парсится")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result)))))

(println "\n=== ТЕСТ: ТОЛЬКО READ_ADC ===")
(def only-read "unsigned char read_adc() { return P1; }")
(let [result (parser/parse (lexer/tokenize only-read))]
  (if (:success result)
    (println "✅ УСПЕХ! read_adc парсится отдельно")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result))))) 