(require '[c51cc.lexer :as lexer])

(println "🔍 ТЕСТИРОВАНИЕ ЛЕКСЕРА БЕЗ КОММЕНТАРИЕВ")
(println "========================================")

;; Тест 1: Многострочная программа БЕЗ комментариев (после препроцессора)
(let [code "unsigned char led_state = 0;

void delay(unsigned int ms) {
  for (unsigned int i = 0; i < ms; i++) {
  }
}

void main() {
  while (1) {
    led_state = !led_state;
    delay(1000);
  }
}"
      tokens (lexer/tokenize code)]
  (println (str "Код после препроцессора:\n" code))
  (println (str "\nКоличество токенов: " (count tokens)))
  (println "\nВсе токены:")
  (doseq [token tokens]
    (println (str "  " token))))

;; Тест 2: Простая программа без комментариев
(let [code "int x = 10; void main() { x = x + 1; }"
      tokens (lexer/tokenize code)]
  (println (str "\n\nПростая программа: " code))
  (println (str "Количество токенов: " (count tokens)))
  (println "Токены:")
  (doseq [token tokens]
    (println (str "  " token))))

;; Тест 3: Проверим, что происходит с комментариями (если они попадают в лексер)
(let [code "int x = 10; // это комментарий"
      tokens (lexer/tokenize code)]
  (println (str "\n\nКод с комментарием (НЕ должен попадать в лексер): " code))
  (println (str "Количество токенов: " (count tokens)))
  (println "Токены:")
  (doseq [token tokens]
    (println (str "  " token))))

(println "\n✅ Тестирование лексера завершено") 