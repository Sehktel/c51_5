(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ПРОСТОЙ ТЕСТ C51 ФУНКЦИЙ ===")

;; Тест 1: Простая функция с interrupt
(println "\n1. Тестируем простую interrupt функцию:")
(let [code "void isr(void) interrupt 1 { }"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Код:" code)
  (if (:success result)
    (do
      (println "✓ Парсинг успешен!")
      (println "AST:" (:ast result)))
    (do
      (println "✗ Ошибка:" (:error result)))))

;; Тест 2: Функция с interrupt и using
(println "\n2. Тестируем interrupt + using:")
(let [code "void timer_isr(void) interrupt 1 using 2 { }"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Код:" code)
  (if (:success result)
    (do
      (println "✓ Парсинг успешен!")
      (println "AST:" (:ast result)))
    (do
      (println "✗ Ошибка:" (:error result)))))

;; Тест 3: SFR декларация
(println "\n3. Тестируем SFR декларацию:")
(let [code "sfr P1 = 0x90;"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Код:" code)
  (if (:success result)
    (do
      (println "✓ Парсинг успешен!")
      (println "AST:" (:ast result)))
    (do
      (println "✗ Ошибка:" (:error result)))))

;; Тест 4: SBIT декларация
(println "\n4. Тестируем SBIT декларацию:")
(let [code "sbit LED = 0x90;"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Код:" code)
  (if (:success result)
    (do
      (println "✓ Парсинг успешен!")
      (println "AST:" (:ast result)))
    (do
      (println "✗ Ошибка:" (:error result)))))

(println "\n=== ТЕСТ ЗАВЕРШЕН ===") 