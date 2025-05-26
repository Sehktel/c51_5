(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ТЕСТ ПОДДЕРЖКИ INTERRUPT И USING ===")

;; Тест 1: Функция с interrupt без using
(println "\n1. Тестируем: void foo(void) interrupt 2 { }")
(let [code "void foo(void) interrupt 2 { }"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Токены:" tokens)
  (println "Результат парсинга:")
  (if (:success result)
    (do
      (println "✓ Успешно!")
      (println "AST:" (:ast result)))
    (do
      (println "✗ Ошибка:" (:error result))
      (println "Позиция:" (:position result)))))

;; Тест 2: Функция с interrupt и using
(println "\n2. Тестируем: void timer_isr(void) interrupt 1 using 2 { }")
(let [code "void timer_isr(void) interrupt 1 using 2 { }"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Токены:" tokens)
  (println "Результат парсинга:")
  (if (:success result)
    (do
      (println "✓ Успешно!")
      (println "AST:" (:ast result)))
    (do
      (println "✗ Ошибка:" (:error result))
      (println "Позиция:" (:position result)))))

;; Тест 3: Обычная функция (без interrupt)
(println "\n3. Тестируем: void normal_func(void) { }")
(let [code "void normal_func(void) { }"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Токены:" tokens)
  (println "Результат парсинга:")
  (if (:success result)
    (do
      (println "✓ Успешно!")
      (println "AST:" (:ast result)))
    (do
      (println "✗ Ошибка:" (:error result))
      (println "Позиция:" (:position result)))))

;; Тест 4: Функция с телом
(println "\n4. Тестируем: void ext_int(void) interrupt 0 { P1 = 0x01; }")
(let [code "void ext_int(void) interrupt 0 { P1 = 0x01; }"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Токены:" tokens)
  (println "Результат парсинга:")
  (if (:success result)
    (do
      (println "✓ Успешно!")
      (println "AST:" (:ast result)))
    (do
      (println "✗ Ошибка:" (:error result))
      (println "Позиция:" (:position result)))))

(println "\n=== КОНЕЦ ТЕСТОВ ===") 