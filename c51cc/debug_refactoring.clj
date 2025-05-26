(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "=== ОТЛАДКА РЕФАКТОРИНГА ===")

;; Тест 1: Простое выражение
(println "\n1. Простое выражение:")
(let [code "42;"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Код:" code)
  (println "Токены:" tokens)
  (println "Успех:" (:success result))
  (when (:success result)
    (println "AST:" (:ast result)))
  (when (not (:success result))
    (println "Ошибка:" (:error result))))

;; Тест 2: Простая функция
(println "\n2. Простая функция:")
(let [code "void main() {}"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Код:" code)
  (println "Токены:" tokens)
  (println "Успех:" (:success result))
  (when (:success result)
    (println "AST:" (:ast result)))
  (when (not (:success result))
    (println "Ошибка:" (:error result))))

;; Тест 3: Функция с параметрами
(println "\n3. Функция с параметрами:")
(let [code "int add(int a, int b) { return a; }"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Код:" code)
  (println "Токены:" tokens)
  (println "Успех:" (:success result))
  (when (:success result)
    (println "AST:" (:ast result)))
  (when (not (:success result))
    (println "Ошибка:" (:error result))))

;; Тест 4: C51 interrupt
(println "\n4. C51 interrupt:")
(let [code "void timer_isr() interrupt 1 {}"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Код:" code)
  (println "Токены:" tokens)
  (println "Успех:" (:success result))
  (when (:success result)
    (println "AST:" (:ast result)))
  (when (not (:success result))
    (println "Ошибка:" (:error result))))

;; Тест 5: SFR декларация
(println "\n5. SFR декларация:")
(let [code "sfr P0 = 0x80;"
      tokens (lexer/tokenize code)
      result (parser/parse tokens)]
  (println "Код:" code)
  (println "Токены:" tokens)
  (println "Успех:" (:success result))
  (when (:success result)
    (println "AST:" (:ast result)))
  (when (not (:success result))
    (println "Ошибка:" (:error result))))

(println "\n=== ОТЛАДКА ЗАВЕРШЕНА ===") 