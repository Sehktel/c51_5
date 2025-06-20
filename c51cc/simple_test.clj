(ns simple_test
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(println "=== Тест гигиенического макроса do-parse ===")

;; Тест 1: Проверка макроэкспансии
(println "\n1. Проверка макроэкспансии:")
(try
  (let [expanded (macroexpand '(parser/do-parse 
                                 x (parser/expect-token :int)
                                 y (parser/expect-token :identifier)
                                 (parser/return-parser [x y])))]
    (println "✅ Макроэкспансия прошла успешно")
    (println "Результат:" (take 50 (str expanded))))
  (catch Exception e
    (println "❌ Ошибка макроэкспансии:" (.getMessage e))))

;; Тест 2: Проверка защиты от захвата переменных
(println "\n2. Проверка защиты от захвата переменных:")
(try
  (macroexpand '(parser/do-parse 
                  state (parser/expect-token :int)
                  (parser/return-parser state)))
  (println "❌ Защита от захвата 'state' НЕ работает")
  (catch Exception e
    (println "✅ Защита от захвата 'state' работает:" (.getMessage e))))

;; Тест 3: Проверка валидации четности аргументов
(println "\n3. Проверка валидации четности аргументов:")
(try
  (macroexpand '(parser/do-parse 
                  x (parser/expect-token :int)
                  (parser/return-parser x)
                  extra-arg))
  (println "❌ Валидация четности НЕ работает")
  (catch Exception e
    (println "✅ Валидация четности работает:" (.getMessage e))))

;; Тест 4: Базовая функциональность
(println "\n4. Тест базовой функциональности:")
(try
  (let [tokens (lexer/tokenize "int x;")
        initial-state (parser/parse-state tokens)
        parser-fn (parser/do-parse 
                    dummy1 (parser/expect-token :int)
                    dummy2 (parser/expect-token :identifier)
                    (parser/return-parser :success))
        result (parser-fn initial-state)]
    (println "Результат парсинга:" result)
    (if (:success? result)
      (println "✅ Базовый тест прошел успешно")
      (println "❌ Базовый тест провалился")))
  (catch Exception e
    (println "❌ Ошибка в базовом тесте:" (.getMessage e))))

;; Простой тест макроса do-parse
(println "\nТестирование макроса do-parse...")

;; Создаем простой парсер с do-parse
(def simple-parser
  (parser/do-parse
    x (parser/expect-token :number)
    _ (parser/expect-token-value :plus)
    y (parser/expect-token :number)
    (parser/return-parser (+ (:value x) (:value y)))))

;; Тестируем
(let [tokens (lexer/tokenize "42 + 24")
      state (parser/parse-state tokens)
      result (simple-parser state)]
  (println "Токены:" tokens)
  (println "Результат:" result))

(println "Тест завершен.")

(println "\n=== Тесты завершены ===")

(println "Тестируем токенизацию:")
(def tokens (lexer/tokenize "void foo(void) interrupt 2 { }"))
(println "Токены:")
(doseq [token tokens]
  (println "  " token)) 