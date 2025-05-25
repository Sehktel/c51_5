(require '[c51cc.parser :as p])
(require '[c51cc.lexer :as l])

(println "=== Тест гигиенического макроса do-parse ===")

;; Тест 1: Проверка макроэкспансии
(println "\n1. Проверка макроэкспансии:")
(try
  (let [expanded (macroexpand '(p/do-parse 
                                 x (p/expect-token :int)
                                 y (p/expect-token :identifier)
                                 (p/return-parser [x y])))]
    (println "✅ Макроэкспансия прошла успешно")
    (println "Результат:" (take 50 (str expanded))))
  (catch Exception e
    (println "❌ Ошибка макроэкспансии:" (.getMessage e))))

;; Тест 2: Проверка защиты от захвата переменных
(println "\n2. Проверка защиты от захвата переменных:")
(try
  (macroexpand '(p/do-parse 
                  state (p/expect-token :int)
                  (p/return-parser state)))
  (println "❌ Защита от захвата 'state' НЕ работает")
  (catch Exception e
    (println "✅ Защита от захвата 'state' работает:" (.getMessage e))))

;; Тест 3: Проверка валидации четности аргументов
(println "\n3. Проверка валидации четности аргументов:")
(try
  (macroexpand '(p/do-parse 
                  x (p/expect-token :int)
                  (p/return-parser x)
                  extra-arg))
  (println "❌ Валидация четности НЕ работает")
  (catch Exception e
    (println "✅ Валидация четности работает:" (.getMessage e))))

;; Тест 4: Базовая функциональность
(println "\n4. Тест базовой функциональности:")
(try
  (let [tokens (l/tokenize "int x;")
        initial-state (p/parse-state tokens)
        parser-fn (p/do-parse 
                    dummy1 (p/expect-token :int)
                    dummy2 (p/expect-token :identifier)
                    (p/return-parser :success))
        result (parser-fn initial-state)]
    (println "Результат парсинга:" result)
    (if (:success? result)
      (println "✅ Базовый тест прошел успешно")
      (println "❌ Базовый тест провалился")))
  (catch Exception e
    (println "❌ Ошибка в базовом тесте:" (.getMessage e))))

(println "\n=== Тесты завершены ===") 