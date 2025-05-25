(require '[c51cc.parser :as p])
(require '[c51cc.lexer :as l])

(println "=== ФИНАЛЬНЫЙ ТЕСТ ГИГИЕНИЧЕСКОГО МАКРОСА do-parse ===")

;; Тест 1: Простейший случай - только финальное выражение
(println "\n1. Простейший случай:")
(try
  (let [tokens (l/tokenize "int")
        state (p/parse-state tokens)
        parser-fn (p/do-parse (p/return-parser :success))
        result (parser-fn state)]
    (println "Результат:" result)
    (if (:success? result)
      (println "✅ Простейший тест прошел")
      (println "❌ Простейший тест провалился")))
  (catch Exception e
    (println "❌ Ошибка:" (.getMessage e))))

;; Тест 2: Одна пара binding-expression + финальное выражение
(println "\n2. Одна пара binding-expression:")
(try
  (let [tokens (l/tokenize "int")
        state (p/parse-state tokens)
        parser-fn (p/do-parse 
                    token-var (p/expect-token :int)
                    (p/return-parser token-var))
        result (parser-fn state)]
    (println "Результат:" result)
    (if (:success? result)
      (println "✅ Тест с одной парой прошел")
      (println "❌ Тест с одной парой провалился")))
  (catch Exception e
    (println "❌ Ошибка:" (.getMessage e))))

;; Тест 3: Две пары binding-expression + финальное выражение
(println "\n3. Две пары binding-expression:")
(try
  (let [tokens (l/tokenize "int x")
        state (p/parse-state tokens)
        parser-fn (p/do-parse 
                    type-token-var (p/expect-token :int)
                    name-token-var (p/expect-token :identifier)
                    (p/return-parser {:type type-token-var :name name-token-var}))
        result (parser-fn state)]
    (println "Результат:" result)
    (if (:success? result)
      (println "✅ Тест с двумя парами прошел")
      (println "❌ Тест с двумя парами провалился")))
  (catch Exception e
    (println "❌ Ошибка:" (.getMessage e))))

;; Тест 4: Защита от захвата переменных
(println "\n4. Защита от захвата переменных:")
(try
  (macroexpand '(p/do-parse 
                  state (p/expect-token :int)
                  (p/return-parser state)))
  (println "❌ Защита НЕ работает")
  (catch Exception e
    (println "✅ Защита работает:" (.getMessage e))))

;; Тест 5: Валидация нечетности аргументов
(println "\n5. Валидация нечетности аргументов:")
(try
  (macroexpand '(p/do-parse 
                  x (p/expect-token :int)))  ; Четное количество - ошибка
  (println "❌ Валидация НЕ работает")
  (catch Exception e
    (println "✅ Валидация работает:" (.getMessage e))))

(println "\n=== ТЕСТЫ ЗАВЕРШЕНЫ ===") 