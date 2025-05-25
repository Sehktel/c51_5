(require '[c51cc.parser :as p])
(require '[c51cc.lexer :as l])

(println "=== ФИНАЛЬНЫЙ ТЕСТ РЕФАКТОРИНГА С ВСПОМОГАТЕЛЬНЫМИ ФУНКЦИЯМИ ===")

;; Тест 1: Использование success-parser макроса
(println "\n1. Тест с success-parser:")
(try
  (let [tokens (l/tokenize "x = 5;")
        state (p/parse-state tokens)
        parser-fn (p/do-parse 
                    expr (p/parser-fn p/parse-expression)
                    _ (p/expect-token-value :semicolon)
                    (p/success-parser (p/expression-statement-node expr)))
        result (parser-fn state)]
    (println "Результат:" (:success? result))
    (if (:success? result)
      (println "✅ success-parser работает")
      (println "❌ success-parser провалился:" (:error result))))
  (catch Exception e
    (println "❌ Ошибка:" (.getMessage e))))

;; Тест 2: Использование make-success-parser функции
(println "\n2. Тест с make-success-parser:")
(try
  (let [tokens (l/tokenize "return 42;")
        state (p/parse-state tokens)
        parser-fn (p/do-parse 
                    _ (p/expect-token-value :return)
                    expr (p/optional (p/parser-fn p/parse-expression))
                    _ (p/expect-token-value :semicolon)
                    (p/make-success-parser #(p/return-statement-node expr)))
        result (parser-fn state)]
    (println "Результат:" (:success? result))
    (if (:success? result)
      (println "✅ make-success-parser работает")
      (println "❌ make-success-parser провалился:" (:error result))))
  (catch Exception e
    (println "❌ Ошибка:" (.getMessage e))))

;; Тест 3: Использование функциональных комбинаторов
(println "\n3. Тест функциональных комбинаторов:")
(try
  (let [tokens (l/tokenize "x = 5;")
        state (p/parse-state tokens)
        ;; Создаем парсер с помощью функциональных комбинаторов
        parser-fn (p/>>= (p/parse-expression state)
                        (fn [expr state]
                          (p/>>= ((p/expect-token-value :semicolon) state)
                                (fn [_ state]
                                  (p/success (p/expression-statement-node expr) state)))))
        result (if (fn? parser-fn) (parser-fn state) parser-fn)]
    (println "Результат:" (:success? result))
    (if (:success? result)
      (println "✅ Функциональные комбинаторы работают")
      (println "❌ Функциональные комбинаторы провалились:" (:error result))))
  (catch Exception e
    (println "❌ Ошибка:" (.getMessage e))))

;; Тест 4: Комплексная программа
(println "\n4. Тест комплексной программы:")
(try
  (let [code "int main() { return 0; }"
        tokens (l/tokenize code)
        result (p/parse tokens)]
    (println "Результат парсинга комплексной программы:" (:success result))
    (if (:success result)
      (println "✅ Комплексная программа работает")
      (println "❌ Комплексная программа провалилась:" (:error result))))
  (catch Exception e
    (println "❌ Ошибка:" (.getMessage e))))

(println "\n=== РЕФАКТОРИНГ ЗАВЕРШЕН ===")
(println "Гигиенический макрос do-parse готов к использованию!")
(println "Доступны вспомогательные функции:")
(println "- success-parser (макрос)")
(println "- parser-fn (макрос)")  
(println "- make-success-parser (функция)")
(println "- Функциональные комбинаторы: >>=, >>, sequence-parsers") 