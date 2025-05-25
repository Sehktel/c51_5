(ns test-hygiene
  "Тест гигиенического макроса do-parse"
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

;; Тест 1: Базовая функциональность
(defn test-basic-hygiene []
  (println "=== Тест базовой функциональности ===")
  (let [tokens (lexer/tokenize "int x;")
        initial-state (parser/parse-state tokens)]
    
    ;; Тестируем новый гигиенический макрос - простой случай
    (try
      (let [parser-fn (parser/do-parse 
                        _ (parser/expect-token :int)
                        _ (parser/expect-token :identifier)
                        (parser/return-parser :success))
            result (parser-fn initial-state)]
        (println "Результат парсинга:" result)
        (if (:success? result)
          (println "✅ Базовый тест прошел успешно")
          (println "❌ Базовый тест провалился")))
      (catch Exception e
        (println "❌ Ошибка в базовом тесте:" (.getMessage e))))))

;; Тест 2: Проверка защиты от захвата переменных
(defn test-variable-capture-protection []
  (println "\n=== Тест защиты от захвата переменных ===")
  
  ;; Тест с запрещенным именем 'state'
  (try
    (macroexpand '(parser/do-parse 
                    state (parser/expect-token :int)
                    (parser/return-parser state)))
    (println "❌ Защита от захвата 'state' не работает")
    (catch Exception e
      (println "✅ Защита от захвата 'state' работает:" (.getMessage e))))
  
  ;; Тест с запрещенным именем 'result'
  (try
    (macroexpand '(parser/do-parse 
                    result (parser/expect-token :int)
                    (parser/return-parser result)))
    (println "❌ Защита от захвата 'result' не работает")
    (catch Exception e
      (println "✅ Защита от захвата 'result' работает:" (.getMessage e)))))

;; Тест 3: Проверка валидации входных данных
(defn test-input-validation []
  (println "\n=== Тест валидации входных данных ===")
  
  ;; Тест с нечетным количеством аргументов
  (try
    (macroexpand '(parser/do-parse 
                    token (parser/expect-token :int)
                    (parser/return-parser token)
                    extra-arg))  ; Нечетное количество
    (println "❌ Валидация четности аргументов не работает")
    (catch Exception e
      (println "✅ Валидация четности аргументов работает:" (.getMessage e)))))

;; Тест 4: Сравнение с функциональными комбинаторами
(defn test-functional-combinators []
  (println "\n=== Тест функциональных комбинаторов ===")
  (let [tokens (lexer/tokenize "int x;")
        initial-state (parser/parse-state tokens)]
    
    ;; Используем функциональные комбинаторы
    (try
      (let [parser-fn (parser/sequence-parsers 
                        (parser/expect-token :int)
                        (parser/expect-token :identifier))
            result (parser-fn initial-state)]
        (println "Результат функциональных комбинаторов:" result)
        (if (:success? result)
          (println "✅ Функциональные комбинаторы работают")
          (println "❌ Функциональные комбинаторы не работают")))
      (catch Exception e
        (println "❌ Ошибка в функциональных комбинаторах:" (.getMessage e))))))

;; Запуск всех тестов
(defn run-all-tests []
  (println "Запуск тестов гигиенического макроса do-parse\n")
  (test-basic-hygiene)
  (test-variable-capture-protection)
  (test-input-validation)
  (test-functional-combinators)
  (println "\n=== Тесты завершены ==="))

;; Автоматический запуск при загрузке файла
(run-all-tests) 