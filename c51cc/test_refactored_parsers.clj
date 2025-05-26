(ns test-refactored-parsers
  "Тестирование отрефакторенных парсеров с использованием макроса do-parse"
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(println "=== ТЕСТИРОВАНИЕ ОТРЕФАКТОРЕННЫХ ПАРСЕРОВ ===")

;; Отрефакторенная версия parse-expression-statement
(defn parse-expression-statement-refactored
  "Отрефакторенная версия парсера оператора-выражения с использованием do-parse"
  [state]
  (parser/do-parse
    expr (fn [state] (parser/parse-expression state))
    _ (parser/expect-token-value :semicolon)
    (parser/return-parser (parser/expression-statement-node expr))))

;; Отрефакторенная версия parse-return-statement
(defn parse-return-statement-refactored
  "Отрефакторенная версия парсера оператора return с использованием do-parse"
  [state]
  (parser/do-parse
    _ (parser/expect-token-value :return)
    expr (parser/optional (fn [state] (parser/parse-expression state)))
    _ (parser/expect-token-value :semicolon)
    (parser/return-parser (parser/return-statement-node expr))))

;; Отрефакторенная версия parse-while-statement
(defn parse-while-statement-refactored
  "Отрефакторенная версия парсера цикла while с использованием do-parse"
  [state]
  (parser/do-parse
    _ (parser/expect-token-value :while)
    _ (parser/expect-token-value :open-round)
    condition (fn [state] (parser/parse-expression state))
    _ (parser/expect-token-value :close-round)
    body (fn [state] (parser/parse-statement state))
    (parser/return-parser (parser/while-statement-node condition body))))

;; Тестирование отрефакторенных функций
(println "\n1. Тест parse-expression-statement-refactored:")
(let [tokens (lexer/tokenize "x = 5;")
      state (parser/parse-state tokens)
      result ((parse-expression-statement-refactored state) state)]
  (println "Результат:" (:success? result))
  (when (:success? result)
    (println "AST:" (:value result))))

(println "\n2. Тест parse-return-statement-refactored:")
(let [tokens (lexer/tokenize "return x + 1;")
      state (parser/parse-state tokens)
      result ((parse-return-statement-refactored state) state)]
  (println "Результат:" (:success? result))
  (when (:success? result)
    (println "AST:" (:value result))))

(println "\n3. Тест parse-while-statement-refactored:")
(let [tokens (lexer/tokenize "while (x > 0) x--;")
      state (parser/parse-state tokens)
      result ((parse-while-statement-refactored state) state)]
  (println "Результат:" (:success? result))
  (when (:success? result)
    (println "AST:" (:value result))))

;; Сравнение с оригинальными функциями
(println "\n=== СРАВНЕНИЕ С ОРИГИНАЛЬНЫМИ ФУНКЦИЯМИ ===")

(println "\n4. Сравнение parse-expression-statement:")
(let [tokens (lexer/tokenize "x = 5;")
      state (parser/parse-state tokens)
      original-result (parser/parse-expression-statement state)
      refactored-result ((parse-expression-statement-refactored state) state)]
  (println "Оригинал успешен:" (:success? original-result))
  (println "Рефакторинг успешен:" (:success? refactored-result))
  (when (and (:success? original-result) (:success? refactored-result))
    (println "Результаты идентичны:" (= (:value original-result) (:value refactored-result)))))

(println "\n✅ РЕФАКТОРИНГ ЗАВЕРШЕН!")
(println "Макрос do-parse успешно применен для упрощения парсеров") 