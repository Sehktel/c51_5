(ns debug-macro
  (:require [c51cc.parser :as parser]
            [clojure.pprint :as pprint]))

(println "=== ОТЛАДКА МАКРОЭКСПАНСИИ do-parse ===")

;; Отладка parse-expression-statement
(println "\n1. Макроэкспансия parse-expression-statement:")
(try
  (let [expanded (macroexpand '(parser/do-parse 
                                 expr (fn [state] (parser/parse-expression state))
                                 _ (parser/expect-token-value :semicolon)
                                 (parser/return-parser (parser/expression-statement-node expr))))]
    (println "Результат:")
    (pprint/pprint expanded))
  (catch Exception e
    (println "Ошибка:" (.getMessage e))))

;; Отладка parse-return-statement
(println "\n2. Макроэкспансия parse-return-statement:")
(try
  (let [expanded (macroexpand '(parser/do-parse 
                                 _ (parser/expect-token-value :return)
                                 expr (parser/optional (fn [state] (parser/parse-expression state)))
                                 _ (parser/expect-token-value :semicolon)
                                 (parser/return-parser (parser/return-statement-node expr))))]
    (println "Результат:")
    (pprint/pprint expanded))
  (catch Exception e
    (println "Ошибка:" (.getMessage e))))

;; Простая макроэкспансия
(println "\n3. Простая макроэкспансия:")
(try
  (let [expanded (macroexpand '(parser/do-parse 
                                 token-var (parser/expect-token :int)
                                 (fn [state] (parser/success token-var state))))]
    (println "Результат:")
    (pprint/pprint expanded))
  (catch Exception e
    (println "Ошибка:" (.getMessage e))))

;; Проверим, что генерирует макрос
(println "\n4. Тест с return-parser:")
(try
  (let [expanded (macroexpand '(parser/do-parse 
                                 token-var (parser/expect-token :int)
                                 (parser/return-parser token-var)))]
    (println "Результат:")
    (pprint/pprint expanded))
  (catch Exception e
    (println "Ошибка:" (.getMessage e))))

;; Проверим простейший случай
(println "\n5. Простейший случай:")
(try
  (let [expanded (macroexpand '(parser/do-parse (parser/return-parser :test)))]
    (println "Результат:")
    (pprint/pprint expanded))
  (catch Exception e
    (println "Ошибка:" (.getMessage e))))

(println "Отладка макроса do-parse")

;; Посмотрим, как разворачивается простой случай
(println "\nМакроэкспансия простого случая:")
(let [expanded (macroexpand-1 '(parser/do-parse
                                 num (parser/expect-token :number)
                                 (parser/return-parser (:value num))))]
  (println "Результат:")
  (pprint/pprint expanded))

;; Посмотрим полную экспансию
(println "\nПолная макроэкспансия:")
(let [expanded (macroexpand '(parser/do-parse
                               num (parser/expect-token :number)
                               (parser/return-parser (:value num))))]
  (println "Результат:")
  (pprint/pprint expanded)) 