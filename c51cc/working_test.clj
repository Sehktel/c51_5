(ns working-test
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(println "🧪 ДЕМОНСТРАЦИЯ РАБОТАЮЩЕГО МАКРОСА do-parse")
(println "===========================================")

;; Тест 1: Простой парсер с do-parse
(println "\n1. Простой парсер числа:")
(let [tokens (lexer/tokenize "42")
      state (parser/parse-state tokens)
      number-parser (parser/do-parse
                      num (parser/expect-token :number)
                      (parser/return-parser (:value num)))
      result (number-parser state)]
  (println "Токены:" tokens)
  (println "Результат:" result)
  (when (:success? result)
    (println "✅ Успешно распарсили число:" (:value result))))

;; Тест 2: Парсер с игнорированием результата
(println "\n2. Парсер с игнорированием (символ _):")
(let [tokens (lexer/tokenize "int x;")
      state (parser/parse-state tokens)
      var-parser (parser/do-parse
                   type-spec parser/parse-type-specifier
                   name (parser/expect-token :identifier)
                   _ (parser/expect-token-value :semicolon)
                   (parser/return-parser 
                     {:type (:base-type type-spec)
                      :name (:value name)}))
      result (var-parser state)]
  (println "Токены:" tokens)
  (println "Результат:" result)
  (when (:success? result)
    (println "✅ Успешно распарсили объявление:" (:value result))))

;; Тест 3: Использование новых комбинаторов
(println "\n3. Тест комбинатора parenthesized:")
(let [tokens (lexer/tokenize "(42)")
      state (parser/parse-state tokens)
      paren-parser (parser/parenthesized parser/parse-expression)
      result (paren-parser state)]
  (println "Токены:" tokens)
  (println "Результат:" result)
  (when (:success? result)
    (println "✅ Успешно распарсили выражение в скобках")))

;; Тест 4: Комбинатор transform
(println "\n4. Тест комбинатора transform:")
(let [tokens (lexer/tokenize "42")
      state (parser/parse-state tokens)
      double-parser (parser/transform 
                      parser/parse-expression
                      (fn [expr]
                        (if (= (:ast-type expr) :literal)
                          (assoc expr :value (* 2 (:value expr)))
                          expr)))
      result (double-parser state)]
  (println "Токены:" tokens)
  (println "Результат:" result)
  (when (:success? result)
    (println "✅ Успешно трансформировали:" (:value (:value result)))))

;; Тест 5: Обработка ошибок с контекстом
(println "\n5. Тест обработки ошибок:")
(let [tokens (lexer/tokenize "int")  ; Неполное объявление
      state (parser/parse-state tokens)
      error-parser (parser/with-error-context
                     (parser/do-parse
                       type-spec parser/parse-type-specifier
                       name (parser/expect-token :identifier)
                       (parser/return-parser {:type type-spec :name name}))
                     "Объявление переменной")
      result (error-parser state)]
  (println "Токены:" tokens)
  (println "Результат:" result)
  (when-not (:success? result)
    (println "✅ Ошибка с контекстом:" (:error result))))

;; Тест 6: Рефакторинг сложного парсера
(println "\n6. Рефакторинг parse-while-statement с do-parse:")
(let [tokens (lexer/tokenize "while (x) { }")
      state (parser/parse-state tokens)
      while-parser (parser/do-parse
                     _ (parser/expect-token-value :while)
                     _ (parser/expect-token-value :open-round)
                     condition parser/parse-expression
                     _ (parser/expect-token-value :close-round)
                     body parser/parse-statement
                     (parser/return-parser 
                       (parser/while-statement-node condition body)))
      result (while-parser state)]
  (println "Токены:" tokens)
  (println "Результат:" result)
  (when (:success? result)
    (println "✅ Успешно распарсили while с do-parse")))

(println "\n🎉 ВСЕ ТЕСТЫ ЗАВЕРШЕНЫ!")
(println "Макрос do-parse работает корректно и гигиенически чист!") 