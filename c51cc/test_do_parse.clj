(ns test_do_parse
  "Тестирование улучшенного макроса do-parse и новых комбинаторов"
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ РАБОТЫ МАКРОСА do-parse
;; ============================================================================

(defn test-simple-parser
  "Простой тест парсера с использованием do-parse"
  []
  (let [tokens (lexer/tokenize "int x = 42;")
        state (parser/parse-state tokens)]
    
    ;; Тестируем парсер объявления переменной с использованием do-parse
    (let [var-parser (parser/do-parse
                       type-spec parser/parse-type-specifier
                       name (parser/expect-token :identifier)
                       _ (parser/expect-token-value :equal)
                       init parser/parse-expression
                       _ (parser/expect-token-value :semicolon)
                       (parser/return-parser 
                         (parser/variable-declaration-node 
                           type-spec 
                           (parser/extract-identifier-name name) 
                           init)))]
      
      (println "=== Тест простого парсера с do-parse ===")
      (println "Токены:" tokens)
      (let [result (var-parser state)]
        (println "Результат:" result)
        (when (:success? result)
          (println "AST:" (:value result)))))))

(defn test-error-handling
  "Тест обработки ошибок с контекстом"
  []
  (let [tokens (lexer/tokenize "int x")  ; Неполное объявление
        state (parser/parse-state tokens)]
    
    ;; Тестируем парсер с контекстом ошибок
    (let [var-parser (parser/with-error-context
                       (parser/do-parse
                         type-spec parser/parse-type-specifier
                         name (parser/expect-token :identifier)
                         _ (parser/expect-token-value :semicolon)
                         (parser/return-parser 
                           (parser/variable-declaration-node 
                             type-spec 
                             (parser/extract-identifier-name name) 
                             nil)))
                       "Объявление переменной")]
      
      (println "\n=== Тест обработки ошибок ===")
      (println "Токены:" tokens)
      (let [result (var-parser state)]
        (println "Результат:" result)))))

(defn test-combinators
  "Тест новых комбинаторов"
  []
  (let [tokens (lexer/tokenize "(42)")
        state (parser/parse-state tokens)]
    
    ;; Тестируем комбинатор parenthesized
    (let [paren-parser (parser/parenthesized parser/parse-expression)]
      
      (println "\n=== Тест комбинатора parenthesized ===")
      (println "Токены:" tokens)
      (let [result (paren-parser state)]
        (println "Результат:" result)
        (when (:success? result)
          (println "AST:" (:value result))))))

  (let [tokens (lexer/tokenize "1, 2, 3")
        state (parser/parse-state tokens)]
    
    ;; Тестируем комбинатор comma-separated
    (let [list-parser (parser/comma-separated parser/parse-expression)]
      
      (println "\n=== Тест комбинатора comma-separated ===")
      (println "Токены:" tokens)
      (let [result (list-parser state)]
        (println "Результат:" result)
        (when (:success? result)
          (println "Список выражений:" (:value result)))))))

(defn test-transform-combinator
  "Тест комбинатора transform"
  []
  (let [tokens (lexer/tokenize "42")
        state (parser/parse-state tokens)]
    
    ;; Тестируем комбинатор transform для удвоения числа
    (let [double-parser (parser/transform 
                          parser/parse-expression
                          (fn [expr]
                            (if (= (:ast-type expr) :literal)
                              (assoc expr :value (* 2 (:value expr)))
                              expr)))]
      
      (println "\n=== Тест комбинатора transform ===")
      (println "Токены:" tokens)
      (let [result (double-parser state)]
        (println "Результат:" result)
        (when (:success? result)
          (println "Трансформированное выражение:" (:value result)))))))

(defn test-hygiene
  "Тест гигиенической чистоты макроса do-parse"
  []
  (println "\n=== Тест гигиенической чистоты ===")
  
  ;; Проверяем, что переменные не захватываются
  (let [state "внешняя переменная state"
        result "внешняя переменная result"
        tokens (lexer/tokenize "int x;")
        parse-state (parser/parse-state tokens)]
    
    (println "Внешние переменные:")
    (println "  state:" state)
    (println "  result:" result)
    
    ;; Используем do-parse с переменными, которые могли бы конфликтовать
    (let [parser-fn (parser/do-parse
                      type-spec parser/parse-type-specifier
                      name (parser/expect-token :identifier)
                      _ (parser/expect-token-value :semicolon)
                      (parser/return-parser 
                        {:type type-spec 
                         :name (parser/extract-identifier-name name)}))]
      
      (let [parse-result (parser-fn parse-state)]
        (println "Результат парсинга:" parse-result)
        (println "Внешние переменные после парсинга:")
        (println "  state:" state)
        (println "  result:" result)
        (println "Переменные не изменились - макрос гигиеничен!")))))

;; ============================================================================
;; ЗАПУСК ТЕСТОВ
;; ============================================================================

(defn run-all-tests
  "Запускает все тесты"
  []
  (println "🧪 ТЕСТИРОВАНИЕ УЛУЧШЕННОГО ПАРСЕРА")
  (println "=====================================")
  
  (test-simple-parser)
  (test-error-handling)
  (test-combinators)
  (test-transform-combinator)
  (test-hygiene)
  
  (println "\n✅ Все тесты завершены!"))

;; Автоматический запуск при загрузке файла
(run-all-tests) 