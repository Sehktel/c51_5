(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "=== ДИАГНОСТИКА ПАРАМЕТРОВ ===")

;; Тестируем отдельные компоненты
(defn test-component [name component-fn input]
  (println (str "\n--- " name " ---"))
  (println (str "Вход: " input))
  (let [tokens (lexer/tokenize input)
        state (parser/parse-state tokens)]
    (println (str "Токены: " tokens))
    (try
      (let [result (component-fn state)]
        (if (:success? result)
          (do
            (println "✓ УСПЕХ")
            (println (str "Результат: " (:value result)))
            (println (str "Оставшиеся токены: " (drop (:position (:state result)) tokens))))
          (println (str "✗ ОШИБКА: " (:error result)))))
      (catch Exception e
        (println (str "✗ ИСКЛЮЧЕНИЕ: " (.getMessage e)))))))

;; Тестируем parse-type-specifier
(test-component "parse-type-specifier" 
                #(parser/parse-type-specifier %) 
                "int")

;; Тестируем parse-single-parameter
(test-component "parse-single-parameter" 
                #(parser/parse-single-parameter %) 
                "int a")

;; Тестируем parse-parameter-list с одним параметром
(test-component "parse-parameter-list (один параметр)" 
                #(parser/parse-parameter-list %) 
                "int a")

;; Тестируем parse-parameter-list с двумя параметрами
(test-component "parse-parameter-list (два параметра)" 
                #(parser/parse-parameter-list %) 
                "int a, int b")

;; Тестируем parse-parameter-list с void
(test-component "parse-parameter-list (void)" 
                #(parser/parse-parameter-list %) 
                "void")

;; Тестируем полную функцию по частям
(println "\n=== ТЕСТ ПОЛНОЙ ФУНКЦИИ ПО ЧАСТЯМ ===")
(let [code "int add(int a, int b) { return a + b; }"
      tokens (lexer/tokenize code)
      state (parser/parse-state tokens)]
  (println (str "Полный код: " code))
  (println (str "Все токены: " tokens))
  
  ;; Пробуем парсить тип возврата
  (println "\n1. Парсим тип возврата...")
  (let [type-result (parser/parse-type-specifier state)]
    (if (:success? type-result)
      (do
        (println "✓ Тип возврата: " (:value type-result))
        
        ;; Пробуем парсить имя функции
        (println "\n2. Парсим имя функции...")
        (let [name-result ((parser/expect-token :identifier) (:state type-result))]
          (if (:success? name-result)
            (do
              (println "✓ Имя функции: " (:value name-result))
              
              ;; Пробуем парсить открывающую скобку
              (println "\n3. Парсим открывающую скобку...")
              (let [paren-result ((parser/expect-token-value :open-round) (:state name-result))]
                (if (:success? paren-result)
                  (do
                    (println "✓ Открывающая скобка найдена")
                    
                    ;; Пробуем парсить параметры
                    (println "\n4. Парсим параметры...")
                    (let [params-result (parser/parse-parameter-list (:state paren-result))]
                      (if (:success? params-result)
                        (println "✓ Параметры: " (:value params-result))
                        (println "✗ Ошибка параметров: " (:error params-result)))))
                  (println "✗ Ошибка скобки: " (:error paren-result)))))
            (println "✗ Ошибка имени: " (:error name-result)))))
      (println "✗ Ошибка типа: " (:error type-result)))))

(println "\n=== КОНЕЦ ДИАГНОСТИКИ ===") 