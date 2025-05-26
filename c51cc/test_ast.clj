(ns c51cc.test-ast
  "Тестирование модуля AST компилятора C51
   
   Демонстрирует:
   - Создание различных типов узлов AST
   - Валидацию структуры AST
   - Обход дерева с помощью visitor pattern
   - Анализ и трансформацию AST
   - Красивый вывод структуры"
  (:require [c51cc.ast :as ast]
            [c51cc.lexer :as lexer]
            [c51cc.parser :as parser]
            [clojure.string :as str]))

;; ============================================================================
;; ТЕСТОВЫЕ ДАННЫЕ
;; ============================================================================

(defn create-sample-ast
  "Создает пример AST для простой C51 программы:
   
   int main() {
       int x = 5;
       int y = x + 10;
       if (y > 10) {
           return y;
       }
       return 0;
   }"
  []
  (ast/program-node
    [(ast/function-declaration-node
       {:ast-type :type-keyword :base-type :int :value :int}
       "main"
       []
       (ast/block-statement-node
         [(ast/variable-declaration-node
            {:ast-type :type-keyword :base-type :int :value :int}
            "x"
            (ast/literal-node 5 :number))
          
          (ast/variable-declaration-node
            {:ast-type :type-keyword :base-type :int :value :int}
            "y"
            (ast/binary-expression-node
              :plus
              (ast/identifier-node "x")
              (ast/literal-node 10 :number)))
          
          (ast/if-statement-node
            (ast/binary-expression-node
              :greater
              (ast/identifier-node "y")
              (ast/literal-node 10 :number))
            (ast/return-statement-node
              (ast/identifier-node "y"))
            nil)
          
          (ast/return-statement-node
            (ast/literal-node 0 :number))]))]))

(defn create-complex-ast
  "Создает более сложный пример AST с циклами и вызовами функций:
   
   void delay(int ms) {
       for (int i = 0; i < ms; i++) {
           // пустое тело цикла
       }
   }
   
   int factorial(int n) {
       if (n <= 1) {
           return 1;
       }
       return n * factorial(n - 1);
   }"
  []
  (ast/program-node
    [(ast/function-declaration-node
       {:ast-type :type-keyword :base-type :void :value :void}
       "delay"
       [(ast/parameter-declaration-node
          {:ast-type :type-keyword :base-type :int :value :int}
          "ms")]
       (ast/block-statement-node
         [(ast/for-statement-node
            (ast/variable-declaration-node
              {:ast-type :type-keyword :base-type :int :value :int}
              "i"
              (ast/literal-node 0 :number))
            (ast/binary-expression-node
              :less
              (ast/identifier-node "i")
              (ast/identifier-node "ms"))
            (ast/unary-expression-node
              :post-increment
              (ast/identifier-node "i"))
            (ast/block-statement-node []))]))
     
     (ast/function-declaration-node
       {:ast-type :type-keyword :base-type :int :value :int}
       "factorial"
       [(ast/parameter-declaration-node
          {:ast-type :type-keyword :base-type :int :value :int}
          "n")]
       (ast/block-statement-node
         [(ast/if-statement-node
            (ast/binary-expression-node
              :less-equal
              (ast/identifier-node "n")
              (ast/literal-node 1 :number))
            (ast/return-statement-node
              (ast/literal-node 1 :number))
            nil)
          
          (ast/return-statement-node
            (ast/binary-expression-node
              :multiply
              (ast/identifier-node "n")
              (ast/call-expression-node
                (ast/identifier-node "factorial")
                [(ast/binary-expression-node
                   :minus
                   (ast/identifier-node "n")
                   (ast/literal-node 1 :number))])))]))]))

(defn create-c51-specific-ast
  "Создает AST с специфичными для C51 конструкциями:
   
   sfr P1 = 0x90;
   sbit LED = P1^0;
   
   void timer_interrupt() interrupt 1 using 2 {
       LED = !LED;
   }"
  []
  (ast/program-node
    [(ast/sfr-declaration-node "P1" 0x90)
     
     (ast/sbit-declaration-node "LED" 0x90)  ; P1^0
     
     (ast/interrupt-declaration-node
       "timer_interrupt"
       1
       2)]))

;; ============================================================================
;; РЕАЛИЗАЦИЯ VISITOR ДЛЯ ТЕСТИРОВАНИЯ
;; ============================================================================

(defrecord PrintVisitor [indent-level output]
  ast/ASTVisitor
  (visit-node [visitor node]
    (let [indent (apply str (repeat (:indent-level visitor) "  "))
          node-info (str indent "Посещение: " (name (:ast-type node)))]
      (swap! (:output visitor) conj node-info)
      node))
  
  (enter-node [visitor node]
    (let [indent (apply str (repeat (:indent-level visitor) "  "))
          enter-info (str indent "-> Вход в: " (name (:ast-type node)))]
      (swap! (:output visitor) conj enter-info)
      (->PrintVisitor (inc (:indent-level visitor)) (:output visitor))))
  
  (exit-node [visitor node]
    (let [indent (apply str (repeat (dec (:indent-level visitor)) "  "))
          exit-info (str indent "<- Выход из: " (name (:ast-type node)))]
      (swap! (:output visitor) conj exit-info)
      (->PrintVisitor (dec (:indent-level visitor)) (:output visitor)))))

(defrecord StatisticsVisitor [stats]
  ast/ASTVisitor
  (visit-node [visitor node]
    (swap! (:stats visitor) update (:ast-type node) (fnil inc 0))
    node)
  
  (enter-node [visitor node] visitor)
  (exit-node [visitor node] visitor))

;; ============================================================================
;; ТЕСТОВЫЕ ФУНКЦИИ
;; ============================================================================

(defn test-node-creation
  "Тестирует создание различных типов узлов AST"
  []
  (println "=== ТЕСТ: Создание узлов AST ===")
  
  ;; Тест создания простых узлов
  (let [id-node (ast/identifier-node "variable")
        lit-node (ast/literal-node 42 :number)
        bin-node (ast/binary-expression-node :plus id-node lit-node)]
    
    (println "Узел идентификатора:")
    (ast/pretty-print id-node)
    
    (println "\nУзел литерала:")
    (ast/pretty-print lit-node)
    
    (println "\nБинарное выражение:")
    (ast/pretty-print bin-node))
  
  ;; Тест создания сложных узлов
  (let [func-node (ast/function-declaration-node
                    {:ast-type :type-keyword :base-type :int :value :int}
                    "test_function"
                    [(ast/parameter-declaration-node
                       {:ast-type :type-keyword :base-type :int :value :int}
                       "param")]
                    (ast/block-statement-node
                      [(ast/return-statement-node
                         (ast/identifier-node "param"))]))]
    
    (println "\nОбъявление функции:")
    (ast/pretty-print func-node))
  
  (println "\n" (str/join "" (repeat 50 "="))))

(defn test-ast-validation
  "Тестирует валидацию AST"
  []
  (println "\n=== ТЕСТ: Валидация AST ===")
  
  ;; Тест валидного AST
  (let [valid-ast (create-sample-ast)
        errors (ast/validate-ast valid-ast)]
    (println "Валидация корректного AST:")
    (if (empty? errors)
      (println "✓ AST корректен")
      (do (println "✗ Найдены ошибки:")
          (doseq [error errors]
            (println "  -" (:message error))))))
  
  ;; Тест невалидного AST
  (let [invalid-ast {:ast-type :invalid-type :name "test"}
        errors (ast/validate-ast invalid-ast)]
    (println "\nВалидация некорректного AST:")
    (if (empty? errors)
      (println "✓ AST корректен")
      (do (println "✗ Найдены ошибки:")
          (doseq [error errors]
            (println "  -" (:message error))))))
  
  (println "\n" (str/join "" (repeat 50 "="))))

(defn test-ast-traversal
  "Тестирует обход AST с помощью visitor pattern"
  []
  (println "\n=== ТЕСТ: Обход AST ===")
  
  (let [sample-ast (create-sample-ast)
        output (atom [])
        visitor (->PrintVisitor 0 output)]
    
    (println "Обход AST с PrintVisitor:")
    (ast/walk-ast visitor sample-ast)
    
    (doseq [line @output]
      (println line)))
  
  (let [sample-ast (create-complex-ast)
        stats (atom {})
        visitor (->StatisticsVisitor stats)]
    
    (println "\nСбор статистики по узлам:")
    (ast/walk-ast visitor sample-ast)
    
    (doseq [[node-type count] (sort-by first @stats)]
      (println (str "  " (name node-type) ": " count))))
  
  (println "\n" (str/join "" (repeat 50 "="))))

(defn test-ast-analysis
  "Тестирует функции анализа AST"
  []
  (println "\n=== ТЕСТ: Анализ AST ===")
  
  (let [sample-ast (create-complex-ast)]
    
    (println "Анализ сложного AST:")
    (ast/print-ast-summary sample-ast)
    
    (println "\nИдентификаторы в AST:")
    (let [identifiers (ast/collect-identifiers sample-ast)]
      (println (str "  " (str/join ", " (sort identifiers)))))
    
    (println "\nВызовы функций:")
    (let [calls (ast/collect-function-calls sample-ast)]
      (doseq [call calls]
        (println (str "  " (:function call) " с " (:arguments call) " аргументами"))))
    
    (println "\nОбъявления функций:")
    (let [functions (ast/find-function-declarations sample-ast)]
      (doseq [func functions]
        (println (str "  " (:name func) " возвращает " 
                     (get-in func [:return-type :base-type])
                     " с " (:parameters func) " параметрами")))))
  
  (println "\n" (str/join "" (repeat 50 "="))))

(defn test-ast-transformation
  "Тестирует трансформацию и оптимизацию AST"
  []
  (println "\n=== ТЕСТ: Трансформация AST ===")
  
  ;; Создаем AST с константными выражениями для оптимизации
  (let [expr-ast (ast/binary-expression-node
                   :plus
                   (ast/literal-node 5 :number)
                   (ast/literal-node 10 :number))]
    
    (println "Исходное выражение:")
    (ast/pretty-print expr-ast)
    
    (println "\nПосле оптимизации:")
    (let [optimized (ast/optimize-ast expr-ast)]
      (ast/pretty-print optimized)))
  
  ;; Тест пользовательской трансформации
  (let [sample-ast (create-sample-ast)
        transform-fn (fn [node]
                      (if (and (= (:ast-type node) :identifier)
                               (= (:name node) "x"))
                        (ast/identifier-node "renamed_x")
                        node))]
    
    (println "\nПользовательская трансформация (переименование 'x' в 'renamed_x'):")
    (let [transformed (ast/transform-ast transform-fn sample-ast)]
      (println "Идентификаторы после трансформации:")
      (let [identifiers (ast/collect-identifiers transformed)]
        (println (str "  " (str/join ", " (sort identifiers)))))))
  
  (println "\n" (str/join "" (repeat 50 "="))))

(defn test-c51-specific-features
  "Тестирует специфичные для C51 возможности AST"
  []
  (println "\n=== ТЕСТ: Специфичные для C51 возможности ===")
  
  (let [c51-ast (create-c51-specific-ast)]
    
    (println "AST с C51-специфичными конструкциями:")
    (ast/pretty-print c51-ast)
    
    (println "\nСводка по C51 AST:")
    (ast/print-ast-summary c51-ast))
  
  (println "\n" (str/join "" (repeat 50 "="))))

(defn test-type-inference
  "Тестирует вывод типов выражений"
  []
  (println "\n=== ТЕСТ: Вывод типов ===")
  
  (let [expressions [(ast/literal-node 42 :number)
                     (ast/literal-node "hello" :string)
                     (ast/binary-expression-node
                       :plus
                       (ast/literal-node 5 :number)
                       (ast/literal-node 10 :number))
                     (ast/call-expression-node
                       (ast/identifier-node "function")
                       [(ast/literal-node 1 :number)])]]
    
    (println "Вывод типов для различных выражений:")
    (doseq [expr expressions]
      (let [inferred-type (ast/infer-expression-type expr)]
        (println (str "  " (name (:ast-type expr)) " -> " (name inferred-type))))))
  
  (println "\n" (str/join "" (repeat 50 "="))))

;; ============================================================================
;; ГЛАВНАЯ ФУНКЦИЯ ТЕСТИРОВАНИЯ
;; ============================================================================

(defn run-all-tests
  "Запускает все тесты модуля AST"
  []
  (println "ТЕСТИРОВАНИЕ МОДУЛЯ AST КОМПИЛЯТОРА C51")
  (println (str/join "" (repeat 60 "=")))
  
  (test-node-creation)
  (test-ast-validation)
  (test-ast-traversal)
  (test-ast-analysis)
  (test-ast-transformation)
  (test-c51-specific-features)
  (test-type-inference)
  
  (println "\n" (str/join "" (repeat 60 "=")))
  (println "ВСЕ ТЕСТЫ ЗАВЕРШЕНЫ"))

;; ============================================================================
;; ДЕМОНСТРАЦИОННЫЕ ФУНКЦИИ
;; ============================================================================

(defn demo-ast-pretty-printing
  "Демонстрирует различные варианты красивого вывода AST"
  []
  (println "\n=== ДЕМОНСТРАЦИЯ: Красивый вывод AST ===")
  
  (let [sample-ast (create-sample-ast)]
    
    (println "Стандартный вывод:")
    (ast/pretty-print sample-ast)
    
    (println "\nВывод с метаданными:")
    (ast/pretty-print sample-ast :show-metadata true)
    
    (println "\nВывод с увеличенным отступом:")
    (ast/pretty-print sample-ast :indent-size 4)))

(defn demo-ast-integration-with-parser
  "Демонстрирует интеграцию AST с парсером (концептуально)"
  []
  (println "\n=== ДЕМОНСТРАЦИЯ: Интеграция с парсером ===")
  
  ;; Это концептуальная демонстрация того, как AST интегрируется с парсером
  (let [source-code "int x = 5 + 10;"
        tokens (lexer/tokenize source-code)]
    
    (println (str "Исходный код: " source-code))
    (println (str "Токены: " tokens))
    
    ;; В реальной интеграции здесь был бы вызов парсера
    ;; который создает AST используя функции из ast.clj
    (let [simulated-ast (ast/variable-declaration-node
                          {:ast-type :type-keyword :base-type :int :value :int}
                          "x"
                          (ast/binary-expression-node
                            :plus
                            (ast/literal-node 5 :number)
                            (ast/literal-node 10 :number)))]
      
      (println "\nСимулированный AST:")
      (ast/pretty-print simulated-ast)
      
      (println "\nПосле оптимизации:")
      (let [optimized (ast/optimize-ast simulated-ast)]
        (ast/pretty-print optimized)))))

;; Для быстрого тестирования в REPL
(defn quick-test
  "Быстрый тест основной функциональности"
  []
  (let [ast (create-sample-ast)]
    (println "Быстрый тест AST:")
    (ast/print-ast-summary ast)
    (println "\nИдентификаторы:" (ast/collect-identifiers ast))))

;; Запуск тестов при загрузке файла (можно закомментировать)
;; (run-all-tests) 