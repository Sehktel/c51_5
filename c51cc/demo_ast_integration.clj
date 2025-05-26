(require '[c51cc.ast :as ast]
         '[c51cc.lexer :as lexer]
         '[clojure.string :as str])

(println "=== ДЕМОНСТРАЦИЯ ИНТЕГРАЦИИ AST С КОМПИЛЯТОРОМ C51 ===")
(println)

;; ============================================================================
;; ДЕМОНСТРАЦИЯ 1: Создание AST для реальной C51 программы
;; ============================================================================

(println "1. СОЗДАНИЕ AST ДЛЯ ПРОГРАММЫ МИГАНИЯ СВЕТОДИОДА")
(println "=" (str/join "" (repeat 50 "=")))

(def led-blink-ast
  (ast/program-node
    [(ast/sfr-declaration-node "P1" 0x90)
     (ast/sbit-declaration-node "LED" 0x90)
     
     (ast/function-declaration-node
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
       {:ast-type :type-keyword :base-type :void :value :void}
       "main"
       []
       (ast/block-statement-node
         [(ast/while-statement-node
            (ast/literal-node 1 :number)
            (ast/block-statement-node
              [(ast/assignment-expression-node
                 :assign
                 (ast/identifier-node "LED")
                 (ast/literal-node 1 :number))
               
               (ast/expression-statement-node
                 (ast/call-expression-node
                   (ast/identifier-node "delay")
                   [(ast/literal-node 500 :number)]))
               
               (ast/assignment-expression-node
                 :assign
                 (ast/identifier-node "LED")
                 (ast/literal-node 0 :number))
               
               (ast/expression-statement-node
                 (ast/call-expression-node
                   (ast/identifier-node "delay")
                   [(ast/literal-node 500 :number)]))]))]))]))

(println "Структура AST программы мигания светодиода:")
(ast/pretty-print led-blink-ast)

(println "\nАнализ программы:")
(ast/print-ast-summary led-blink-ast)

;; ============================================================================
;; ДЕМОНСТРАЦИЯ 2: Анализ и оптимизация AST
;; ============================================================================

(println "\n\n2. АНАЛИЗ И ОПТИМИЗАЦИЯ AST")
(println "=" (str/join "" (repeat 50 "=")))

;; Создаем AST с константными выражениями для демонстрации оптимизации
(def optimization-demo-ast
  (ast/program-node
    [(ast/function-declaration-node
       {:ast-type :type-keyword :base-type :int :value :int}
       "calculate"
       []
       (ast/block-statement-node
         [(ast/variable-declaration-node
            {:ast-type :type-keyword :base-type :int :value :int}
            "result"
            (ast/binary-expression-node
              :plus
              (ast/binary-expression-node
                :multiply
                (ast/literal-node 5 :number)
                (ast/literal-node 3 :number))
              (ast/binary-expression-node
                :minus
                (ast/literal-node 20 :number)
                (ast/literal-node 5 :number))))
          
          (ast/return-statement-node
            (ast/identifier-node "result"))]))]))

(println "Исходный AST с константными выражениями:")
(ast/pretty-print optimization-demo-ast)

(println "\nПосле оптимизации:")
(def optimized-ast (ast/optimize-ast optimization-demo-ast))
(ast/pretty-print optimized-ast)

;; ============================================================================
;; ДЕМОНСТРАЦИЯ 3: Visitor Pattern для анализа кода
;; ============================================================================

(println "\n\n3. АНАЛИЗ КОДА С ПОМОЩЬЮ VISITOR PATTERN")
(println "=" (str/join "" (repeat 50 "=")))

;; Создаем visitor для подсчета сложности кода
(defrecord ComplexityVisitor [complexity]
  ast/ASTVisitor
  (visit-node [visitor node]
    (case (:ast-type node)
      :if-statement (swap! (:complexity visitor) inc)
      :while-statement (swap! (:complexity visitor) inc)
      :for-statement (swap! (:complexity visitor) inc)
      :call-expression (swap! (:complexity visitor) inc)
      nil)
    node)
  
  (enter-node [visitor node] visitor)
  (exit-node [visitor node] visitor))

;; Создаем visitor для поиска потенциальных проблем
(defrecord CodeAnalysisVisitor [issues]
  ast/ASTVisitor
  (visit-node [visitor node]
    (case (:ast-type node)
      :while-statement
      (when (and (= (:ast-type (:condition node)) :literal)
                 (= (:value (:condition node)) 1))
        (swap! (:issues visitor) conj "Найден бесконечный цикл while(1)"))
      
      :binary-expression
      (when (and (= (:operator node) :divide)
                 (= (:ast-type (:right node)) :literal)
                 (= (:value (:right node)) 0))
        (swap! (:issues visitor) conj "Потенциальное деление на ноль"))
      
      nil)
    node)
  
  (enter-node [visitor node] visitor)
  (exit-node [visitor node] visitor))

;; Анализируем сложность программы мигания светодиода
(let [complexity (atom 0)
      visitor (->ComplexityVisitor complexity)]
  (ast/walk-ast visitor led-blink-ast)
  (println (str "Цикломатическая сложность программы: " @complexity)))

;; Ищем потенциальные проблемы
(let [issues (atom [])
      visitor (->CodeAnalysisVisitor issues)]
  (ast/walk-ast visitor led-blink-ast)
  (println "\nПотенциальные проблемы в коде:")
  (if (empty? @issues)
    (println "  Проблем не найдено")
    (doseq [issue @issues]
      (println (str "  - " issue)))))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ 4: Трансформация AST
;; ============================================================================

(println "\n\n4. ТРАНСФОРМАЦИЯ AST")
(println "=" (str/join "" (repeat 50 "=")))

;; Создаем трансформацию для замены магических чисел на константы
(defn replace-magic-numbers [ast]
  (ast/transform-ast
    (fn [node]
      (if (and (= (:ast-type node) :literal)
               (= (:literal-type node) :number)
               (= (:value node) 500))
        (ast/identifier-node "DELAY_MS")
        node))
    ast))

(println "Замена магических чисел на именованные константы:")
(println "\nИсходный код (фрагмент):")
(ast/pretty-print 
  (ast/call-expression-node
    (ast/identifier-node "delay")
    [(ast/literal-node 500 :number)]))

(println "\nПосле трансформации:")
(ast/pretty-print 
  (replace-magic-numbers
    (ast/call-expression-node
      (ast/identifier-node "delay")
      [(ast/literal-node 500 :number)])))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ 5: Интеграция с лексером
;; ============================================================================

(println "\n\n5. ИНТЕГРАЦИЯ С ЛЕКСЕРОМ")
(println "=" (str/join "" (repeat 50 "=")))

(def sample-code "int x = 5 + 10;")
(println (str "Исходный код: " sample-code))

(def tokens (lexer/tokenize sample-code))
(println (str "Токены: " tokens))

;; Симулируем создание AST из токенов
(def simulated-ast
  (ast/variable-declaration-node
    {:ast-type :type-keyword :base-type :int :value :int}
    "x"
    (ast/binary-expression-node
      :plus
      (ast/literal-node 5 :number)
      (ast/literal-node 10 :number))))

(println "\nСозданный AST:")
(ast/pretty-print simulated-ast)

(println "\nПосле оптимизации:")
(ast/pretty-print (ast/optimize-ast simulated-ast))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ 6: Валидация и отчеты об ошибках
;; ============================================================================

(println "\n\n6. ВАЛИДАЦИЯ И ОТЧЕТЫ ОБ ОШИБКАХ")
(println "=" (str/join "" (repeat 50 "=")))

;; Создаем некорректный AST для демонстрации валидации
(def invalid-ast
  {:ast-type :function-declaration
   :name ""  ; пустое имя - ошибка
   :return-type {:ast-type :type-keyword :base-type :int}
   :parameters []
   :body {:ast-type :block-statement :statements []}})

(def errors (ast/validate-ast invalid-ast))
(println "Результат валидации некорректного AST:")
(if (empty? errors)
  (println "✓ AST корректен")
  (do (println "✗ Найдены ошибки:")
      (doseq [error errors]
        (println (str "  - " (:message error))))))

;; ============================================================================
;; ЗАКЛЮЧЕНИЕ
;; ============================================================================

(println "\n\n" (str/join "" (repeat 60 "=")))
(println "ДЕМОНСТРАЦИЯ ЗАВЕРШЕНА")
(println (str/join "" (repeat 60 "=")))
(println)
(println "Модуль AST предоставляет:")
(println "✓ Полную систему типов узлов для C51")
(println "✓ Валидацию структуры AST")
(println "✓ Visitor pattern для обхода и анализа")
(println "✓ Оптимизацию константных выражений")
(println "✓ Трансформацию и рефакторинг кода")
(println "✓ Интеграцию с лексером и парсером")
(println "✓ Красивый вывод и отладочную информацию")
(println "✓ Специфичные для C51 конструкции")
(println)
(println "AST готов к использованию в компиляторе C51!") 