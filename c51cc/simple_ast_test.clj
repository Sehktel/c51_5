(require '[c51cc.ast :as ast])

(println "=== ПРОСТОЙ ТЕСТ AST ===")

;; Тест создания простого узла
(def test-id (ast/identifier-node "test_variable"))
(println "Создан узел идентификатора:")
(ast/pretty-print test-id)

;; Тест создания литерала
(def test-lit (ast/literal-node 42 :number))
(println "\nСоздан узел литерала:")
(ast/pretty-print test-lit)

;; Тест создания бинарного выражения
(def test-expr (ast/binary-expression-node :plus test-id test-lit))
(println "\nСоздано бинарное выражение:")
(ast/pretty-print test-expr)

;; Тест валидации
(def errors (ast/validate-ast test-expr))
(println "\nРезультат валидации:")
(if (empty? errors)
  (println "✓ AST корректен")
  (println "✗ Найдены ошибки:" errors))

;; Тест анализа
(def identifiers (ast/collect-identifiers test-expr))
(println "\nИдентификаторы в выражении:" identifiers)

;; Тест оптимизации константного выражения
(def const-expr (ast/binary-expression-node 
                  :plus 
                  (ast/literal-node 5 :number) 
                  (ast/literal-node 10 :number)))
(println "\nИсходное константное выражение:")
(ast/pretty-print const-expr)

(def optimized (ast/optimize-ast const-expr))
(println "\nПосле оптимизации:")
(ast/pretty-print optimized)

(println "\n=== ТЕСТ ЗАВЕРШЕН УСПЕШНО ===") 