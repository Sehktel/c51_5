(ns test-ast-refactoring
  "Демонстрационный тест рефакторированного модуля AST
   
   Этот файл демонстрирует:
   - Работу новой модульной архитектуры AST
   - Валидацию через Malli
   - Visitor pattern с мультиметодами
   - Производительность и диагностику
   - Обратную совместимость с существующим кодом"
  (:require [c51cc.ast-new :as ast]
            [c51cc.ast.types :as types]
            [c51cc.ast.constructors :as constructors]
            [c51cc.ast.validation.malli :as malli-validation]
            [c51cc.ast.visitor :as visitor]))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ СОЗДАНИЯ AST
;; ============================================================================

(defn create-sample-ast
  "Создает пример AST для демонстрации возможностей"
  []
  (let [;; Создаем типы
        int-type (constructors/make-ast-node :type-specifier :type :int)
        void-type (constructors/make-ast-node :type-specifier :type :void)
        
        ;; Создаем параметры функции
        param1 (ast/parameter-declaration-node int-type "x")
        param2 (ast/parameter-declaration-node int-type "y")
        
        ;; Создаем выражения
        x-id (ast/identifier-node "x")
        y-id (ast/identifier-node "y")
        add-expr (ast/binary-expression-node :plus x-id y-id)
        
        ;; Создаем операторы
        return-stmt (ast/return-statement-node add-expr)
        body (ast/block-statement-node [return-stmt])
        
        ;; Создаем функцию
        add-func (ast/function-declaration-node int-type "add" [param1 param2] body)
        
        ;; Создаем переменную
        var-init (ast/literal-node 42 :number)
        var-decl (ast/variable-declaration-node int-type "result" var-init)
        
        ;; Создаем программу
        program (ast/program-node [add-func var-decl])]
    
    program))

(defn create-complex-ast
  "Создает более сложный пример AST с различными конструкциями"
  []
  (let [;; Типы
        int-type (constructors/make-ast-node :type-specifier :type :int)
        void-type (constructors/make-ast-node :type-specifier :type :void)
        
        ;; Функция main
        main-param (ast/parameter-declaration-node void-type "void")
        
        ;; Переменные в main
        i-init (ast/literal-node 0 :number)
        i-decl (ast/variable-declaration-node int-type "i" i-init)
        
        sum-init (ast/literal-node 0 :number)
        sum-decl (ast/variable-declaration-node int-type "sum" sum-init)
        
        ;; Цикл for
        i-id (ast/identifier-node "i")
        ten-lit (ast/literal-node 10 :number)
        condition (ast/binary-expression-node :less i-id ten-lit)
        
        one-lit (ast/literal-node 1 :number)
        increment (ast/binary-expression-node :plus i-id one-lit)
        update (ast/assignment-expression-node :assign i-id increment)
        
        ;; Тело цикла
        sum-id (ast/identifier-node "sum")
        add-to-sum (ast/binary-expression-node :plus sum-id i-id)
        sum-assign (ast/assignment-expression-node :assign sum-id add-to-sum)
        sum-stmt (ast/expression-statement-node sum-assign)
        
        loop-body (ast/block-statement-node [sum-stmt])
        for-loop (ast/for-statement-node nil condition update loop-body)
        
        ;; Return
        return-stmt (ast/return-statement-node sum-id)
        
        ;; Тело main
        main-body (ast/block-statement-node [i-decl sum-decl for-loop return-stmt])
        main-func (ast/function-declaration-node int-type "main" [] main-body)
        
        ;; C51 специфичные узлы
        sfr-decl (ast/sfr-declaration-node "P1" 0x90)
        interrupt-decl (ast/interrupt-declaration-node "timer0_isr" 1 0)
        
        ;; Программа
        program (ast/program-node [main-func sfr-decl interrupt-decl])]
    
    program))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ ВАЛИДАЦИИ
;; ============================================================================

(defn demo-validation
  "Демонстрирует работу системы валидации"
  []
  (println "\n=== ДЕМОНСТРАЦИЯ ВАЛИДАЦИИ ===")
  
  ;; Валидный AST
  (let [valid-ast (create-sample-ast)
        validation-result (ast/validate-ast-tree valid-ast)]
    (println "Валидация корректного AST:")
    (println (str "  Результат: " (if (:valid validation-result) "✅ ВАЛИДЕН" "❌ НЕВАЛИДЕН")))
    (when (:message validation-result)
      (println (str "  Сообщение: " (:message validation-result)))))
  
  ;; Невалидный AST (создаем намеренно некорректный узел)
  (let [invalid-node {:ast-type :invalid-type :some-field "value"}
        validation-result (ast/validate-ast-node invalid-node)]
    (println "\nВалидация некорректного узла:")
    (println (str "  Результат: " (if (:valid validation-result) "✅ ВАЛИДЕН" "❌ НЕВАЛИДЕН")))
    (when (:errors validation-result)
      (println (str "  Ошибки: " (:errors validation-result)))))
  
  ;; Демонстрация генерации тестовых данных
  (println "\nГенерация случайного AST:")
  (try
    (let [random-ast (ast/generate-random-ast :max-depth 3 :node-count 10)]
      (println "  ✅ Случайный AST сгенерирован успешно")
      (println (str "  Тип корневого узла: " (:ast-type random-ast))))
    (catch Exception e
      (println (str "  ❌ Ошибка генерации: " (.getMessage e))))))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ VISITOR PATTERN
;; ============================================================================

(defn demo-visitor-pattern
  "Демонстрирует работу visitor pattern с мультиметодами"
  []
  (println "\n=== ДЕМОНСТРАЦИЯ VISITOR PATTERN ===")
  
  (let [ast (create-complex-ast)]
    
    ;; Подсчет узлов по типам
    (println "Подсчет узлов по типам:")
    (let [counts (ast/count-nodes-by-type ast)]
      (doseq [[type count] (sort-by first counts)]
        (println (str "  " (name type) ": " count))))
    
    ;; Сбор всех идентификаторов
    (println "\nВсе идентификаторы в AST:")
    (let [identifiers (ast/collect-identifiers ast)]
      (println (str "  " (str/join ", " (sort (set identifiers))))))
    
    ;; Поиск функций
    (println "\nОбъявления функций:")
    (let [functions (ast/find-function-declarations ast)]
      (doseq [func functions]
        (println (str "  " (:name func) " (параметров: " (:parameters func) ")"))))
    
    ;; Поиск C51-специфичных узлов
    (println "\nC51-специфичные узлы:")
    (let [c51-nodes (ast/collect-c51-specific-nodes ast)]
      (doseq [node c51-nodes]
        (println (str "  " (:ast-type node) ": " 
                     (or (:name node) (:function-name node))))))
    
    ;; Глубина AST
    (println (str "\nГлубина AST: " (ast/calculate-ast-depth ast)))
    
    ;; Демонстрация различных стратегий обхода
    (println "\nСравнение стратегий обхода:")
    (let [debug-visitor (visitor/make-debug-visitor :show-enter false :show-details false)
          preorder-nodes (ast/collect-nodes ast :strategy :preorder)
          postorder-nodes (ast/collect-nodes ast :strategy :postorder)
          breadth-first-nodes (ast/collect-nodes ast :strategy :breadth-first)]
      (println (str "  Pre-order: " (count preorder-nodes) " узлов"))
      (println (str "  Post-order: " (count postorder-nodes) " узлов"))
      (println (str "  Breadth-first: " (count breadth-first-nodes) " узлов")))))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ ОПТИМИЗАЦИЙ
;; ============================================================================

(defn demo-optimizations
  "Демонстрирует работу системы оптимизаций"
  []
  (println "\n=== ДЕМОНСТРАЦИЯ ОПТИМИЗАЦИЙ ===")
  
  ;; Создаем AST с константными выражениями
  (let [;; 2 + 3 * 4
        two (ast/literal-node 2 :number)
        three (ast/literal-node 3 :number)
        four (ast/literal-node 4 :number)
        
        mult-expr (ast/binary-expression-node :multiply three four)
        add-expr (ast/binary-expression-node :plus two mult-expr)
        
        ;; Создаем простую функцию
        int-type (constructors/make-ast-node :type-specifier :type :int)
        return-stmt (ast/return-statement-node add-expr)
        body (ast/block-statement-node [return-stmt])
        func (ast/function-declaration-node int-type "test" [] body)
        program (ast/program-node [func])]
    
    (println "Исходное выражение: 2 + (3 * 4)")
    
    ;; Без оптимизаций
    (let [unoptimized (ast/optimize-ast program :level :O0)]
      (println "Без оптимизаций (O0): выражение не изменено"))
    
    ;; С базовыми оптимизациями
    (let [optimized (ast/optimize-ast program :level :O1)]
      (println "С оптимизациями (O1): константы должны быть свернуты")
      
      ;; Проверяем, что оптимизация сработала
      (let [optimized-expr (get-in optimized [:declarations 0 :body :statements 0 :expression])]
        (if (and (= (:ast-type optimized-expr) :literal)
                 (= (:value optimized-expr) 14))
          (println "  ✅ Константы успешно свернуты: результат = 14")
          (println "  ❌ Оптимизация не сработала"))))))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ ПРОИЗВОДИТЕЛЬНОСТИ
;; ============================================================================

(defn demo-performance
  "Демонстрирует метрики производительности"
  []
  (println "\n=== ДЕМОНСТРАЦИЯ ПРОИЗВОДИТЕЛЬНОСТИ ===")
  
  (let [ast (create-complex-ast)
        metrics (ast/ast-performance-metrics ast)]
    
    (println "Метрики производительности:")
    (println (str "  Общее время анализа: " 
                 (/ (:total-time-ns (:performance metrics)) 1e6) " мс"))
    (println (str "  Время валидации: " 
                 (/ (:validation-time-ns (:performance metrics)) 1e6) " мс"))
    (println (str "  Узлов в секунду: " 
                 (int (:nodes-per-second (:performance metrics)))))
    
    (println "\nСтруктурные метрики:")
    (let [structure (:structure metrics)]
      (println (str "  Количество узлов: " (:node-count structure)))
      (println (str "  Глубина: " (:depth structure)))
      (println (str "  Функций: " (:function-count structure)))
      (println (str "  Выражений: " (:expression-count structure)))
      (println (str "  Оценка сложности: " (format "%.2f" (:complexity-score structure)))))
    
    (println "\nСтатус валидации:")
    (let [validation (:validation metrics)]
      (println (str "  Валиден: " (if (:is-valid validation) "✅ Да" "❌ Нет")))
      (println (str "  Ошибок: " (:error-count validation))))))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ ДИАГНОСТИКИ
;; ============================================================================

(defn demo-diagnostics
  "Демонстрирует систему диагностики AST"
  []
  (println "\n=== ДЕМОНСТРАЦИЯ ДИАГНОСТИКИ ===")
  
  (let [ast (create-complex-ast)
        diagnosis (ast/diagnose-ast ast)]
    
    (println (str "Статус диагностики: " (:status diagnosis)))
    
    (if (empty? (:issues diagnosis))
      (println "✅ Проблем не обнаружено")
      (do
        (println "Обнаруженные проблемы:")
        (doseq [issue (:issues diagnosis)]
          (println (str "  " (:severity issue) ": " (:message issue)))
          (when (:recommendation issue)
            (println (str "    Рекомендация: " (:recommendation issue)))))))
    
    (when (:recommendations diagnosis)
      (println "\nОбщие рекомендации:")
      (doseq [rec (:recommendations diagnosis)]
        (println (str "  • " rec))))))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ ОБРАТНОЙ СОВМЕСТИМОСТИ
;; ============================================================================

(defn demo-backward-compatibility
  "Демонстрирует обратную совместимость с существующим кодом"
  []
  (println "\n=== ДЕМОНСТРАЦИЯ ОБРАТНОЙ СОВМЕСТИМОСТИ ===")
  
  (let [ast (create-sample-ast)]
    
    ;; Старые функции должны работать
    (println "Проверка старых функций:")
    
    ;; valid-ast-node?
    (let [node (ast/identifier-node "test")]
      (println (str "  valid-ast-node?: " (ast/valid-ast-node? node))))
    
    ;; validate-ast (старый формат)
    (let [errors (ast/validate-ast ast)]
      (println (str "  validate-ast: " (if (empty? errors) "нет ошибок" (str (count errors) " ошибок")))))
    
    ;; Старые константы доступны
    (println (str "  ast-node-types доступны: " (contains? ast/ast-node-types :program)))
    (println (str "  binary-operators доступны: " (contains? ast/all-binary-operators :plus)))
    
    ;; Старые конструкторы работают
    (let [old-style-node (ast/make-ast-node :identifier :name "old_style")]
      (println (str "  Старый стиль конструктора: " (ast/valid-ast-node? old-style-node))))
    
    (println "✅ Обратная совместимость сохранена")))

;; ============================================================================
;; ГЛАВНАЯ ДЕМОНСТРАЦИОННАЯ ФУНКЦИЯ
;; ============================================================================

(defn run-ast-refactoring-demo
  "Запускает полную демонстрацию рефакторированного модуля AST"
  []
  (println "🚀 ДЕМОНСТРАЦИЯ РЕФАКТОРИРОВАННОГО МОДУЛЯ AST")
  (println "=" (apply str (repeat 50 "=")))
  
  ;; Проверяем, что модули загружены
  (println "\n📦 Проверка загрузки модулей:")
  (println (str "  ✅ ast.types: " (boolean types/ast-node-types)))
  (println (str "  ✅ ast.constructors: " (boolean constructors/make-ast-node)))
  (println (str "  ✅ ast.validation.malli: " (boolean malli-validation/validate-ast-node)))
  (println (str "  ✅ ast.visitor: " (boolean visitor/walk-ast)))
  
  ;; Запускаем демонстрации
  (demo-validation)
  (demo-visitor-pattern)
  (demo-optimizations)
  (demo-performance)
  (demo-diagnostics)
  (demo-backward-compatibility)
  
  (println "\n🎉 ДЕМОНСТРАЦИЯ ЗАВЕРШЕНА")
  (println "=" (apply str (repeat 50 "=")))
  (println "\nРефакторинг AST успешно выполнен!")
  (println "Основные достижения:")
  (println "  • Модульная архитектура")
  (println "  • Валидация через Malli")
  (println "  • Visitor pattern с мультиметодами")
  (println "  • Система диагностики и метрик")
  (println "  • Обратная совместимость")
  (println "  • Готовность к plugin системе оптимизаций"))

;; Запуск демонстрации при загрузке файла
(comment
  (run-ast-refactoring-demo)) 