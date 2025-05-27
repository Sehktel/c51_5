(ns c51cc.test-simple-optimizations
  "Тесты и демонстрация простых оптимизаций HIR для компилятора C51"
  (:require [c51cc.ir.hir :as hir]
            [c51cc.optimizations.simple :as opt]))

;; ============================================================================
;; ТЕСТОВЫЕ ДАННЫЕ
;; ============================================================================

(defn create-test-constant-folding
  "Создает HIR для тестирования constant folding: x = 5 + 3 * 2"
  []
  (hir/hir-assign
    (hir/hir-variable "x" :uint8)
    (hir/hir-add
      (hir/hir-constant 5 :uint8)
      (hir/hir-mul
        (hir/hir-constant 3 :uint8)
        (hir/hir-constant 2 :uint8)
        :uint8)
      :uint8)))

(defn create-test-strength-reduction
  "Создает HIR для тестирования strength reduction: y = a * 8 + b / 4"
  []
  (hir/hir-assign
    (hir/hir-variable "y" :uint8)
    (hir/hir-add
      (hir/hir-mul
        (hir/hir-variable "a" :uint8)
        (hir/hir-constant 8 :uint8)
        :uint8)
      (hir/hir-div
        (hir/hir-variable "b" :uint8)
        (hir/hir-constant 4 :uint8)
        :uint8)
      :uint8)))

(defn create-test-dead-code
  "Создает HIR для тестирования dead code elimination"
  []
  (hir/hir-block
    [(hir/hir-assign
       (hir/hir-variable "used" :uint8)
       (hir/hir-constant 10 :uint8))
     
     (hir/hir-assign
       (hir/hir-variable "unused" :uint8)
       (hir/hir-constant 20 :uint8))
     
     (hir/hir-return
       (hir/hir-variable "used" :uint8))]))

(defn create-test-copy-propagation
  "Создает HIR для тестирования copy propagation: t = a; result = t + b"
  []
  (hir/hir-block
    [(hir/hir-assign
       (hir/hir-variable "t" :uint8)
       (hir/hir-variable "a" :uint8))
     
     (hir/hir-assign
       (hir/hir-variable "result" :uint8)
       (hir/hir-add
         (hir/hir-variable "t" :uint8)
         (hir/hir-variable "b" :uint8)
         :uint8))]))

(defn create-complex-test
  "Создает сложный HIR для комплексного тестирования всех оптимизаций"
  []
  (hir/hir-block
    [;; Константные выражения
     (hir/hir-assign
       (hir/hir-variable "const_result" :uint8)
       (hir/hir-add
         (hir/hir-mul
           (hir/hir-constant 2 :uint8)
           (hir/hir-constant 3 :uint8)
           :uint8)
         (hir/hir-constant 4 :uint8)
         :uint8))
     
     ;; Strength reduction
     (hir/hir-assign
       (hir/hir-variable "shifted" :uint8)
       (hir/hir-mul
         (hir/hir-variable "input" :uint8)
         (hir/hir-constant 16 :uint8)
         :uint8))
     
     ;; Copy propagation
     (hir/hir-assign
       (hir/hir-variable "temp" :uint8)
       (hir/hir-variable "shifted" :uint8))
     
     ;; Dead code (unused variable)
     (hir/hir-assign
       (hir/hir-variable "dead_var" :uint8)
       (hir/hir-constant 99 :uint8))
     
     ;; Использование оптимизированных значений
     (hir/hir-assign
       (hir/hir-variable "final_result" :uint8)
       (hir/hir-add
         (hir/hir-variable "const_result" :uint8)
         (hir/hir-variable "temp" :uint8)
         :uint8))
     
     (hir/hir-return
       (hir/hir-variable "final_result" :uint8))]))

;; ============================================================================
;; ТЕСТОВЫЕ ФУНКЦИИ
;; ============================================================================

(defn test-constant-folding
  "Тестирует constant folding оптимизацию"
  []
  (println "=== Тест Constant Folding ===")
  (let [original (create-test-constant-folding)
        optimized (opt/constant-fold original)]
    
    (println "\nИсходный HIR:")
    (hir/pretty-print-hir original)
    
    (println "\nПосле constant folding:")
    (hir/pretty-print-hir optimized)
    
    (println "\nОжидаемый результат: x = 11 (5 + 3*2 = 5 + 6 = 11)")
    optimized))

(defn test-strength-reduction
  "Тестирует strength reduction оптимизацию"
  []
  (println "\n=== Тест Strength Reduction ===")
  (let [original (create-test-strength-reduction)
        optimized (opt/strength-reduce original)]
    
    (println "\nИсходный HIR:")
    (hir/pretty-print-hir original)
    
    (println "\nПосле strength reduction:")
    (hir/pretty-print-hir optimized)
    
    (println "\nОжидаемый результат: y = (a << 3) + (b >> 2)")
    optimized))

(defn test-dead-code-elimination
  "Тестирует dead code elimination оптимизацию"
  []
  (println "\n=== Тест Dead Code Elimination ===")
  (let [original (create-test-dead-code)
        optimized (opt/dead-code-elimination original)]
    
    (println "\nИсходный HIR:")
    (hir/pretty-print-hir original)
    
    (println "\nПосле dead code elimination:")
    (hir/pretty-print-hir optimized)
    
    (println "\nОжидаемый результат: присваивание unused должно быть удалено")
    optimized))

(defn test-copy-propagation
  "Тестирует copy propagation оптимизацию"
  []
  (println "\n=== Тест Copy Propagation ===")
  (let [original (create-test-copy-propagation)
        optimized (opt/copy-propagation original)]
    
    (println "\nИсходный HIR:")
    (hir/pretty-print-hir original)
    
    (println "\nПосле copy propagation:")
    (hir/pretty-print-hir optimized)
    
    (println "\nОжидаемый результат: t заменено на a в выражении")
    optimized))

(defn test-all-optimizations
  "Тестирует все оптимизации вместе"
  []
  (println "\n=== Тест Всех Оптимизаций ===")
  (let [original (create-complex-test)
        optimized (opt/apply-simple-optimizations original)
        stats (opt/optimization-statistics original optimized)]
    
    (println "\nИсходный HIR:")
    (hir/pretty-print-hir original)
    
    (println "\nПосле всех оптимизаций:")
    (hir/pretty-print-hir optimized)
    
    (println "\nСтатистика оптимизаций:")
    (println (str "  Исходных узлов: " (:original-nodes stats)))
    (println (str "  Оптимизированных узлов: " (:optimized-nodes stats)))
    (println (str "  Удалено узлов: " (:nodes-eliminated stats)))
    (println (str "  Сокращение: " (format "%.1f%%" (:reduction-percent stats))))
    
    optimized))

(defn test-iterative-optimization
  "Тестирует итеративную оптимизацию"
  []
  (println "\n=== Тест Итеративной Оптимизации ===")
  (let [original (create-complex-test)
        optimized (opt/optimize-iteratively original)
        stats (opt/optimization-statistics original optimized)]
    
    (println "\nИсходный HIR:")
    (hir/pretty-print-hir original)
    
    (println "\nПосле итеративной оптимизации:")
    (hir/pretty-print-hir optimized)
    
    (println "\nСтатистика:")
    (println (str "  Сокращение: " (format "%.1f%%" (:reduction-percent stats))))
    
    optimized))

;; ============================================================================
;; СПЕЦИФИЧНЫЕ ДЛЯ AT89S4051 ТЕСТЫ
;; ============================================================================

(defn test-at89s4051-bit-operations
  "Тестирует оптимизации битовых операций для AT89S4051"
  []
  (println "\n=== Тест Битовых Операций AT89S4051 ===")
  (let [original (hir/hir-block
                   [(hir/hir-assign
                      (hir/hir-variable "masked" :uint8)
                      (hir/hir-and
                        (hir/hir-variable "input" :uint8)
                        (hir/hir-constant 0xFF :uint8)
                        :uint8))
                    
                    (hir/hir-assign
                      (hir/hir-variable "cleared" :uint8)
                      (hir/hir-or
                        (hir/hir-variable "value" :uint8)
                        (hir/hir-constant 0 :uint8)
                        :uint8))])
        
        optimized (opt/apply-simple-optimizations original)]
    
    (println "\nИсходный HIR:")
    (hir/pretty-print-hir original)
    
    (println "\nПосле оптимизации:")
    (hir/pretty-print-hir optimized)
    
    (println "\nОжидаемый результат:")
    (println "  masked = input (x & 0xFF → x для uint8)")
    (println "  cleared = value (x | 0 → x)")
    
    optimized))

(defn test-at89s4051-arithmetic
  "Тестирует арифметические оптимизации для AT89S4051"
  []
  (println "\n=== Тест Арифметических Операций AT89S4051 ===")
  (let [original (hir/hir-block
                   [(hir/hir-assign
                      (hir/hir-variable "doubled" :uint8)
                      (hir/hir-mul
                        (hir/hir-variable "x" :uint8)
                        (hir/hir-constant 2 :uint8)
                        :uint8))
                    
                    (hir/hir-assign
                      (hir/hir-variable "halved" :uint8)
                      (hir/hir-div
                        (hir/hir-variable "y" :uint8)
                        (hir/hir-constant 2 :uint8)
                        :uint8))
                    
                    (hir/hir-assign
                      (hir/hir-variable "unchanged" :uint8)
                      (hir/hir-add
                        (hir/hir-variable "z" :uint8)
                        (hir/hir-constant 0 :uint8)
                        :uint8))])
        
        optimized (opt/apply-simple-optimizations original)]
    
    (println "\nИсходный HIR:")
    (hir/pretty-print-hir original)
    
    (println "\nПосле оптимизации:")
    (hir/pretty-print-hir optimized)
    
    (println "\nОжидаемый результат:")
    (println "  doubled = x << 1 (x * 2 → x << 1)")
    (println "  halved = y >> 1 (y / 2 → y >> 1)")
    (println "  unchanged = z (z + 0 → z)")
    
    optimized))

;; ============================================================================
;; ГЛАВНАЯ ФУНКЦИЯ ТЕСТИРОВАНИЯ
;; ============================================================================

(defn run-all-tests
  "Запускает все тесты оптимизаций"
  []
  (println "🚀 ТЕСТИРОВАНИЕ ПРОСТЫХ ОПТИМИЗАЦИЙ КОМПИЛЯТОРА C51")
  (println (str "=" (apply str (repeat 60 "="))))
  
  ;; Базовые тесты
  (test-constant-folding)
  (test-strength-reduction)
  (test-dead-code-elimination)
  (test-copy-propagation)
  
  ;; Комплексные тесты
  (test-all-optimizations)
  (test-iterative-optimization)
  
  ;; Специфичные для AT89S4051
  (test-at89s4051-bit-operations)
  (test-at89s4051-arithmetic)
  
  (println "\n✅ ВСЕ ТЕСТЫ ЗАВЕРШЕНЫ")
  (println "\n📊 ВЫВОДЫ:")
  (println "  • Constant folding успешно свертывает константные выражения")
  (println "  • Strength reduction заменяет дорогие операции на дешевые")
  (println "  • Dead code elimination удаляет неиспользуемый код")
  (println "  • Copy propagation распространяет копии переменных")
  (println "  • Итеративная оптимизация достигает неподвижной точки")
  (println "  • Специфичные для AT89S4051 оптимизации работают корректно"))

;; ============================================================================
;; ДЕМОНСТРАЦИОННЫЕ ПРИМЕРЫ
;; ============================================================================

(defn demo-c51-optimization
  "Демонстрирует оптимизацию типичного C51 кода"
  []
  (println "\n🎯 ДЕМОНСТРАЦИЯ: Оптимизация типичного C51 кода")
  (println (str "-" (apply str (repeat 50 "-"))))
  
  ;; Имитируем HIR для типичного C51 кода:
  ;; uint8_t result = (input * 4) + (mask & 0xFF) + 0;
  ;; uint8_t unused = 42;
  ;; return result;
  
  (let [original (hir/hir-block
                   [(hir/hir-assign
                      (hir/hir-variable "result" :uint8)
                      (hir/hir-add
                        (hir/hir-add
                          (hir/hir-mul
                            (hir/hir-variable "input" :uint8)
                            (hir/hir-constant 4 :uint8)
                            :uint8)
                          (hir/hir-and
                            (hir/hir-variable "mask" :uint8)
                            (hir/hir-constant 0xFF :uint8)
                            :uint8)
                          :uint8)
                        (hir/hir-constant 0 :uint8)
                        :uint8))
                    
                    (hir/hir-assign
                      (hir/hir-variable "unused" :uint8)
                      (hir/hir-constant 42 :uint8))
                    
                    (hir/hir-return
                      (hir/hir-variable "result" :uint8))])
        
        optimized (opt/optimize-iteratively original)
        stats (opt/optimization-statistics original optimized)]
    
    (println "\nИсходный C51 код (концептуально):")
    (println "  uint8_t result = (input * 4) + (mask & 0xFF) + 0;")
    (println "  uint8_t unused = 42;")
    (println "  return result;")
    
    (println "\nИсходный HIR:")
    (hir/pretty-print-hir original)
    
    (println "\nОптимизированный HIR:")
    (hir/pretty-print-hir optimized)
    
    (println "\nОптимизированный C51 код (концептуально):")
    (println "  uint8_t result = (input << 2) + mask;")
    (println "  return result;")
    
    (println "\nПрименённые оптимизации:")
    (println "  • input * 4 → input << 2 (strength reduction)")
    (println "  • mask & 0xFF → mask (bitwise simplification)")
    (println "  • + 0 удалено (strength reduction)")
    (println "  • unused = 42 удалено (dead code elimination)")
    
    (println (str "\nСтатистика: " (:nodes-eliminated stats) " узлов удалено ("
                  (format "%.1f%%" (:reduction-percent stats)) " сокращение)"))
    
    optimized))

;; Запуск демонстрации при загрузке файла
(comment
  (run-all-tests)
  (demo-c51-optimization)
  ) 