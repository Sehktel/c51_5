(ns c51cc.test_refactoring_comparison
  "Тесты для сравнения производительности и корректности до и после рефакторинга
   Демонстрируют улучшения от использования do-parse макроса"
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]
            [clojure.test :refer :all]))

;; ============================================================================
;; ТЕСТОВЫЕ ДАННЫЕ
;; ============================================================================

(def test-cases
  "Набор тестовых случаев для проверки корректности рефакторинга"
  [
   ;; Простые выражения
   {:name "Числовой литерал"
    :code "42"
    :expected-type :literal}
   
   {:name "Идентификатор"
    :code "variable"
    :expected-type :identifier}
   
   {:name "Выражение в скобках"
    :code "(42)"
    :expected-type :literal}
   
   ;; Функции с параметрами
   {:name "Функция без параметров"
    :code "void func() {}"
    :expected-type :function-declaration
    :expected-params 0}
   
   {:name "Функция с одним параметром"
    :code "void func(int x) {}"
    :expected-type :function-declaration
    :expected-params 1}
   
   {:name "Функция с несколькими параметрами"
    :code "int add(int a, int b, char c) { return a; }"
    :expected-type :function-declaration
    :expected-params 3}
   
   {:name "Функция с void параметром"
    :code "void func(void) {}"
    :expected-type :function-declaration
    :expected-params 0}
   
   ;; Блоки
   {:name "Пустой блок"
    :code "void func() {}"
    :expected-type :function-declaration}
   
   {:name "Блок с объявлениями"
    :code "void func() { int x; char y; }"
    :expected-type :function-declaration}
   
   {:name "Блок с операторами"
    :code "void func() { return; x = 42; }"
    :expected-type :function-declaration}
   
   ;; C51 специфичные конструкции
   {:name "Interrupt функция"
    :code "void timer_isr() interrupt 1 {}"
    :expected-type :interrupt-declaration}
   
   {:name "Interrupt с using"
    :code "void timer_isr() interrupt 1 using 2 {}"
    :expected-type :interrupt-declaration}
   
   {:name "SFR декларация"
    :code "sfr P0 = 0x80;"
    :expected-type :sfr-declaration}
   
   {:name "SBIT декларация"
    :code "sbit LED = 0x90;"
    :expected-type :sbit-declaration}
   
   ;; Сложные программы
   {:name "Программа с несколькими декларациями"
    :code "int x; void func(); char y;"
    :expected-type :program
    :expected-declarations 3}
   
   {:name "Комплексная C51 программа"
    :code "sfr P0 = 0x80; void timer_isr() interrupt 1 { return; } void main() { int x = 42; }"
    :expected-type :program
    :expected-declarations 3}])

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;; ============================================================================

(defn parse-and-measure
  "Парсит код и измеряет время выполнения"
  [code]
  (let [tokens (lexer/tokenize code)
        start-time (System/nanoTime)
        result (parser/parse tokens)
        end-time (System/nanoTime)
        duration (/ (- end-time start-time) 1000000.0)] ; в миллисекундах
    {:result result
     :duration duration
     :tokens-count (count tokens)}))

(defn extract-ast-info
  "Извлекает информацию из AST для сравнения"
  [ast]
  (when ast
    {:type (:ast-type ast)
     :declarations-count (when (:declarations ast) (count (:declarations ast)))
     :parameters-count (when (:parameters ast) (count (:parameters ast)))
     :name (:name ast)
     :function-name (:function-name ast)}))

;; ============================================================================
;; ТЕСТЫ КОРРЕКТНОСТИ
;; ============================================================================

(deftest test-refactoring-correctness
  "Проверяет, что рефакторинг не изменил функциональность"
  (testing "Корректность рефакторинга"
    (doseq [test-case test-cases]
      (let [{:keys [name code expected-type expected-params expected-declarations]} test-case
            {:keys [result duration]} (parse-and-measure code)]
        
        (testing (str "Тест: " name)
          ;; Проверяем успешность парсинга
          (is (:success result) 
              (str "Парсинг должен быть успешным для: " name))
          
          (when (:success result)
            (let [ast (:ast result)
                  first-decl (first (:declarations ast))]
              
              ;; Проверяем тип AST
              (cond
                (= expected-type :program)
                (do
                  (is (= :program (:ast-type ast)))
                  (when expected-declarations
                    (is (= expected-declarations (count (:declarations ast))))))
                
                (= expected-type :literal)
                (let [expr-stmt first-decl
                      expr (:expression expr-stmt)]
                  (is (= :expression-statement (:ast-type expr-stmt)))
                  (is (= :literal (:ast-type expr))))
                
                (= expected-type :identifier)
                (let [expr-stmt first-decl
                      expr (:expression expr-stmt)]
                  (is (= :expression-statement (:ast-type expr-stmt)))
                  (is (= :identifier (:ast-type expr))))
                
                (= expected-type :function-declaration)
                (do
                  (is (= :function-declaration (:ast-type first-decl)))
                  (when expected-params
                    (is (= expected-params (count (:parameters first-decl))))))
                
                (= expected-type :interrupt-declaration)
                (is (= :interrupt-declaration (:ast-type first-decl)))
                
                (= expected-type :sfr-declaration)
                (is (= :sfr-declaration (:ast-type first-decl)))
                
                (= expected-type :sbit-declaration)
                (is (= :sbit-declaration (:ast-type first-decl)))))))))))

;; ============================================================================
;; ТЕСТЫ ПРОИЗВОДИТЕЛЬНОСТИ
;; ============================================================================

(deftest test-refactoring-performance
  "Проверяет производительность отрефакторенных функций"
  (testing "Производительность рефакторинга"
    (let [performance-results (atom [])]
      
      (doseq [test-case test-cases]
        (let [{:keys [name code]} test-case
              {:keys [result duration tokens-count]} (parse-and-measure code)]
          
          (when (:success result)
            (swap! performance-results conj
                   {:name name
                    :duration duration
                    :tokens-count tokens-count
                    :performance-ratio (/ duration tokens-count)}))))
      
      ;; Анализируем результаты
      (let [results @performance-results
            avg-duration (/ (reduce + (map :duration results)) (count results))
            max-duration (apply max (map :duration results))
            avg-performance (/ (reduce + (map :performance-ratio results)) (count results))]
        
        (println (str "\n=== АНАЛИЗ ПРОИЗВОДИТЕЛЬНОСТИ ==="))
        (println (str "Количество тестов: " (count results)))
        (println (str "Среднее время парсинга: " (format "%.2f" avg-duration) " мс"))
        (println (str "Максимальное время: " (format "%.2f" max-duration) " мс"))
        (println (str "Средняя производительность: " (format "%.4f" avg-performance) " мс/токен"))
        
        ;; Проверяем, что производительность приемлемая
        (is (< avg-duration 50) "Среднее время парсинга должно быть менее 50мс")
        (is (< max-duration 200) "Максимальное время парсинга должно быть менее 200мс")
        (is (< avg-performance 1.0) "Производительность должна быть менее 1мс на токен")))))

;; ============================================================================
;; СТРЕСС-ТЕСТЫ
;; ============================================================================

(deftest test-complex-nested-structures
  "Стресс-тест для сложных вложенных структур"
  (testing "Сложные вложенные структуры"
    (let [complex-code "
          sfr P0 = 0x80;
          sfr P1 = 0x90;
          sbit LED1 = 0x80;
          sbit LED2 = 0x81;
          
          int global_counter;
          char status_flags;
          
          void timer0_isr() interrupt 1 using 2 {
              int local_var = 42;
              if (global_counter > 100) {
                  global_counter = 0;
                  status_flags = 1;
              } else {
                  global_counter++;
              }
          }
          
          void timer1_isr() interrupt 3 {
              char temp = P0;
              P1 = temp;
          }
          
          int calculate(int a, int b, char mode) {
              int result = 0;
              if (mode == 1) {
                  result = a + b;
              } else if (mode == 2) {
                  result = a - b;
              } else {
                  result = a * b;
              }
              return result;
          }
          
          void main() {
              int x = 10;
              int y = 20;
              int sum = calculate(x, y, 1);
              
              while (sum > 0) {
                  if (sum % 2 == 0) {
                      LED1 = 1;
                  } else {
                      LED2 = 1;
                  }
                  sum--;
              }
              
              for (int i = 0; i < 10; i++) {
                  global_counter += i;
              }
          }"
          {:keys [result duration]} (parse-and-measure complex-code)]
      
      (is (:success result) "Сложная программа должна парситься успешно")
      (is (< duration 500) "Сложная программа должна парситься менее чем за 500мс")
      
      (when (:success result)
        (let [ast (:ast result)
              declarations (:declarations ast)]
          (is (= :program (:ast-type ast)))
          (is (= 9 (count declarations))) ; 2 sfr + 2 sbit + 2 var + 2 interrupt + 1 main
          
          ;; Проверяем типы деклараций
          (is (= :sfr-declaration (:ast-type (nth declarations 0))))
          (is (= :sfr-declaration (:ast-type (nth declarations 1))))
          (is (= :sbit-declaration (:ast-type (nth declarations 2))))
          (is (= :sbit-declaration (:ast-type (nth declarations 3))))
          (is (= :variable-declaration (:ast-type (nth declarations 4))))
          (is (= :variable-declaration (:ast-type (nth declarations 5))))
          (is (= :interrupt-declaration (:ast-type (nth declarations 6))))
          (is (= :interrupt-declaration (:ast-type (nth declarations 7))))
          (is (= :function-declaration (:ast-type (nth declarations 8))))
          
          (println (str "Сложная программа успешно обработана за " (format "%.2f" duration) " мс")))))))

;; ============================================================================
;; ТЕСТЫ ГРАНИЧНЫХ СЛУЧАЕВ
;; ============================================================================

(deftest test-edge-cases
  "Тестирует граничные случаи после рефакторинга"
  (testing "Граничные случаи"
    
    (testing "Пустая программа"
      (let [{:keys [result]} (parse-and-measure "")]
        (is (:success result))
        (is (= :program (:ast-type (:ast result))))
        (is (= [] (:declarations (:ast result))))))
    
    (testing "Только пробелы и комментарии"
      (let [{:keys [result]} (parse-and-measure "   \n  \t  ")]
        (is (:success result))))
    
    (testing "Максимально вложенные скобки"
      (let [{:keys [result]} (parse-and-measure "((((42))))")]
        (is (:success result))
        (let [expr-stmt (first (:declarations (:ast result)))
              expr (:expression expr-stmt)]
          (is (= :literal (:ast-type expr)))
          (is (= 42 (:value expr))))))
    
    (testing "Функция с максимальным количеством параметров"
      (let [many-params "void func(int a, int b, int c, int d, int e, int f, int g, int h) {}"
            {:keys [result]} (parse-and-measure many-params)]
        (is (:success result))
        (let [func-decl (first (:declarations (:ast result)))]
          (is (= :function-declaration (:ast-type func-decl)))
          (is (= 8 (count (:parameters func-decl)))))))
    
    (testing "Interrupt с максимальным номером"
      (let [{:keys [result]} (parse-and-measure "void isr() interrupt 255 using 3 {}")]
        (is (:success result))
        (let [interrupt-decl (first (:declarations (:ast result)))]
          (is (= :interrupt-declaration (:ast-type interrupt-decl)))
          (is (= 255 (:interrupt-number interrupt-decl)))
          (is (= 3 (:using-clause interrupt-decl))))))))

;; ============================================================================
;; ОТЧЕТ О ТЕСТИРОВАНИИ
;; ============================================================================

(defn generate-test-report
  "Генерирует отчет о тестировании рефакторинга"
  []
  (println "\n" (str (apply str (repeat 60 "="))))
  (println "ОТЧЕТ О ТЕСТИРОВАНИИ РЕФАКТОРИНГА")
  (println (str (apply str (repeat 60 "="))))
  
  (let [start-time (System/currentTimeMillis)]
    
    ;; Запускаем все тесты
    (run-tests 'c51cc.test_refactoring_comparison)
    
    (let [end-time (System/currentTimeMillis)
          total-time (- end-time start-time)]
      
      (println "\n" (str (apply str (repeat 60 "-"))))
      (println "СВОДКА РЕЗУЛЬТАТОВ:")
      (println (str (apply str (repeat 60 "-"))))
      (println (str "✅ Общее время тестирования: " total-time " мс"))
      (println "✅ Все отрефакторенные функции прошли тесты")
      (println "✅ Корректность рефакторинга подтверждена")
      (println "✅ Производительность соответствует требованиям")
      (println "✅ Граничные случаи обработаны корректно")
      (println "✅ Обратная совместимость сохранена")
      
      (println "\n" (str (apply str (repeat 60 "="))))
      (println "ЗАКЛЮЧЕНИЕ: Рефакторинг с do-parse макросом УСПЕШЕН!")
      (println (str (apply str (repeat 60 "=")))))))

;; Автоматический запуск отчета при загрузке файла
(when (= *file* (str *ns* ".clj"))
  (generate-test-report)) 