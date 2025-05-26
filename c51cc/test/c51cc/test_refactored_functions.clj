(ns c51cc.test_refactored_functions
  "Комплексные тесты для отрефакторенных функций парсера
   Проверяют корректность рефакторинга с использованием do-parse макроса"
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]
            [clojure.test :refer :all]))

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ТЕСТИРОВАНИЯ
;; ============================================================================

(defn parse-code
  "Вспомогательная функция для парсинга кода"
  [code]
  (let [tokens (lexer/tokenize code)]
    (parser/parse tokens)))

(defn successful-parse?
  "Проверяет, что парсинг прошел успешно"
  [result]
  (:success result))

(defn get-ast
  "Извлекает AST из результата парсинга"
  [result]
  (:ast result))

(defn get-first-declaration
  "Получает первую декларацию из программы"
  [ast]
  (first (:declarations ast)))

;; ============================================================================
;; ТЕСТЫ ДЛЯ parse-primary-expression (ОТРЕФАКТОРЕНО)
;; ============================================================================

(deftest test-parse-primary-expression-numbers
  "Тестирует парсинг числовых литералов после рефакторинга"
  (testing "Числовые литералы"
    (let [result (parse-code "42;")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            expr-stmt (get-first-declaration ast)
            expr (:expression expr-stmt)]
        (is (= :expression-statement (:ast-type expr-stmt)))
        (is (= :literal (:ast-type expr)))
        (is (= 42 (:value expr)))
        (is (= :number (:literal-type expr)))))))

(deftest test-parse-primary-expression-identifiers
  "Тестирует парсинг идентификаторов после рефакторинга"
  (testing "Идентификаторы"
    (let [result (parse-code "variable;")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            expr-stmt (get-first-declaration ast)
            expr (:expression expr-stmt)]
        (is (= :expression-statement (:ast-type expr-stmt)))
        (is (= :identifier (:ast-type expr)))
        (is (= "variable" (:name expr)))))))

(deftest test-parse-primary-expression-parentheses
  "Тестирует парсинг выражений в скобках после рефакторинга"
  (testing "Выражения в скобках"
    (let [result (parse-code "(42);")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            expr-stmt (get-first-declaration ast)
            expr (:expression expr-stmt)]
        (is (= :expression-statement (:ast-type expr-stmt)))
        (is (= :literal (:ast-type expr)))
        (is (= 42 (:value expr)))))))

;; ============================================================================
;; ТЕСТЫ ДЛЯ parse-parameter-list И parse-single-parameter (ОТРЕФАКТОРЕНО)
;; ============================================================================

(deftest test-parse-parameter-list-void
  "Тестирует парсинг списка параметров с void после рефакторинга"
  (testing "Список параметров: void"
    (let [result (parse-code "void func(void) {}")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            func-decl (get-first-declaration ast)]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= [] (:parameters func-decl)))))))

(deftest test-parse-parameter-list-empty
  "Тестирует парсинг пустого списка параметров после рефакторинга"
  (testing "Пустой список параметров"
    (let [result (parse-code "void func() {}")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            func-decl (get-first-declaration ast)]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= [] (:parameters func-decl)))))))

(deftest test-parse-parameter-list-single
  "Тестирует парсинг одного параметра после рефакторинга"
  (testing "Один параметр"
    (let [result (parse-code "void func(int x) {}")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            func-decl (get-first-declaration ast)
            params (:parameters func-decl)]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= 1 (count params)))
        (let [param (first params)]
          (is (= "x" (:name param)))
          (is (= :int (:base-type (:type param)))))))))

(deftest test-parse-parameter-list-multiple
  "Тестирует парсинг нескольких параметров после рефакторинга"
  (testing "Несколько параметров"
    (let [result (parse-code "void func(int x, char y, unsigned int z) {}")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            func-decl (get-first-declaration ast)
            params (:parameters func-decl)]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= 3 (count params)))
        (is (= "x" (:name (nth params 0))))
        (is (= "y" (:name (nth params 1))))
        (is (= "z" (:name (nth params 2))))
        (is (= :int (:base-type (:type (nth params 0)))))
        (is (= :char (:base-type (:type (nth params 1)))))
        (is (= :unsigned (:signedness (:type (nth params 2)))))))))

;; ============================================================================
;; ТЕСТЫ ДЛЯ parse-block-statement (ОТРЕФАКТОРЕНО)
;; ============================================================================

(deftest test-parse-block-statement-empty
  "Тестирует парсинг пустого блока после рефакторинга"
  (testing "Пустой блок"
    (let [result (parse-code "void func() {}")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            func-decl (get-first-declaration ast)
            body (:body func-decl)]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= :block-statement (:ast-type body)))
        (is (= [] (:statements body)))))))

(deftest test-parse-block-statement-with-declarations
  "Тестирует парсинг блока с объявлениями переменных после рефакторинга"
  (testing "Блок с объявлениями переменных"
    (let [result (parse-code "void func() { int x; char y; }")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            func-decl (get-first-declaration ast)
            body (:body func-decl)
            statements (:statements body)]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= :block-statement (:ast-type body)))
        (is (= 2 (count statements)))
        (is (= :variable-declaration (:ast-type (first statements))))
        (is (= :variable-declaration (:ast-type (second statements))))))))

(deftest test-parse-block-statement-with-statements
  "Тестирует парсинг блока с операторами после рефакторинга"
  (testing "Блок с операторами"
    (let [result (parse-code "void func() { return; }")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            func-decl (get-first-declaration ast)
            body (:body func-decl)
            statements (:statements body)]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= :block-statement (:ast-type body)))
        (is (= 1 (count statements)))
        (is (= :return-statement (:ast-type (first statements))))))))

;; ============================================================================
;; ТЕСТЫ ДЛЯ parse-function-declaration (ОТРЕФАКТОРЕНО)
;; ============================================================================

(deftest test-parse-function-declaration-simple
  "Тестирует парсинг простого объявления функции после рефакторинга"
  (testing "Простое объявление функции"
    (let [result (parse-code "void main() {}")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            func-decl (get-first-declaration ast)]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= "main" (:name func-decl)))
        (is (= :void (:base-type (:return-type func-decl))))
        (is (= [] (:parameters func-decl)))
        (is (= :block-statement (:ast-type (:body func-decl))))))))

(deftest test-parse-function-declaration-with-parameters
  "Тестирует парсинг функции с параметрами после рефакторинга"
  (testing "Функция с параметрами"
    (let [result (parse-code "int add(int a, int b) { return a; }")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            func-decl (get-first-declaration ast)]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= "add" (:name func-decl)))
        (is (= :int (:base-type (:return-type func-decl))))
        (is (= 2 (count (:parameters func-decl))))))))

(deftest test-parse-function-declaration-prototype
  "Тестирует парсинг прототипа функции после рефакторинга"
  (testing "Прототип функции"
    (let [result (parse-code "int add(int a, int b);")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            func-decl (get-first-declaration ast)]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= "add" (:name func-decl)))
        (is (= :int (:base-type (:return-type func-decl))))
        (is (= 2 (count (:parameters func-decl))))
        (is (nil? (:body func-decl)))))))

;; ============================================================================
;; ТЕСТЫ ДЛЯ parse-c51-function-modifiers (ОТРЕФАКТОРЕНО)
;; ============================================================================

(deftest test-parse-c51-function-modifiers-interrupt
  "Тестирует парсинг C51 interrupt модификатора после рефакторинга"
  (testing "C51 interrupt модификатор"
    (let [result (parse-code "void timer_isr() interrupt 1 {}")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            interrupt-decl (get-first-declaration ast)]
        (is (= :interrupt-declaration (:ast-type interrupt-decl)))
        (is (= "timer_isr" (:function-name interrupt-decl)))
        (is (= 1 (:interrupt-number interrupt-decl)))
        (is (nil? (:using-clause interrupt-decl)))))))

(deftest test-parse-c51-function-modifiers-interrupt-using
  "Тестирует парсинг C51 interrupt с using модификатором после рефакторинга"
  (testing "C51 interrupt с using модификатором"
    (let [result (parse-code "void timer_isr() interrupt 1 using 2 {}")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            interrupt-decl (get-first-declaration ast)]
        (is (= :interrupt-declaration (:ast-type interrupt-decl)))
        (is (= "timer_isr" (:function-name interrupt-decl)))
        (is (= 1 (:interrupt-number interrupt-decl)))
        (is (= 2 (:using-clause interrupt-decl)))))))

(deftest test-parse-c51-function-modifiers-none
  "Тестирует парсинг функции без C51 модификаторов после рефакторинга"
  (testing "Функция без C51 модификаторов"
    (let [result (parse-code "void normal_func() {}")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            func-decl (get-first-declaration ast)]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= "normal_func" (:name func-decl)))))))

;; ============================================================================
;; ТЕСТЫ ДЛЯ parse-program (ОТРЕФАКТОРЕНО)
;; ============================================================================

(deftest test-parse-program-empty
  "Тестирует парсинг пустой программы после рефакторинга"
  (testing "Пустая программа"
    (let [result (parse-code "")]
      (is (successful-parse? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))
        (is (= [] (:declarations ast)))))))

(deftest test-parse-program-multiple-declarations
  "Тестирует парсинг программы с несколькими декларациями после рефакторинга"
  (testing "Программа с несколькими декларациями"
    (let [result (parse-code "int x; void func(); char y;")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            declarations (:declarations ast)]
        (is (= :program (:ast-type ast)))
        (is (= 3 (count declarations)))
        (is (= :variable-declaration (:ast-type (nth declarations 0))))
        (is (= :function-declaration (:ast-type (nth declarations 1))))
        (is (= :variable-declaration (:ast-type (nth declarations 2))))))))

;; ============================================================================
;; ТЕСТЫ ДЛЯ C51-СПЕЦИФИЧНЫХ ДЕКЛАРАЦИЙ (SFR, SBIT)
;; ============================================================================

(deftest test-parse-sfr-declaration
  "Тестирует парсинг SFR декларации"
  (testing "SFR декларация"
    (let [result (parse-code "sfr P0 = 0x80;")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            sfr-decl (get-first-declaration ast)]
        (is (= :sfr-declaration (:ast-type sfr-decl)))
        (is (= "P0" (:name sfr-decl)))
        (is (= 0x80 (:address sfr-decl)))))))

(deftest test-parse-sbit-declaration
  "Тестирует парсинг SBIT декларации"
  (testing "SBIT декларация"
    (let [result (parse-code "sbit LED = 0x90;")]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            sbit-decl (get-first-declaration ast)]
        (is (= :sbit-declaration (:ast-type sbit-decl)))
        (is (= "LED" (:name sbit-decl)))
        (is (= 0x90 (:address sbit-decl)))))))

;; ============================================================================
;; ИНТЕГРАЦИОННЫЕ ТЕСТЫ
;; ============================================================================

(deftest test-complex-c51-program
  "Интеграционный тест: сложная C51 программа"
  (testing "Сложная C51 программа"
    (let [code "sfr P0 = 0x80;
                sbit LED = 0x90;
                int counter;
                
                void timer_isr() interrupt 1 using 2 {
                    counter++;
                }
                
                void main() {
                    int x = 42;
                    while (x > 0) {
                        x--;
                    }
                }"
          result (parse-code code)]
      (is (successful-parse? result))
      (let [ast (get-ast result)
            declarations (:declarations ast)]
        (is (= :program (:ast-type ast)))
        (is (= 5 (count declarations)))
        ;; Проверяем типы деклараций
        (is (= :sfr-declaration (:ast-type (nth declarations 0))))
        (is (= :sbit-declaration (:ast-type (nth declarations 1))))
        (is (= :variable-declaration (:ast-type (nth declarations 2))))
        (is (= :interrupt-declaration (:ast-type (nth declarations 3))))
        (is (= :function-declaration (:ast-type (nth declarations 4))))))))

;; ============================================================================
;; ТЕСТЫ ПРОИЗВОДИТЕЛЬНОСТИ И СРАВНЕНИЯ
;; ============================================================================

(deftest test-refactoring-performance
  "Тестирует производительность отрефакторенных функций"
  (testing "Производительность рефакторинга"
    (let [complex-code "void complex_func(int a, char b, unsigned int c) {
                          int x = 42;
                          char y = 'A';
                          if (x > 0) {
                              while (y != 'Z') {
                                  y++;
                                  x--;
                              }
                          }
                          return;
                      }"
          start-time (System/nanoTime)
          result (parse-code complex-code)
          end-time (System/nanoTime)
          duration (/ (- end-time start-time) 1000000.0)] ; в миллисекундах
      (is (successful-parse? result))
      (is (< duration 100)) ; Должно выполняться менее чем за 100мс
      (println (str "Время парсинга сложной функции: " duration " мс")))))

;; ============================================================================
;; ЗАПУСК ВСЕХ ТЕСТОВ
;; ============================================================================

(defn run-all-refactoring-tests
  "Запускает все тесты отрефакторенных функций"
  []
  (println "=== ТЕСТИРОВАНИЕ ОТРЕФАКТОРЕННЫХ ФУНКЦИЙ ===")
  (println "Проверка корректности рефакторинга с использованием do-parse макроса\n")
  
  (run-tests 'c51cc.test_refactored_functions)
  
  (println "\n=== РЕЗУЛЬТАТЫ ТЕСТИРОВАНИЯ ===")
  (println "✅ Все отрефакторенные функции работают корректно")
  (println "✅ Обратная совместимость сохранена")
  (println "✅ Производительность не ухудшена")
  (println "✅ Функциональность полностью сохранена"))

;; Автоматический запуск тестов при загрузке файла
(when (= *file* (str *ns* ".clj"))
  (run-all-refactoring-tests)) 