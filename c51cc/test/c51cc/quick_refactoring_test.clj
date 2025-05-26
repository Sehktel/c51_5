(ns c51cc.quick_refactoring_test
  "Быстрые тесты для проверки отрефакторенных функций"
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]
            [clojure.test :refer :all]))

(defn parse-code [code]
  (let [tokens (lexer/tokenize code)]
    (parser/parse tokens)))

(deftest test-basic-expressions
  "Тестирует базовые выражения после рефакторинга"
  (testing "Числовой литерал"
    (let [result (parse-code "42;")]
      (is (:success result))
      (is (= :literal (:ast-type (:expression (first (:declarations (:ast result)))))))))
  
  (testing "Идентификатор"
    (let [result (parse-code "x;")]
      (is (:success result))
      (is (= :identifier (:ast-type (:expression (first (:declarations (:ast result)))))))))
  
  (testing "Выражение в скобках"
    (let [result (parse-code "(42);")]
      (is (:success result))
      (is (= :literal (:ast-type (:expression (first (:declarations (:ast result))))))))))

(deftest test-basic-functions
  "Тестирует базовые функции после рефакторинга"
  (testing "Простая функция"
    (let [result (parse-code "void main() {}")]
      (is (:success result))
      (let [func-decl (first (:declarations (:ast result)))]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= "main" (:name func-decl))))))
  
  (testing "Функция с параметрами"
    (let [result (parse-code "int add(int a, int b) { return a; }")]
      (is (:success result))
      (let [func-decl (first (:declarations (:ast result)))]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= "add" (:name func-decl)))
        (is (= 2 (count (:parameters func-decl)))))))
  
  (testing "Функция с void параметрами"
    (let [result (parse-code "void func(void) {}")]
      (is (:success result))
      (let [func-decl (first (:declarations (:ast result)))]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= [] (:parameters func-decl)))))))

(deftest test-c51-specific
  "Тестирует C51-специфичные конструкции после рефакторинга"
  (testing "Interrupt функция"
    (let [result (parse-code "void timer_isr() interrupt 1 {}")]
      (is (:success result))
      (let [interrupt-decl (first (:declarations (:ast result)))]
        (is (= :interrupt-declaration (:ast-type interrupt-decl)))
        (is (= "timer_isr" (:function-name interrupt-decl)))
        (is (= 1 (:interrupt-number interrupt-decl))))))
  
  (testing "SFR декларация"
    (let [result (parse-code "sfr P0 = 0x80;")]
      (is (:success result))
      (let [sfr-decl (first (:declarations (:ast result)))]
        (is (= :sfr-declaration (:ast-type sfr-decl)))
        (is (= "P0" (:name sfr-decl)))
        (is (= 0x80 (:address sfr-decl))))))
  
  (testing "SBIT декларация"
    (let [result (parse-code "sbit LED = 0x90;")]
      (is (:success result))
      (let [sbit-decl (first (:declarations (:ast result)))]
        (is (= :sbit-declaration (:ast-type sbit-decl)))
        (is (= "LED" (:name sbit-decl)))
        (is (= 0x90 (:address sbit-decl)))))))

(deftest test-complex-program
  "Тестирует сложную программу после рефакторинга"
  (testing "Комплексная C51 программа"
    (let [code "sfr P0 = 0x80;
                void timer_isr() interrupt 1 {
                    int x = 42;
                }
                void main() {
                    return;
                }"
          result (parse-code code)]
      (is (:success result))
      (let [ast (:ast result)
            declarations (:declarations ast)]
        (is (= :program (:ast-type ast)))
        (is (= 3 (count declarations)))
        (is (= :sfr-declaration (:ast-type (nth declarations 0))))
        (is (= :interrupt-declaration (:ast-type (nth declarations 1))))
        (is (= :function-declaration (:ast-type (nth declarations 2))))))))

(defn run-quick-tests []
  (println "=== БЫСТРЫЕ ТЕСТЫ РЕФАКТОРИНГА ===")
  (run-tests 'c51cc.quick_refactoring_test)
  (println "=== ТЕСТЫ ЗАВЕРШЕНЫ ==="))

;; Автоматический запуск при загрузке
(when (= *file* (str *ns* ".clj"))
  (run-quick-tests)) 