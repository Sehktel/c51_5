(ns c51cc.parser-test
  "Тесты для функционального парсера C51"
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]
            [clojure.test :refer :all]))

(deftest test-simple-expression
  "Тест парсинга простого выражения"
  (testing "Парсинг числового литерала"
    (let [tokens (lexer/tokenize "42")
          result (parser/parse tokens)]
      (is (:success result))
      (is (= :program (get-in result [:ast :ast-type])))))

  (testing "Парсинг идентификатора"
    (let [tokens (lexer/tokenize "x")
          result (parser/parse tokens)]
      (is (:success result))))

  (testing "Парсинг арифметического выражения"
    (let [tokens (lexer/tokenize "2 + 3")
          result (parser/parse tokens)]
      (is (:success result)))))

(deftest test-variable-declaration
  "Тест парсинга объявления переменной"
  (testing "Простое объявление переменной"
    (let [tokens (lexer/tokenize "int x;")
          result (parser/parse tokens)]
      (is (:success result))
      (when (:success result)
        (let [ast (:ast result)
              first-decl (first (:declarations ast))]
          (is (= :variable-declaration (:ast-type first-decl)))
          (is (= "x" (:name first-decl)))))))

  (testing "Объявление с инициализацией"
    (let [tokens (lexer/tokenize "int x = 42;")
          result (parser/parse tokens)]
      (is (:success result))
      (when (:success result)
        (let [ast (:ast result)
              first-decl (first (:declarations ast))]
          (is (= :variable-declaration (:ast-type first-decl)))
          (is (= "x" (:name first-decl)))
          (is (not (nil? (:initializer first-decl)))))))))

(deftest test-function-declaration
  "Тест парсинга объявления функции"
  (testing "Простая функция без параметров"
    (let [tokens (lexer/tokenize "void main() { return; }")
          result (parser/parse tokens)]
      (is (:success result))
      (when (:success result)
        (let [ast (:ast result)
              first-decl (first (:declarations ast))]
          (is (= :function-declaration (:ast-type first-decl)))
          (is (= "main" (:name first-decl)))
          (is (= :void (get-in first-decl [:return-type :base-type])))))))

  (testing "Функция с параметрами"
    (let [tokens (lexer/tokenize "int add(int a, int b) { return a + b; }")
          result (parser/parse tokens)]
      (is (:success result))
      (when (:success result)
        (let [ast (:ast result)
              first-decl (first (:declarations ast))]
          (is (= :function-declaration (:ast-type first-decl)))
          (is (= "add" (:name first-decl)))
          (is (= 2 (count (:parameters first-decl)))))))))

(deftest test-control-structures
  "Тест парсинга управляющих конструкций"
  (testing "If statement"
    (let [tokens (lexer/tokenize "void test() { if (x > 0) { return; } }")
          result (parser/parse tokens)]
      (is (:success result))))

  (testing "While loop"
    (let [tokens (lexer/tokenize "void test() { while (x > 0) { x = x - 1; } }")
          result (parser/parse tokens)]
      (is (:success result))))

  (testing "For loop"
    (let [tokens (lexer/tokenize "void test() { for (i = 0; i < 10; i = i + 1) { x = x + 1; } }")
          result (parser/parse tokens)]
      (is (:success result)))))

(deftest test-ast-validation
  "Тест валидации AST"
  (testing "Валидация корректного AST"
    (let [tokens (lexer/tokenize "int main() { return 0; }")
          result (parser/parse tokens)]
      (when (:success result)
        (is (parser/validate-ast (:ast result)))))))

(defn run-parser-tests []
  "Запуск всех тестов парсера"
  (run-tests 'c51cc.parser-test))

;; Пример использования парсера
(defn demo-parser []
  "Демонстрация работы парсера"
  (println "=== Демонстрация парсера C51 ===")
  
  ;; Простая программа
  (let [code "int main() { 
                int x = 42;
                if (x > 0) {
                  return x;
                } else {
                  return 0;
                }
              }"
        tokens (lexer/tokenize code)
        result (parser/parse tokens)]
    
    (println "\nИсходный код:")
    (println code)
    
    (println "\nТокены:")
    (doseq [token tokens]
      (println "  " token))
    
    (println "\nРезультат парсинга:")
    (if (:success result)
      (do
        (println "✓ Парсинг успешен")
        (println "\nAST:")
        (parser/pretty-print-ast (:ast result))
        (println "\nВалидация AST:" (parser/validate-ast (:ast result))))
      (do
        (println "✗ Ошибка парсинга:")
        (println "  " (:error result))
        (println "  Позиция:" (:position result))))))

;; Экспорт функций для тестирования
(def ^:export run-parser-tests run-parser-tests)
(def ^:export demo-parser demo-parser) 