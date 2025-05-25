(ns c51cc.parser-test
  "Тесты для функционального парсера C51 с монадическими комбинаторами"
  (:require [clojure.test :refer :all]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ТЕСТИРОВАНИЯ
;; ============================================================================

(defn parse-string 
  "Вспомогательная функция для парсинга строки кода"
  [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn parse-successful? 
  "Проверяет, был ли парсинг успешным"
  [result]
  (:success result))

(defn get-ast 
  "Извлекает AST из результата парсинга"
  [result]
  (:ast result))

(defn get-error 
  "Извлекает ошибку из результата парсинга"
  [result]
  (:error result))

;; ============================================================================
;; ТЕСТЫ БАЗОВЫХ ПАРСЕРОВ
;; ============================================================================

(deftest test-basic-parsing-infrastructure
  (testing "Создание состояния парсера"
    (let [tokens [{:type :number :value 42}]
          state (parser/parse-state tokens)]
      (is (= tokens (:tokens state)))
      (is (= 0 (:position state)))))

  (testing "Успешный результат парсинга"
    (let [state (parser/parse-state [])
          result (parser/success :test-value state)]
      (is (:success? result))
      (is (= :test-value (:value result)))
      (is (= state (:state result)))
      (is (nil? (:error result)))))

  (testing "Неуспешный результат парсинга"
    (let [state (parser/parse-state [])
          result (parser/failure "test error" state)]
      (is (not (:success? result)))
      (is (nil? (:value result)))
      (is (= state (:state result)))
      (is (= "test error" (:error result))))))

;; ============================================================================
;; ТЕСТЫ ПАРСИНГА ЛИТЕРАЛОВ И ИДЕНТИФИКАТОРОВ
;; ============================================================================

(deftest test-primary-expressions
  (testing "Парсинг числового литерала"
    (let [result (parse-string "42;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))
        (let [first-decl (first (:declarations ast))]
          (is (= :expression-statement (:ast-type first-decl)))
          (let [expr (:expression first-decl)]
            (is (= :literal (:ast-type expr)))
            (is (= 42 (:value expr)))
            (is (= :number (:literal-type expr))))))))

  (testing "Парсинг идентификатора"
    (let [result (parse-string "variable_name;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))
        (let [first-decl (first (:declarations ast))]
          (is (= :expression-statement (:ast-type first-decl)))
          (let [expr (:expression first-decl)]
            (is (= :identifier (:ast-type expr)))
            (is (= "variable_name" (:name expr))))))))

  (testing "Парсинг выражения в скобках"
    (let [result (parse-string "(42);")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))
        (let [first-decl (first (:declarations ast))]
          (is (= :expression-statement (:ast-type first-decl)))
          (let [expr (:expression first-decl)]
            (is (= :literal (:ast-type expr)))
            (is (= 42 (:value expr)))))))))

;; ============================================================================
;; ТЕСТЫ УНАРНЫХ ВЫРАЖЕНИЙ
;; ============================================================================

(deftest test-unary-expressions
  (testing "Унарный минус"
    (let [result (parse-string "-42;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :unary-expression (:ast-type expr)))
          (is (= :minus (:operator expr)))
          (let [operand (:operand expr)]
            (is (= :literal (:ast-type operand)))
            (is (= 42 (:value operand))))))))

  (testing "Унарный плюс"
    (let [result (parse-string "+42;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :unary-expression (:ast-type expr)))
          (is (= :plus (:operator expr)))))))

  (testing "Логическое отрицание"
    (let [result (parse-string "!x;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :unary-expression (:ast-type expr)))
          (is (= :not (:operator expr)))
          (let [operand (:operand expr)]
            (is (= :identifier (:ast-type operand)))
            (is (= "x" (:name operand))))))))

  (testing "Префиксный инкремент"
    (let [result (parse-string "++x;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :unary-expression (:ast-type expr)))
          (is (= :increment (:operator expr)))))))

  (testing "Префиксный декремент"
    (let [result (parse-string "--x;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :unary-expression (:ast-type expr)))
          (is (= :decrement (:operator expr))))))))

;; ============================================================================
;; ТЕСТЫ БИНАРНЫХ ВЫРАЖЕНИЙ
;; ============================================================================

(deftest test-binary-expressions
  (testing "Простое сложение"
    (let [result (parse-string "a + b;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :binary-expression (:ast-type expr)))
          (is (= :plus (:operator expr)))
          (is (= "a" (:name (:left expr))))
          (is (= "b" (:name (:right expr))))))))

  (testing "Простое умножение"
    (let [result (parse-string "x * y;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :binary-expression (:ast-type expr)))
          (is (= :multiply (:operator expr)))
          (is (= "x" (:name (:left expr))))
          (is (= "y" (:name (:right expr))))))))

  (testing "Приоритет операторов: умножение перед сложением"
    (let [result (parse-string "a + b * c;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :binary-expression (:ast-type expr)))
          (is (= :plus (:operator expr)))
          (is (= "a" (:name (:left expr))))
          (let [right (:right expr)]
            (is (= :binary-expression (:ast-type right)))
            (is (= :multiply (:operator right)))
            (is (= "b" (:name (:left right))))
            (is (= "c" (:name (:right right)))))))))

  (testing "Выражение со скобками"
    (let [result (parse-string "(a + b) * c;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :binary-expression (:ast-type expr)))
          (is (= :multiply (:operator expr)))
          (let [left (:left expr)]
            (is (= :binary-expression (:ast-type left)))
            (is (= :plus (:operator left)))
            (is (= "a" (:name (:left left))))
            (is (= "b" (:name (:right left)))))
          (is (= "c" (:name (:right expr)))))))))

;; ============================================================================
;; ТЕСТЫ ОПЕРАТОРОВ СРАВНЕНИЯ И ЛОГИЧЕСКИХ ОПЕРАТОРОВ
;; ============================================================================

(deftest test-comparison-and-logical-expressions
  (testing "Оператор равенства"
    (let [result (parse-string "x == y;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :binary-expression (:ast-type expr)))
          (is (= :equal (:operator expr)))))))

  (testing "Оператор неравенства"
    (let [result (parse-string "x != y;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :binary-expression (:ast-type expr)))
          (is (= :not-equal (:operator expr)))))))

  (testing "Оператор меньше"
    (let [result (parse-string "x < y;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :binary-expression (:ast-type expr)))
          (is (= :less (:operator expr)))))))

  (testing "Логическое И"
    (let [result (parse-string "x && y;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :binary-expression (:ast-type expr)))
          (is (= :and (:operator expr)))))))

  (testing "Логическое ИЛИ"
    (let [result (parse-string "x || y;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :binary-expression (:ast-type expr)))
          (is (= :or (:operator expr))))))))

;; ============================================================================
;; ТЕСТЫ ОПЕРАТОРОВ ПРИСВАИВАНИЯ
;; ============================================================================

(deftest test-assignment-expressions
  (testing "Простое присваивание"
    (let [result (parse-string "x = 42;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :assignment-expression (:ast-type expr)))
          (is (= :equal (:operator expr)))
          (is (= "x" (:name (:left expr))))
          (is (= 42 (:value (:right expr))))))))

  (testing "Составное присваивание с И"
    (let [result (parse-string "x &= y;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :assignment-expression (:ast-type expr)))
          (is (= :and-equal (:operator expr))))))))

;; ============================================================================
;; ТЕСТЫ ВЫЗОВОВ ФУНКЦИЙ
;; ============================================================================

(deftest test-function-calls
  (testing "Вызов функции без аргументов"
    (let [result (parse-string "func();")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :call-expression (:ast-type expr)))
          (is (= "func" (:name (:callee expr))))
          (is (empty? (:arguments expr)))))))

  (testing "Вызов функции с одним аргументом"
    (let [result (parse-string "func(42);")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :call-expression (:ast-type expr)))
          (is (= "func" (:name (:callee expr))))
          (is (= 1 (count (:arguments expr))))
          (is (= 42 (:value (first (:arguments expr)))))))))

  (testing "Вызов функции с несколькими аргументами"
    (let [result (parse-string "func(a, b, 42);")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :call-expression (:ast-type expr)))
          (is (= "func" (:name (:callee expr))))
          (is (= 3 (count (:arguments expr)))))))))

;; ============================================================================
;; ТЕСТЫ ПАРСИНГА ТИПОВ
;; ============================================================================

(deftest test-type-parsing
  (testing "Простой тип int"
    (let [tokens (lexer/tokenize "int")
          state (parser/parse-state tokens)
          result (parser/parse-type-specifier state)]
      (is (:success? result))
      (let [type-spec (:value result)]
        (is (= :int (:base-type type-spec)))
        (is (nil? (:signedness type-spec))))))

  (testing "Беззнаковый int"
    (let [tokens (lexer/tokenize "unsigned int")
          state (parser/parse-state tokens)
          result (parser/parse-type-specifier state)]
      (is (:success? result))
      (let [type-spec (:value result)]
        (is (= :int (:base-type type-spec)))
        (is (= :unsigned (:signedness type-spec))))))

  (testing "Знаковый char"
    (let [tokens (lexer/tokenize "signed char")
          state (parser/parse-state tokens)
          result (parser/parse-type-specifier state)]
      (is (:success? result))
      (let [type-spec (:value result)]
        (is (= :char (:base-type type-spec)))
        (is (= :signed (:signedness type-spec))))))

  (testing "Тип void"
    (let [tokens (lexer/tokenize "void")
          state (parser/parse-state tokens)
          result (parser/parse-type-specifier state)]
      (is (:success? result))
      (let [type-spec (:value result)]
        (is (= :void (:base-type type-spec)))
        (is (nil? (:signedness type-spec)))))))

;; ============================================================================
;; ТЕСТЫ ОБЪЯВЛЕНИЙ ПЕРЕМЕННЫХ
;; ============================================================================

(deftest test-variable-declarations
  (testing "Простое объявление переменной"
    (let [result (parse-string "int x;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))
        (let [decl (first (:declarations ast))]
          (is (= :variable-declaration (:ast-type decl)))
          (is (= :int (:base-type (:type decl))))
          (is (= "x" (:name decl)))
          (is (nil? (:initializer decl)))))))

  (testing "Объявление переменной с инициализацией"
    (let [result (parse-string "int x = 42;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [decl (first (:declarations ast))]
          (is (= :variable-declaration (:ast-type decl)))
          (is (= :int (:base-type (:type decl))))
          (is (= "x" (:name decl)))
          (is (= 42 (:value (:initializer decl))))))))

  (testing "Объявление беззнаковой переменной"
    (let [result (parse-string "unsigned char c;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [decl (first (:declarations ast))]
          (is (= :variable-declaration (:ast-type decl)))
          (is (= :char (:base-type (:type decl))))
          (is (= :unsigned (:signedness (:type decl))))
          (is (= "c" (:name decl))))))))

;; ============================================================================
;; ТЕСТЫ ОПЕРАТОРОВ
;; ============================================================================

(deftest test-statements
  (testing "Оператор return без выражения"
    (let [result (parse-string "return;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [stmt (first (:declarations ast))]
          (is (= :return-statement (:ast-type stmt)))
          (is (nil? (:expression stmt)))))))

  (testing "Оператор return с выражением"
    (let [result (parse-string "return 42;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [stmt (first (:declarations ast))]
          (is (= :return-statement (:ast-type stmt)))
          (is (= 42 (:value (:expression stmt))))))))

  (testing "Блок операторов"
    (let [result (parse-string "{ x = 10; return x; }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [block (first (:declarations ast))]
          (is (= :block-statement (:ast-type block)))
          (is (= 2 (count (:statements block))))))))

  (testing "Условный оператор if"
    (let [result (parse-string "if (x > 0) return x;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [if-stmt (first (:declarations ast))]
          (is (= :if-statement (:ast-type if-stmt)))
          (is (= :binary-expression (:ast-type (:condition if-stmt))))
          (is (= :return-statement (:ast-type (:then-branch if-stmt))))
          (is (nil? (:else-branch if-stmt)))))))

  (testing "Условный оператор if-else"
    (let [result (parse-string "if (x > 0) return x; else return 0;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [if-stmt (first (:declarations ast))]
          (is (= :if-statement (:ast-type if-stmt)))
          (is (= :return-statement (:ast-type (:then-branch if-stmt))))
          (is (= :return-statement (:ast-type (:else-branch if-stmt))))))))

  (testing "Цикл while"
    (let [result (parse-string "while (x > 0) x--;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [while-stmt (first (:declarations ast))]
          (is (= :while-statement (:ast-type while-stmt)))
          (is (= :binary-expression (:ast-type (:condition while-stmt))))
          (is (= :expression-statement (:ast-type (:body while-stmt))))))))

  (testing "Цикл for"
    (let [result (parse-string "for (i = 0; i < 10; i++) sum += i;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [for-stmt (first (:declarations ast))]
          (is (= :for-statement (:ast-type for-stmt)))
          (is (= :assignment-expression (:ast-type (:init for-stmt))))
          (is (= :binary-expression (:ast-type (:condition for-stmt))))
          (is (= :unary-expression (:ast-type (:update for-stmt))))
          (is (= :expression-statement (:ast-type (:body for-stmt)))))))))

;; ============================================================================
;; ТЕСТЫ ОБЪЯВЛЕНИЙ ФУНКЦИЙ
;; ============================================================================

(deftest test-function-declarations
  (testing "Простая функция без параметров"
    (let [result (parse-string "void main() { }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [func-decl (first (:declarations ast))]
          (is (= :function-declaration (:ast-type func-decl)))
          (is (= :void (:base-type (:return-type func-decl))))
          (is (= "main" (:name func-decl)))
          (is (empty? (:parameters func-decl)))
          (is (= :block-statement (:ast-type (:body func-decl))))))))

  (testing "Функция с параметрами"
    (let [result (parse-string "int add(int a, int b) { return a + b; }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [func-decl (first (:declarations ast))]
          (is (= :function-declaration (:ast-type func-decl)))
          (is (= :int (:base-type (:return-type func-decl))))
          (is (= "add" (:name func-decl)))
          (is (= 2 (count (:parameters func-decl))))
          (let [param1 (first (:parameters func-decl))
                param2 (second (:parameters func-decl))]
            (is (= :int (:base-type (:type param1))))
            (is (= "a" (:name param1)))
            (is (= :int (:base-type (:type param2))))
            (is (= "b" (:name param2))))))))

  (testing "Функция с комплексным телом"
    (let [result (parse-string "int factorial(int n) { if (n <= 1) return 1; else return n * factorial(n - 1); }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [func-decl (first (:declarations ast))]
          (is (= :function-declaration (:ast-type func-decl)))
          (is (= "factorial" (:name func-decl)))
          (is (= 1 (count (:parameters func-decl))))
          (let [body (:body func-decl)]
            (is (= :block-statement (:ast-type body)))
            (is (= 1 (count (:statements body))))
            (is (= :if-statement (:ast-type (first (:statements body)))))))))))

;; ============================================================================
;; ТЕСТЫ КОМПЛЕКСНЫХ ПРОГРАММ
;; ============================================================================

(deftest test-complete-programs
  (testing "Простая программа с несколькими объявлениями"
    (let [result (parse-string "int x = 10; void main() { x = x + 1; }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))
        (is (= 2 (count (:declarations ast))))
        (is (= :variable-declaration (:ast-type (first (:declarations ast)))))
        (is (= :function-declaration (:ast-type (second (:declarations ast))))))))

  (testing "Программа с вложенными блоками"
    (let [result (parse-string "void test() { int x; int y; x = 1; { y = 2; x = x + y; } }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [func-decl (first (:declarations ast))
              body (:body func-decl)]
          (is (= 4 (count (:statements body))))
          (is (= :variable-declaration (:ast-type (first (:statements body)))))
          (is (= :variable-declaration (:ast-type (second (:statements body)))))
          (is (= :expression-statement (:ast-type (nth (:statements body) 2))))
          (is (= :block-statement (:ast-type (nth (:statements body) 3))))))))

  (testing "Программа с циклами и условиями"
    (let [result (parse-string "int sum(int n) { int result; int i; result = 0; for (i = 1; i <= n; i++) { if (i % 2 == 0) result += i; } return result; }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [func-decl (first (:declarations ast))
              body (:body func-decl)]
          (is (= 5 (count (:statements body))))
          (is (= :variable-declaration (:ast-type (first (:statements body)))))
          (is (= :variable-declaration (:ast-type (second (:statements body)))))
          (is (= :expression-statement (:ast-type (nth (:statements body) 2))))
          (is (= :for-statement (:ast-type (nth (:statements body) 3))))
          (is (= :return-statement (:ast-type (nth (:statements body) 4)))))))))

;; ============================================================================
;; ТЕСТЫ ОБРАБОТКИ ОШИБОК
;; ============================================================================

(deftest test-error-handling
  (testing "Синтаксическая ошибка - отсутствующая скобка"
    (let [result (parse-string "func(")]
      (is (not (parse-successful? result)))
      (is (string? (get-error result)))))

  (testing "Синтаксическая ошибка - неожиданный токен"
    (let [result (parse-string "int + 42")]
      (is (not (parse-successful? result)))
      (is (string? (get-error result)))))

  (testing "Синтаксическая ошибка - отсутствующая точка с запятой"
    (let [result (parse-string "int x = 42")]
      (is (not (parse-successful? result)))
      (is (string? (get-error result)))))

  (testing "Пустой ввод"
    (let [result (parse-string "")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))
        (is (empty? (:declarations ast)))))))

;; ============================================================================
;; ТЕСТЫ ПРОИЗВОДИТЕЛЬНОСТИ И ГРАНИЧНЫХ СЛУЧАЕВ
;; ============================================================================

(deftest test-edge-cases
  (testing "Глубоко вложенные выражения"
    (let [result (parse-string "((((42))));")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :literal (:ast-type expr)))
          (is (= 42 (:value expr)))))))

  (testing "Длинная цепочка операций"
    (let [result (parse-string "a + b + c + d + e;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :binary-expression (:ast-type expr)))
          (is (= :plus (:operator expr)))))))

  (testing "Комплексное выражение с разными приоритетами"
    (let [result (parse-string "a + b * c - d / e;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (let [expr (:expression (first (:declarations ast)))]
          (is (= :binary-expression (:ast-type expr)))
          (is (= :minus (:operator expr)))))))) 