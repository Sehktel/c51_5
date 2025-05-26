(ns c51cc.parser-refactoring-comprehensive-test
  "Комплексные тесты для подтверждения корректности рефакторинга функций парсинга выражений
   Тестирует гигиенический макрос do-parse и универсальные функции парсинга"
  (:require [clojure.test :refer :all]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ТЕСТИРОВАНИЯ
;; ============================================================================

(defn tokenize-and-parse
  "Токенизирует строку и применяет парсер"
  [input parser-fn]
  (let [tokens (lexer/tokenize input)]
    (if (and tokens (vector? tokens) (not-empty tokens))
      (parser-fn (parser/parse-state tokens))
      {:success? false :error (str "Не удалось токенизировать: " input)})))

(defn test-expression-parsing
  "Универсальная функция для тестирования парсинга выражений"
  [input parser-fn expected-ast-type]
  (let [result (tokenize-and-parse input parser-fn)]
    (is (:success? result) (str "Парсинг должен быть успешным для: " input))
    (when (:success? result)
      (is (= (:ast-type (:value result)) expected-ast-type)
          (str "AST должен иметь тип " expected-ast-type " для: " input)))))

;; ============================================================================
;; ТЕСТЫ ГИГИЕНИЧЕСКОГО МАКРОСА DO-PARSE
;; ============================================================================

(deftest test-do-parse-hygiene
  "Тестирует гигиеничность макроса do-parse"
  (testing "Макрос do-parse не захватывает переменные"
    ;; Проверяем, что макрос корректно обрабатывает символ _
    (let [test-parser (parser/do-parse
                       _ (parser/expect-token-value :open-round)
                       expr parser/parse-expression
                       _ (parser/expect-token-value :close-round)
                       (parser/return-parser expr))]
      (is (fn? test-parser) "do-parse должен возвращать функцию-парсер")))
  
  (testing "Запрещенные имена переменных"
    ;; Проверяем, что макрос защищает от захвата переменных
    ;; Примечание: тест может не работать в runtime, но макрос защищает на этапе компиляции
    (is (try 
          (eval '(c51cc.parser/do-parse
                  state c51cc.parser/parse-expression
                  (c51cc.parser/return-parser state)))
          false  ; если не выбросило исключение, тест провален
          (catch Exception e true))  ; если выбросило исключение, тест прошел
        "Макрос должен запрещать использование 'state' как имени переменной")
    
    (is (try 
          (eval '(c51cc.parser/do-parse
                  result c51cc.parser/parse-expression
                  (c51cc.parser/return-parser result)))
          false  ; если не выбросило исключение, тест провален
          (catch Exception e true))  ; если выбросило исключение, тест прошел
        "Макрос должен запрещать использование 'result' как имени переменной")))

;; ============================================================================
;; ТЕСТЫ УНИВЕРСАЛЬНОЙ ФУНКЦИИ PARSE-BINARY-EXPRESSION-WITH-OPERATORS
;; ============================================================================

(deftest test-universal-binary-parser
  "Тестирует универсальную функцию для парсинга бинарных выражений"
  (testing "Создание кастомного парсера"
    (let [custom-parser (parser/parse-binary-expression-with-operators 
                         parser/parse-primary-expression
                         parser/additive-operator?)
          result (tokenize-and-parse "a + b" custom-parser)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :plus))))))
  
  (testing "Левая ассоциативность"
    (let [custom-parser (parser/parse-binary-expression-with-operators 
                         parser/parse-primary-expression
                         parser/multiplicative-operator?)
          result (tokenize-and-parse "a * b * c" custom-parser)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Проверяем структуру ((a * b) * c)
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :multiply))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :multiply)))))))

;; ============================================================================
;; ТЕСТЫ ПРЕДИКАТОВ ОПЕРАТОРОВ
;; ============================================================================

(deftest test-operator-predicates
  "Тестирует корректность предикатов для различных типов операторов"
  (testing "Мультипликативные операторы"
    (is (parser/multiplicative-operator? {:type :math-operator :value :multiply}))
    (is (parser/multiplicative-operator? {:type :math-operator :value :divide}))
    (is (parser/multiplicative-operator? {:type :math-operator :value :modulo}))
    (is (not (parser/multiplicative-operator? {:type :math-operator :value :plus}))))
  
  (testing "Операторы сдвига"
    (is (parser/shift-operator? {:type :bitwise-operator :value :shift-left}))
    (is (parser/shift-operator? {:type :bitwise-operator :value :shift-right}))
    (is (not (parser/shift-operator? {:type :math-operator :value :plus}))))
  
  (testing "Аддитивные операторы"
    (is (parser/additive-operator? {:type :math-operator :value :plus}))
    (is (parser/additive-operator? {:type :math-operator :value :minus}))
    (is (not (parser/additive-operator? {:type :math-operator :value :multiply}))))
  
  (testing "Операторы сравнения"
    (is (parser/relational-operator? {:type :comparison-operator :value :less}))
    (is (parser/relational-operator? {:type :comparison-operator :value :greater}))
    (is (parser/relational-operator? {:type :comparison-operator :value :less-equal}))
    (is (parser/relational-operator? {:type :comparison-operator :value :greater-equal}))
    (is (not (parser/relational-operator? {:type :comparison-operator :value :not-equal}))))
  
  (testing "Операторы равенства"
    (is (parser/equality-operator? {:type :logical-operator :value :equal}))
    (is (parser/equality-operator? {:type :comparison-operator :value :not-equal}))
    (is (not (parser/equality-operator? {:type :comparison-operator :value :less}))))
  
  (testing "Логические операторы"
    (is (parser/logical-and-operator? {:type :logical-operator :value :and}))
    (is (parser/logical-or-operator? {:type :logical-operator :value :or}))
    (is (not (parser/logical-and-operator? {:type :logical-operator :value :or})))))

;; ============================================================================
;; ТЕСТЫ РЕФАКТОРЕННЫХ ФУНКЦИЙ ПАРСИНГА
;; ============================================================================

(deftest test-refactored-shift-expression
  "Тестирует рефакторенную функцию parse-shift-expression"
  (testing "Базовые операции сдвига"
    (test-expression-parsing "a << 2" parser/parse-shift-expression :binary-expression)
    (test-expression-parsing "x >> 1" parser/parse-shift-expression :binary-expression))
  
  (testing "Приоритет операторов"
    (let [result (tokenize-and-parse "a * b << c" parser/parse-shift-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a * b) << c
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :shift-left))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :multiply))))))
  
  (testing "Левая ассоциативность"
    (let [result (tokenize-and-parse "a << b >> c" parser/parse-shift-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a << b) >> c
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :shift-right))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :shift-left)))))))

(deftest test-refactored-additive-expression
  "Тестирует рефакторенную функцию parse-additive-expression"
  (testing "Базовые аддитивные операции"
    (test-expression-parsing "a + b" parser/parse-additive-expression :binary-expression)
    (test-expression-parsing "x - y" parser/parse-additive-expression :binary-expression))
  
  (testing "Приоритет операторов"
    (let [result (tokenize-and-parse "a << b + c" parser/parse-additive-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a << b) + c
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :plus))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :shift-left))))))
  
  (testing "Левая ассоциативность"
    (let [result (tokenize-and-parse "a + b - c + d" parser/parse-additive-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Проверяем, что операции группируются слева направо
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :plus)))))))

(deftest test-refactored-relational-expression
  "Тестирует рефакторенную функцию parse-relational-expression"
  (testing "Базовые операции сравнения"
    (test-expression-parsing "a < b" parser/parse-relational-expression :binary-expression)
    (test-expression-parsing "x > y" parser/parse-relational-expression :binary-expression)
    (test-expression-parsing "a <= b" parser/parse-relational-expression :binary-expression)
    (test-expression-parsing "x >= y" parser/parse-relational-expression :binary-expression))
  
  (testing "Приоритет операторов"
    (let [result (tokenize-and-parse "a + b < c" parser/parse-relational-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a + b) < c
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :less))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :plus)))))))

(deftest test-refactored-equality-expression
  "Тестирует рефакторенную функцию parse-equality-expression"
  (testing "Базовые операции равенства"
    (test-expression-parsing "a == b" parser/parse-equality-expression :binary-expression)
    (test-expression-parsing "x != y" parser/parse-equality-expression :binary-expression))
  
  (testing "Приоритет операторов"
    (let [result (tokenize-and-parse "a < b == c" parser/parse-equality-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a < b) == c
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :equal))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :less)))))))

(deftest test-refactored-logical-and-expression
  "Тестирует рефакторенную функцию parse-logical-and-expression"
  (testing "Базовые операции логического И"
    (test-expression-parsing "a && b" parser/parse-logical-and-expression :binary-expression))
  
  (testing "Приоритет операторов"
    (let [result (tokenize-and-parse "a == b && c" parser/parse-logical-and-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a == b) && c
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :and))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :equal))))))
  
  (testing "Левая ассоциативность"
    (let [result (tokenize-and-parse "a && b && c" parser/parse-logical-and-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a && b) && c
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :and))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :and)))))))

(deftest test-refactored-assignment-expression
  "Тестирует рефакторенную функцию parse-assignment-expression"
  (testing "Базовые операции присваивания"
    (test-expression-parsing "a = b" parser/parse-assignment-expression :assignment-expression)
    (test-expression-parsing "x += y" parser/parse-assignment-expression :assignment-expression)
    (test-expression-parsing "z -= w" parser/parse-assignment-expression :assignment-expression))
  
  (testing "Правая ассоциативность присваивания"
    (let [result (tokenize-and-parse "a = b = c" parser/parse-assignment-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть a = (b = c)
          (is (= (:ast-type ast) :assignment-expression))
          (is (= (:operator ast) :equal))
          (is (= (:ast-type (:right ast)) :assignment-expression))
          (is (= (:operator (:right ast)) :equal))))))
  
  (testing "Приоритет операторов присваивания"
    (let [result (tokenize-and-parse "a && b = c" parser/parse-assignment-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a && b) = c
          (is (= (:ast-type ast) :assignment-expression))
          (is (= (:operator ast) :equal))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :and))))))
  
  (testing "Предикат assignment-operator?"
    (is (parser/assignment-operator? {:type :assignment-operator :value :equal}))
    (is (parser/assignment-operator? {:type :assignment-operator :value :plus-equal}))
    (is (parser/assignment-operator? {:type :assignment-operator :value :minus-equal}))
    (is (not (parser/assignment-operator? {:type :math-operator :value :plus})))
    (is (not (parser/assignment-operator? {:type :logical-operator :value :and})))))

;; ============================================================================
;; ТЕСТЫ КОМПЛЕКСНЫХ ВЫРАЖЕНИЙ
;; ============================================================================

(deftest test-complex-expression-precedence
  "Тестирует правильность приоритета в комплексных выражениях"
  (testing "Полная цепочка приоритетов"
    (let [result (tokenize-and-parse "a + b * c << d < e == f && g" 
                                    parser/parse-logical-and-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Проверяем, что верхний уровень - это логическое И
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :and))
          ;; Левая часть должна быть выражением равенства
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :equal))))))
  
  (testing "Сложное выражение с группировкой"
    (let [result (tokenize-and-parse "(a + b) * (c - d)" parser/parse-logical-and-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :multiply))
          ;; Обе части должны быть выражениями в скобках
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :plus))
          (is (= (:ast-type (:right ast)) :binary-expression))
          (is (= (:operator (:right ast)) :minus)))))))

;; ============================================================================
;; ТЕСТЫ ПРОИЗВОДИТЕЛЬНОСТИ И ЧИТАЕМОСТИ
;; ============================================================================

(deftest test-refactoring-benefits
  "Тестирует преимущества рефакторинга"
  (testing "Модульность и переиспользование кода"
    ;; Проверяем, что можно создавать новые парсеры на основе универсальной функции
    (let [custom-additive-parser (parser/parse-binary-expression-with-operators 
                                  parser/parse-primary-expression
                                  parser/additive-operator?)
          result (tokenize-and-parse "x + y - z" custom-additive-parser)]
      (is (:success? result))
      (when (:success? result)
        (is (= (:ast-type (:value result)) :binary-expression)))))
  
  (testing "Консистентность поведения"
    ;; Все рефакторенные функции должны вести себя одинаково
    (let [test-cases [["a * b" parser/parse-multiplicative-expression]
                      ["a << b" parser/parse-shift-expression]
                      ["a + b" parser/parse-additive-expression]
                      ["a < b" parser/parse-relational-expression]
                      ["a == b" parser/parse-equality-expression]
                      ["a && b" parser/parse-logical-and-expression]]]
      (doseq [[input parser-fn] test-cases]
        (let [result (tokenize-and-parse input parser-fn)]
          (is (:success? result) (str "Должен успешно парсить: " input))
          (when (:success? result)
            (is (= (:ast-type (:value result)) :binary-expression)
                (str "Должен создавать binary-expression для: " input))))))))

;; ============================================================================
;; ТЕСТЫ ОБРАТНОЙ СОВМЕСТИМОСТИ
;; ============================================================================

(deftest test-backward-compatibility
  "Тестирует, что рефакторинг не нарушил существующую функциональность"
  (testing "Совместимость с существующими тестами"
    ;; Проверяем, что все основные случаи по-прежнему работают
    (let [test-expressions ["a + b * c"
                           "x << y + z"
                           "a < b && c == d"
                           "(x + y) * z"
                           "a++ + ++b"]]
      (doseq [expr test-expressions]
        (let [result (tokenize-and-parse (str expr ";") parser/parse-expression)]
          (is (:success? result) (str "Выражение должно парситься: " expr))))))
  
  (testing "Корректность AST структуры"
    ;; Проверяем, что структура AST не изменилась
    (let [result (tokenize-and-parse "a + b * c;" parser/parse-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :plus))
          (is (= (:ast-type (:right ast)) :binary-expression))
          (is (= (:operator (:right ast)) :multiply)))))))

;; ============================================================================
;; ТЕСТЫ ОБРАБОТКИ ОШИБОК
;; ============================================================================

(deftest test-error-handling
  "Тестирует корректность обработки ошибок в рефакторенных функциях"
  (testing "Некорректные выражения"
    (let [invalid-expressions ["a +"
                              "* b"
                              "a << << b"
                              "a && && b"]]
      (doseq [expr invalid-expressions]
        (let [result (tokenize-and-parse expr parser/parse-logical-and-expression)]
          ;; Некоторые могут частично парситься, но не должны вызывать исключения
          (is (map? result) (str "Должен возвращать результат для: " expr))))))
  
  (testing "Пустой ввод"
    (let [result (tokenize-and-parse "" parser/parse-logical-and-expression)]
      (is (not (:success? result)) "Пустой ввод должен вызывать ошибку"))))

;; ============================================================================
;; ДЕМОНСТРАЦИЯ УЛУЧШЕНИЙ
;; ============================================================================

(deftest test-code-quality-improvements
  "Демонстрирует улучшения качества кода после рефакторинга"
  (testing "Уменьшение дублирования кода"
    ;; Все функции должны быть функциями (не обязательно одинакового типа)
    (is (fn? parser/parse-multiplicative-expression) "parse-multiplicative-expression должна быть функцией")
    (is (fn? parser/parse-additive-expression) "parse-additive-expression должна быть функцией")
    (is (fn? parser/parse-shift-expression) "parse-shift-expression должна быть функцией")
    (is (fn? parser/parse-relational-expression) "parse-relational-expression должна быть функцией")
    (is (fn? parser/parse-equality-expression) "parse-equality-expression должна быть функцией")
    (is (fn? parser/parse-logical-and-expression) "parse-logical-and-expression должна быть функцией"))
  
  (testing "Улучшенная читаемость"
    ;; Предикаты делают код более читаемым
    (is (parser/additive-operator? {:type :math-operator :value :plus}))
    (is (parser/multiplicative-operator? {:type :math-operator :value :multiply}))
    (is (parser/relational-operator? {:type :comparison-operator :value :less})))
  
  (testing "Легкость расширения"
    ;; Можно легко добавить новые типы операторов
    (let [bitwise-operator? (fn [token] 
                             (and (= (:type token) :bitwise-operator)
                                  (#{:and :or :xor} (:value token))))
          bitwise-parser (parser/parse-binary-expression-with-operators 
                         parser/parse-primary-expression
                         bitwise-operator?)]
      (is (fn? bitwise-parser) "Должен создавать новый парсер")))) 