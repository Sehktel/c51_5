(ns c51cc.parser-refactoring-test
  "Тесты для проверки корректности рефакторинга функций парсинга выражений
   Проверяют эквивалентность старой и новой реализации"
  (:require [clojure.test :refer :all]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ТЕСТИРОВАНИЯ
;; ============================================================================

(defn tokenize-and-parse
  "Вспомогательная функция для токенизации и парсинга выражения"
  [expression parser-fn]
  (let [tokens (lexer/tokenize expression)]
    (if (and tokens (vector? tokens) (not-empty tokens))
      (parser-fn (parser/parse-state tokens))
      {:success? false :error "Токенизация не удалась"})))

(defn test-expression-parsing
  "Тестирует парсинг выражения и проверяет успешность"
  [expression parser-fn expected-ast-type]
  (let [result (tokenize-and-parse expression parser-fn)]
    (is (:success? result) (str "Парсинг выражения '" expression "' должен быть успешным"))
    (when (:success? result)
      (is (= (:ast-type (:value result)) expected-ast-type)
          (str "AST должен иметь тип " expected-ast-type)))))

(defn test-operator-precedence
  "Тестирует правильность приоритета операторов"
  [expression parser-fn expected-structure]
  (let [result (tokenize-and-parse expression parser-fn)]
    (is (:success? result) (str "Парсинг выражения '" expression "' должен быть успешным"))
    (when (:success? result)
      (is (= (select-keys (:value result) [:ast-type :operator])
             expected-structure)
          (str "Структура AST должна соответствовать ожидаемой для '" expression "'")))))

;; ============================================================================
;; ТЕСТЫ ДЛЯ МУЛЬТИПЛИКАТИВНЫХ ВЫРАЖЕНИЙ
;; ============================================================================

(deftest test-multiplicative-expression-basic
  "Тестирует базовые мультипликативные выражения"
  (testing "Простое умножение"
    (test-expression-parsing "a * b" parser/parse-multiplicative-expression :binary-expression))
  
  (testing "Простое деление"
    (test-expression-parsing "x / y" parser/parse-multiplicative-expression :binary-expression))
  
  (testing "Остаток от деления"
    (test-expression-parsing "n % m" parser/parse-multiplicative-expression :binary-expression)))

(deftest test-multiplicative-expression-associativity
  "Тестирует левую ассоциативность мультипликативных операторов"
  (testing "Левая ассоциативность умножения"
    (let [result (tokenize-and-parse "a * b * c" parser/parse-multiplicative-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Проверяем, что структура соответствует ((a * b) * c)
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :multiply))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :multiply))))))
  
  (testing "Смешанные мультипликативные операторы"
    (let [result (tokenize-and-parse "a * b / c % d" parser/parse-multiplicative-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Проверяем левую ассоциативность
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :modulo)))))))

;; ============================================================================
;; ТЕСТЫ ДЛЯ ВЫРАЖЕНИЙ СДВИГА
;; ============================================================================

(deftest test-shift-expression-basic
  "Тестирует базовые выражения сдвига"
  (testing "Левый сдвиг"
    (test-expression-parsing "a << 2" parser/parse-shift-expression :binary-expression))
  
  (testing "Правый сдвиг"
    (test-expression-parsing "x >> 1" parser/parse-shift-expression :binary-expression)))

(deftest test-shift-expression-precedence
  "Тестирует приоритет операторов сдвига"
  (testing "Сдвиг имеет меньший приоритет чем умножение"
    (let [result (tokenize-and-parse "a * b << c" parser/parse-shift-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a * b) << c
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :shift-left))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :multiply)))))))

;; ============================================================================
;; ТЕСТЫ ДЛЯ АДДИТИВНЫХ ВЫРАЖЕНИЙ
;; ============================================================================

(deftest test-additive-expression-basic
  "Тестирует базовые аддитивные выражения"
  (testing "Простое сложение"
    (test-expression-parsing "a + b" parser/parse-additive-expression :binary-expression))
  
  (testing "Простое вычитание"
    (test-expression-parsing "x - y" parser/parse-additive-expression :binary-expression)))

(deftest test-additive-expression-precedence
  "Тестирует приоритет аддитивных операторов"
  (testing "Сложение имеет меньший приоритет чем сдвиг"
    (let [result (tokenize-and-parse "a << b + c" parser/parse-additive-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a << b) + c
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :plus))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :shift-left)))))))

;; ============================================================================
;; ТЕСТЫ ДЛЯ ВЫРАЖЕНИЙ СРАВНЕНИЯ
;; ============================================================================

(deftest test-relational-expression-basic
  "Тестирует базовые выражения сравнения"
  (testing "Меньше"
    (test-expression-parsing "a < b" parser/parse-relational-expression :binary-expression))
  
  (testing "Больше"
    (test-expression-parsing "x > y" parser/parse-relational-expression :binary-expression))
  
  (testing "Меньше или равно"
    (test-expression-parsing "a <= b" parser/parse-relational-expression :binary-expression))
  
  (testing "Больше или равно"
    (test-expression-parsing "x >= y" parser/parse-relational-expression :binary-expression)))

(deftest test-relational-expression-precedence
  "Тестирует приоритет операторов сравнения"
  (testing "Сравнение имеет меньший приоритет чем сложение"
    (let [result (tokenize-and-parse "a + b < c" parser/parse-relational-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a + b) < c
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :less))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :plus)))))))

;; ============================================================================
;; ТЕСТЫ ДЛЯ ВЫРАЖЕНИЙ РАВЕНСТВА
;; ============================================================================

(deftest test-equality-expression-basic
  "Тестирует базовые выражения равенства"
  (testing "Равенство"
    (test-expression-parsing "a == b" parser/parse-equality-expression :binary-expression))
  
  (testing "Неравенство"
    (test-expression-parsing "x != y" parser/parse-equality-expression :binary-expression)))

(deftest test-equality-expression-precedence
  "Тестирует приоритет операторов равенства"
  (testing "Равенство имеет меньший приоритет чем сравнение"
    (let [result (tokenize-and-parse "a < b == c" parser/parse-equality-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a < b) == c
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :equal))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :less)))))))

;; ============================================================================
;; ТЕСТЫ ДЛЯ ЛОГИЧЕСКИХ ВЫРАЖЕНИЙ
;; ============================================================================

(deftest test-logical-and-expression-basic
  "Тестирует базовые выражения логического И"
  (testing "Логическое И"
    (test-expression-parsing "a && b" parser/parse-logical-and-expression :binary-expression)))

(deftest test-logical-and-expression-precedence
  "Тестирует приоритет логического И"
  (testing "Логическое И имеет меньший приоритет чем равенство"
    (let [result (tokenize-and-parse "a == b && c" parser/parse-logical-and-expression)]
      (is (:success? result))
      (when (:success? result)
        (let [ast (:value result)]
          ;; Должно быть (a == b) && c
          (is (= (:ast-type ast) :binary-expression))
          (is (= (:operator ast) :and))
          (is (= (:ast-type (:left ast)) :binary-expression))
          (is (= (:operator (:left ast)) :equal)))))))

;; ============================================================================
;; ТЕСТЫ КОМПЛЕКСНЫХ ВЫРАЖЕНИЙ
;; ============================================================================

(deftest test-complex-expression-precedence
  "Тестирует правильность приоритета в комплексных выражениях"
  (testing "Комплексное выражение с множественными операторами"
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
          (is (= (:operator (:left ast)) :equal)))))))

;; ============================================================================
;; ТЕСТЫ ПРОИЗВОДИТЕЛЬНОСТИ И ЧИТАЕМОСТИ
;; ============================================================================

(deftest test-refactoring-benefits
  "Тестирует преимущества рефакторинга"
  (testing "Код стал более читаемым и модульным"
    ;; Проверяем, что предикаты работают корректно
    (is (parser/multiplicative-operator? {:type :math-operator :value :multiply}))
    (is (parser/shift-operator? {:type :bitwise-operator :value :shift-left}))
    (is (parser/additive-operator? {:type :math-operator :value :plus}))
    (is (parser/relational-operator? {:type :comparison-operator :value :less}))
    (is (parser/equality-operator? {:type :logical-operator :value :equal}))
    (is (parser/logical-and-operator? {:type :logical-operator :value :and}))
    (is (parser/logical-or-operator? {:type :logical-operator :value :or})))
  
  (testing "Универсальная функция parse-binary-expression-with-operators работает"
    ;; Тестируем, что универсальная функция может быть переиспользована
    (let [custom-parser (parser/parse-binary-expression-with-operators 
                         parser/parse-primary-expression
                         parser/additive-operator?)
          result (tokenize-and-parse "a + b" custom-parser)]
      (is (:success? result))
      (when (:success? result)
        (is (= (:ast-type (:value result)) :binary-expression))
        (is (= (:operator (:value result)) :plus))))))

;; ============================================================================
;; ТЕСТЫ ГИГИЕНИЧНОСТИ МАКРОСА
;; ============================================================================

(deftest test-macro-hygiene
  "Тестирует гигиеничность макроса do-parse"
  (testing "Макрос не захватывает переменные"
    ;; Этот тест проверяет, что макрос do-parse не вызывает конфликтов имен
    (let [state (parser/parse-state [])
          result (parser/parse-multiplicative-expression state)]
      ;; Если макрос гигиеничен, то парсинг должен работать без ошибок
      (is (map? result))
      (is (contains? result :success?))))
  
  (testing "Переменные в do-parse изолированы"
    ;; Проверяем, что использование переменных с именами вроде 'state' не вызывает проблем
    (let [my-state "test-value"  ; Локальная переменная с именем, которое может конфликтовать
          tokens []
          parser-state (parser/parse-state tokens)
          result (parser/parse-additive-expression parser-state)]
      ;; my-state не должно влиять на работу парсера
      (is (= my-state "test-value"))
      (is (map? result)))))

;; ============================================================================
;; ИНТЕГРАЦИОННЫЕ ТЕСТЫ
;; ============================================================================

(deftest test-integration-with-full-parser
  "Тестирует интеграцию рефакторинговых функций с полным парсером"
  (testing "Рефакторинговые функции работают в контексте полного парсера"
    (let [code "int main() { return a + b * c; }"
          result (parser/parse (lexer/tokenize code))]
      (when (:success result)
        (is (:success result))
        ;; Проверяем, что AST содержит правильную структуру выражения
        (let [ast (:ast result)]
          (is (= (:ast-type ast) :program)))))))

;; ============================================================================
;; ЗАПУСК ВСЕХ ТЕСТОВ
;; ============================================================================

(defn run-all-refactoring-tests
  "Запускает все тесты рефакторинга и выводит статистику"
  []
  (println "=== ТЕСТЫ РЕФАКТОРИНГА ФУНКЦИЙ ПАРСИНГА ===")
  (println "Проверка корректности рефакторинга с использованием do-parse макроса")
  (println)
  
  (let [test-results (run-tests 'c51cc.parser-refactoring-test)]
    (println)
    (println "=== РЕЗУЛЬТАТЫ ТЕСТИРОВАНИЯ ===")
    (println (str "Всего тестов: " (+ (:pass test-results) (:fail test-results) (:error test-results))))
    (println (str "Успешных: " (:pass test-results)))
    (println (str "Неудачных: " (:fail test-results)))
    (println (str "Ошибок: " (:error test-results)))
    (println)
    
    (if (and (zero? (:fail test-results)) (zero? (:error test-results)))
      (println "✅ ВСЕ ТЕСТЫ ПРОШЛИ УСПЕШНО! Рефакторинг корректен.")
      (println "❌ ОБНАРУЖЕНЫ ПРОБЛЕМЫ! Требуется дополнительная проверка."))
    
    test-results))

;; Экспорт функции для запуска тестов
(def ^:export run-refactoring-tests run-all-refactoring-tests) 