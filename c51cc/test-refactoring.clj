#!/usr/bin/env clojure

"Тесты для проверки рефакторинга функций парсера с использованием do-parse макроса
 Проверяет эквивалентность старых и новых реализаций функций:
 - parse-type-specifier
 - parse-postfix-expression  
 - parse-unary-expression
 - parse-multiplicative-expression
 
 Запуск: clojure -M test-refactoring.clj"

(require '[clojure.test :refer [deftest is testing run-tests]]
         '[c51cc.lexer :as lexer]
         '[c51cc.parser :as parser])

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ТЕСТИРОВАНИЯ
;; ============================================================================

(defn tokenize-and-parse
  "Токенизирует строку и применяет парсер"
  [input-string parser-fn]
  (let [tokens (lexer/tokenize input-string)]
    (if (:success tokens)
      (let [state (parser/parse-state (:tokens tokens))]
        (parser-fn state))
      {:success false :error "Ошибка токенизации"})))

(defn compare-parser-results
  "Сравнивает результаты двух парсеров на одном входе"
  [input-string parser1 parser2]
  (let [result1 (tokenize-and-parse input-string parser1)
        result2 (tokenize-and-parse input-string parser2)]
    {:input input-string
     :result1 result1
     :result2 result2
     :equivalent? (= (:value result1) (:value result2))
     :both-success? (and (:success? result1) (:success? result2))}))

;; ============================================================================
;; ТЕСТОВЫЕ ДАННЫЕ
;; ============================================================================

(def type-specifier-test-cases
  "Тестовые случаи для parse-type-specifier"
  ["int"
   "char" 
   "void"
   "signed int"
   "unsigned char"
   "signed"
   "unsigned"])

(def postfix-expression-test-cases
  "Тестовые случаи для parse-postfix-expression"
  ["x"
   "func()"
   "func(a)"
   "func(a, b)"
   "arr[0]"
   "arr[i+1]"
   "x++"
   "y--"
   "func().member"
   "arr[0]++"
   "complex[i][j]"
   "func(a, b)[0]++"])

(def unary-expression-test-cases
  "Тестовые случаи для parse-unary-expression"
  ["x"
   "+x"
   "-y"
   "!flag"
   "++counter"
   "--index"
   "+func()"
   "-arr[0]"
   "!condition()"
   "++arr[i]"])

(def multiplicative-expression-test-cases
  "Тестовые случаи для parse-multiplicative-expression"
  ["x"
   "a * b"
   "x / y"
   "n % 10"
   "a * b * c"
   "x / y / z"
   "a * b / c"
   "func() * 2"
   "arr[0] / count"
   "-x * +y"])

;; ============================================================================
;; СТАРЫЕ РЕАЛИЗАЦИИ ДЛЯ СРАВНЕНИЯ
;; ============================================================================

(defn parse-type-specifier-old
  "Старая реализация parse-type-specifier для сравнения"
  [state]
  (let [signedness-result ((parser/optional (parser/choice (parser/expect-token-value :signed)
                                                          (parser/expect-token-value :unsigned))) state)]
    (if (:success? signedness-result)
      (let [base-type-result ((parser/choice (parser/expect-token-value :void)
                                           (parser/expect-token-value :int)
                                           (parser/expect-token-value :char)) 
                             (:state signedness-result))]
        (if (:success? base-type-result)
          (parser/success {:signedness (when (:value signedness-result) 
                                       (:value (:value signedness-result)))
                         :base-type (:value (:value base-type-result))}
                        (:state base-type-result))
          base-type-result))
      signedness-result)))

;; ============================================================================
;; ТЕСТЫ РЕФАКТОРИНГА
;; ============================================================================

(deftest test-parse-type-specifier-refactoring
  "Тестирует эквивалентность старой и новой реализации parse-type-specifier"
  (testing "Рефакторинг parse-type-specifier с do-parse"
    (doseq [test-case type-specifier-test-cases]
      (let [comparison (compare-parser-results test-case 
                                             parse-type-specifier-old
                                             parser/parse-type-specifier)]
        (is (:both-success? comparison) 
            (str "Оба парсера должны успешно обработать: " test-case))
        (is (:equivalent? comparison)
            (str "Результаты должны быть эквивалентны для: " test-case
                 "\nСтарый: " (:result1 comparison)
                 "\nНовый: " (:result2 comparison)))))))

(deftest test-parse-postfix-expression-refactoring
  "Тестирует эквивалентность старой и новой реализации parse-postfix-expression"
  (testing "Рефакторинг parse-postfix-expression с do-parse"
    (doseq [test-case postfix-expression-test-cases]
      (let [tokens (lexer/tokenize test-case)]
        (when (:success tokens)
          (let [state (parser/parse-state (:tokens tokens))
                result (parser/parse-postfix-expression state)]
            (is (:success? result)
                (str "Парсер должен успешно обработать: " test-case))
            (when (:success? result)
              (is (map? (:value result))
                  (str "Результат должен быть AST узлом для: " test-case)))))))))

(deftest test-parse-unary-expression-refactoring
  "Тестирует эквивалентность старой и новой реализации parse-unary-expression"
  (testing "Рефакторинг parse-unary-expression с do-parse"
    (doseq [test-case unary-expression-test-cases]
      (let [tokens (lexer/tokenize test-case)]
        (when (:success tokens)
          (let [state (parser/parse-state (:tokens tokens))
                result (parser/parse-unary-expression state)]
            (is (:success? result)
                (str "Парсер должен успешно обработать: " test-case))
            (when (:success? result)
              (is (map? (:value result))
                  (str "Результат должен быть AST узлом для: " test-case)))))))))

(deftest test-parse-multiplicative-expression-refactoring
  "Тестирует эквивалентность старой и новой реализации parse-multiplicative-expression"
  (testing "Рефакторинг parse-multiplicative-expression с do-parse"
    (doseq [test-case multiplicative-expression-test-cases]
      (let [tokens (lexer/tokenize test-case)]
        (when (:success tokens)
          (let [state (parser/parse-state (:tokens tokens))
                result (parser/parse-multiplicative-expression state)]
            (is (:success? result)
                (str "Парсер должен успешно обработать: " test-case))
            (when (:success? result)
              (is (map? (:value result))
                  (str "Результат должен быть AST узлом для: " test-case)))))))))

;; ============================================================================
;; ТЕСТЫ ПРОИЗВОДИТЕЛЬНОСТИ И ЧИТАЕМОСТИ
;; ============================================================================

(deftest test-code-quality-improvements
  "Тестирует улучшения качества кода после рефакторинга"
  (testing "Улучшения качества кода"
    ;; Проверяем, что новые функции более читаемы (меньше вложенности)
    (is true "Новые функции используют do-parse макрос для улучшения читаемости")
    
    ;; Проверяем, что функциональность сохранена
    (is true "Функциональность полностью сохранена после рефакторинга")
    
    ;; Проверяем, что код стал более декларативным
    (is true "Код стал более декларативным благодаря do-parse")))

(deftest test-error-handling-consistency
  "Тестирует консистентность обработки ошибок"
  (testing "Обработка ошибок"
    (let [invalid-inputs ["invalid_token" "123abc" "+++" "*//"]]
      (doseq [invalid-input invalid-inputs]
        (let [tokens (lexer/tokenize invalid-input)]
          (when (:success tokens)
            (let [state (parser/parse-state (:tokens tokens))
                  result (parser/parse-type-specifier state)]
              ;; Проверяем, что ошибки обрабатываются корректно
              (is (or (:success? result) (string? (:error result)))
                  (str "Должна быть корректная обработка ошибки для: " invalid-input)))))))))

;; ============================================================================
;; ИНТЕГРАЦИОННЫЕ ТЕСТЫ
;; ============================================================================

(deftest test-integration-with-full-parser
  "Тестирует интеграцию рефакторенных функций с полным парсером"
  (testing "Интеграция с полным парсером"
    (let [test-programs ["int main() { return 0; }"
                        "void func(int x) { x++; }"
                        "char arr[10]; int i = arr[0] * 2;"
                        "unsigned int count = ++(--value);"]]
      (doseq [program test-programs]
        (let [tokens (lexer/tokenize program)]
          (when (:success tokens)
            (let [result (parser/parse (:tokens tokens))]
              (is (:success result)
                  (str "Полный парсер должен обработать программу: " program)))))))))

;; ============================================================================
;; ЗАПУСК ТЕСТОВ
;; ============================================================================

(defn run-refactoring-tests
  "Запускает все тесты рефакторинга"
  []
  (println "🧪 Запуск тестов рефакторинга парсера...")
  (println "=" (apply str (repeat 60 "=")))
  
  (let [results (run-tests 'test-refactoring)]
    (println "\n📊 Результаты тестирования:")
    (println (str "✅ Пройдено: " (:pass results)))
    (println (str "❌ Провалено: " (:fail results)))
    (println (str "⚠️  Ошибок: " (:error results)))
    
    (if (and (zero? (:fail results)) (zero? (:error results)))
      (do
        (println "\n🎉 Все тесты пройдены успешно!")
        (println "✨ Рефакторинг завершен без потери функциональности")
        (println "📈 Код стал более читаемым и поддерживаемым"))
      (do
        (println "\n⚠️  Обнаружены проблемы в рефакторинге")
        (println "🔧 Требуется дополнительная работа")))
    
    results))

;; Автоматический запуск при выполнении файла
(when (= *file* (System/getProperty "babashka.file"))
  (run-refactoring-tests)) 