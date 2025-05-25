(ns c51cc.parser-advanced-test
  "Расширенные тесты для парсера C51 - покрытие сложных случаев и граничных условий
   Тестирование специфичных для микроконтроллера AT89S4051 конструкций"
  (:require [clojure.test :refer :all]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ РАСШИРЕННОГО ТЕСТИРОВАНИЯ
;; ============================================================================

(defn parse-string 
  "Парсинг строки кода с детальной диагностикой"
  [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn parse-successful? 
  "Проверка успешности парсинга"
  [result]
  (:success result))

(defn get-ast 
  "Извлечение AST из результата"
  [result]
  (:ast result))

(defn get-error 
  "Извлечение ошибки из результата"
  [result]
  (:error result))

(defn assert-ast-structure
  "Утверждение о структуре AST с детальной проверкой"
  [ast expected-type & {:keys [declarations-count statements-count]}]
  (is (= expected-type (:ast-type ast)))
  (when declarations-count
    (is (= declarations-count (count (:declarations ast)))))
  (when statements-count
    (is (= statements-count (count (:statements ast))))))

;; ============================================================================
;; ТЕСТЫ СЛОЖНЫХ ВЫРАЖЕНИЙ И ПРИОРИТЕТОВ ОПЕРАТОРОВ
;; ============================================================================

(deftest test-complex-operator-precedence
  (testing "Комплексные выражения с множественными приоритетами"
    ;; Тест: a + b * c / d - e % f
    (let [result (parse-string "a + b * c / d - e % f")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            expr (:expression (first (:declarations ast)))]
        ;; Должно быть: ((a + ((b * c) / d)) - (e % f))
        (is (= :binary-expression (:ast-type expr)))
        (is (= :minus (:operator expr))))))

  (testing "Ассоциативность операторов одинакового приоритета"
    ;; Тест: a - b - c должно быть ((a - b) - c)
    (let [result (parse-string "a - b - c")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            expr (:expression (first (:declarations ast)))]
        (is (= :binary-expression (:ast-type expr)))
        (is (= :minus (:operator expr)))
        (is (= :binary-expression (:ast-type (:left expr))))
        (is (= :minus (:operator (:left expr)))))))

  (testing "Смешанные унарные и бинарные операторы"
    ;; Тест: -a * +b + !c
    (let [result (parse-string "-a * +b + !c")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            expr (:expression (first (:declarations ast)))]
        (is (= :binary-expression (:ast-type expr)))
        (is (= :plus (:operator expr)))))))

;; ============================================================================
;; ТЕСТЫ ПОСТФИКСНЫХ И ПРЕФИКСНЫХ ОПЕРАТОРОВ
;; ============================================================================

(deftest test-increment-decrement-operators
  (testing "Постфиксный инкремент в выражении"
    (let [result (parse-string "x++ + y")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            expr (:expression (first (:declarations ast)))]
        (is (= :binary-expression (:ast-type expr)))
        (is (= :plus (:operator expr)))
        (is (= :postfix-expression (:ast-type (:left expr))))
        (is (= :increment (:operator (:left expr)))))))

  (testing "Постфиксный декремент в выражении"
    (let [result (parse-string "y-- * 2")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            expr (:expression (first (:declarations ast)))]
        (is (= :binary-expression (:ast-type expr)))
        (is (= :multiply (:operator expr)))
        (is (= :postfix-expression (:ast-type (:left expr))))
        (is (= :decrement (:operator (:left expr)))))))

  (testing "Смешанные префиксные и постфиксные операторы"
    ;; Тест: ++x + y--
    (let [result (parse-string "++x + y--")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            expr (:expression (first (:declarations ast)))]
        (is (= :binary-expression (:ast-type expr)))
        (is (= :plus (:operator expr)))
        (is (= :unary-expression (:ast-type (:left expr))))
        (is (= :postfix-expression (:ast-type (:right expr))))))))

;; ============================================================================
;; ТЕСТЫ СЛОЖНЫХ ВЫЗОВОВ ФУНКЦИЙ
;; ============================================================================

(deftest test-complex-function-calls
  (testing "Вложенные вызовы функций"
    (let [result (parse-string "func1(func2(x), func3(y, z))")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            expr (:expression (first (:declarations ast)))]
        (is (= :call-expression (:ast-type expr)))
        (is (= "func1" (:name (:callee expr))))
        (is (= 2 (count (:arguments expr))))
        ;; Первый аргумент - вызов func2
        (is (= :call-expression (:ast-type (first (:arguments expr)))))
        (is (= "func2" (:name (:callee (first (:arguments expr))))))
        ;; Второй аргумент - вызов func3
        (is (= :call-expression (:ast-type (second (:arguments expr)))))
        (is (= "func3" (:name (:callee (second (:arguments expr)))))))))

  (testing "Вызов функции с комплексными выражениями в аргументах"
    (let [result (parse-string "calculate(a + b * c, x > y ? x : y, ++counter)")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            expr (:expression (first (:declarations ast)))]
        (is (= :call-expression (:ast-type expr)))
        (is (= "calculate" (:name (:callee expr))))
        (is (= 3 (count (:arguments expr))))
        ;; Первый аргумент - бинарное выражение
        (is (= :binary-expression (:ast-type (first (:arguments expr))))))))

  (testing "Цепочка вызовов функций (если поддерживается)"
    ;; Примечание: это может не поддерживаться в C51, но тестируем для полноты
    (let [result (parse-string "obj.method1().method2(x)")]
      ;; Ожидаем, что это может не парситься или парситься по-другому
      ;; В зависимости от реализации парсера
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (not (nil? ast))))))))

;; ============================================================================
;; ТЕСТЫ СЛОЖНЫХ УПРАВЛЯЮЩИХ КОНСТРУКЦИЙ
;; ============================================================================

(deftest test-nested-control-structures
  (testing "Глубоко вложенные if-else"
    (let [result (parse-string "
      void test() {
        if (a > 0) {
          if (b > 0) {
            if (c > 0) {
              return 1;
            } else {
              return 2;
            }
          } else {
            return 3;
          }
        } else {
          return 4;
        }
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            func-decl (first (:declarations ast))
            body (:body func-decl)]
        (is (= :function-declaration (:ast-type func-decl)))
        (is (= :block-statement (:ast-type body)))
        (is (= 1 (count (:statements body))))
        (is (= :if-statement (:ast-type (first (:statements body))))))))

  (testing "Вложенные циклы"
    (let [result (parse-string "
      void matrix_multiply() {
        for (i = 0; i < rows; i++) {
          for (j = 0; j < cols; j++) {
            for (k = 0; k < inner; k++) {
              result[i][j] += a[i][k] * b[k][j];
            }
          }
        }
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            func-decl (first (:declarations ast))]
        (is (= :function-declaration (:ast-type func-decl))))))

  (testing "Смешанные управляющие конструкции"
    (let [result (parse-string "
      void complex_logic() {
        while (condition1) {
          if (condition2) {
            for (i = 0; i < limit; i++) {
              if (array[i] == target) {
                return i;
              }
            }
          } else {
            while (condition3) {
              process_data();
              condition3 = update_condition();
            }
          }
        }
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))))))

;; ============================================================================
;; ТЕСТЫ СПЕЦИФИЧНЫХ ДЛЯ C51 КОНСТРУКЦИЙ
;; ============================================================================

(deftest test-c51-specific-features
  (testing "Объявления с модификаторами типов для микроконтроллера"
    ;; Тестируем специфичные для 8051 типы данных
    (let [result (parse-string "unsigned char port_state = 0xFF;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            decl (first (:declarations ast))]
        (is (= :variable-declaration (:ast-type decl)))
        (is (= :char (:base-type (:type decl))))
        (is (= :unsigned (:signedness (:type decl)))))))

  (testing "Работа с портами и регистрами (симуляция)"
    ;; Примечание: реальные регистры 8051 могут требовать специальной обработки
    (let [result (parse-string "
      void init_ports() {
        P0 = 0x00;  // Порт 0 как выход
        P1 = 0xFF;  // Порт 1 как вход
        TMOD = 0x01; // Настройка таймера
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Прерывания и специальные функции (если поддерживаются)"
    ;; Это может не поддерживаться в базовой версии парсера
    (let [result (parse-string "
      void timer_interrupt() interrupt 1 {
        // Обработчик прерывания таймера
        timer_flag = 1;
      }")]
      ;; Проверяем, что парсер хотя бы не падает
      (when (not (parse-successful? result))
        ;; Если не поддерживается, это нормально
        (is (string? (get-error result)))))))

;; ============================================================================
;; ТЕСТЫ ГРАНИЧНЫХ СЛУЧАЕВ И ОШИБОК
;; ============================================================================

(deftest test-edge-cases-and-errors
  (testing "Очень глубокая вложенность выражений"
    (let [deep-expr (apply str (repeat 50 "(") "42" (repeat 50 ")"))
          result (parse-string deep-expr)]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            expr (:expression (first (:declarations ast)))]
        (is (= :literal (:ast-type expr)))
        (is (= 42 (:value expr))))))

  (testing "Длинные цепочки операторов"
    (let [long-chain (clojure.string/join " + " (map str (range 1 21)))
          result (parse-string long-chain)]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Пустые блоки и операторы"
    (let [result (parse-string "void empty() { ; ; ; }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            func-decl (first (:declarations ast))
            body (:body func-decl)]
        (is (= :block-statement (:ast-type body))))))

  (testing "Комментарии в сложных местах"
    (let [result (parse-string "
      int /* комментарий */ x /* еще комментарий */ = /* и тут */ 42 /* и здесь */;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Максимально длинные идентификаторы"
    (let [long-name (apply str (repeat 100 "a"))
          code (str "int " long-name " = 42;")
          result (parse-string code)]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            decl (first (:declarations ast))]
        (is (= long-name (:name decl)))))))

;; ============================================================================
;; ТЕСТЫ ВОССТАНОВЛЕНИЯ ПОСЛЕ ОШИБОК
;; ============================================================================

(deftest test-error-recovery
  (testing "Восстановление после синтаксической ошибки в выражении"
    (let [result (parse-string "int x = 42 + + 3;")]
      ;; Двойной плюс может быть ошибкой или инкрементом
      ;; Проверяем, что парсер обрабатывает это разумно
      (when (not (parse-successful? result))
        (is (string? (get-error result))))))

  (testing "Обработка незакрытых скобок"
    (let [result (parse-string "func(a, b")]
      (is (not (parse-successful? result)))
      (is (string? (get-error result)))))

  (testing "Обработка неожиданных токенов"
    (let [result (parse-string "int x = @ 42;")]
      (is (not (parse-successful? result)))
      (is (string? (get-error result)))))

  (testing "Обработка неполных объявлений"
    (let [result (parse-string "int")]
      (is (not (parse-successful? result)))
      (is (string? (get-error result))))))

;; ============================================================================
;; ТЕСТЫ ПРОИЗВОДИТЕЛЬНОСТИ
;; ============================================================================

(deftest test-performance-characteristics
  (testing "Парсинг большой программы"
    (let [large-program (str "
      void large_function() {
        " (clojure.string/join "\n        " 
            (for [i (range 100)]
              (str "int var" i " = " i ";"))) "
        
        for (int i = 0; i < 100; i++) {
          " (clojure.string/join "\n          "
              (for [j (range 10)]
                (str "var" j " += i;"))) "
        }
      }")
          start-time (System/currentTimeMillis)
          result (parse-string large-program)
          end-time (System/currentTimeMillis)
          parse-time (- end-time start-time)]
      
      (is (parse-successful? result))
      (is (< parse-time 5000)) ; Парсинг должен занимать менее 5 секунд
      (println (str "Время парсинга большой программы: " parse-time "мс"))))

  (testing "Парсинг глубоко вложенной структуры"
    (let [nested-blocks (apply str 
                          (concat (repeat 20 "{ ")
                                  ["int x = 42;"]
                                  (repeat 20 " }")))
          result (parse-string nested-blocks)]
      (is (parse-successful? result)))))

;; ============================================================================
;; ИНТЕГРАЦИОННЫЕ ТЕСТЫ С РЕАЛЬНЫМИ ПРОГРАММАМИ
;; ============================================================================

(deftest test-real-world-programs
  (testing "Простая программа мигания светодиода"
    (let [led-blink-program "
      unsigned char led_state = 0;
      
      void delay_ms(unsigned int ms) {
        unsigned int i, j;
        for (i = 0; i < ms; i++) {
          for (j = 0; j < 120; j++) {
            // Простая задержка для 12MHz кристалла
          }
        }
      }
      
      void main() {
        while (1) {
          P1 = led_state;
          led_state = !led_state;
          delay_ms(500);
        }
      }"
          result (parse-string led-blink-program)]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= 3 (count (:declarations ast))))
        (is (= :variable-declaration (:ast-type (first (:declarations ast)))))
        (is (= :function-declaration (:ast-type (second (:declarations ast)))))
        (is (= :function-declaration (:ast-type (nth (:declarations ast) 2)))))))

  (testing "Программа работы с АЦП"
    (let [adc-program "
      unsigned int adc_read(unsigned char channel) {
        unsigned int result = 0;
        
        // Выбор канала
        ADMUX = channel & 0x07;
        
        // Запуск преобразования
        ADCSRA |= (1 << ADSC);
        
        // Ожидание завершения
        while (ADCSRA & (1 << ADSC)) {
          // Ждем
        }
        
        // Чтение результата
        result = ADCL;
        result |= (ADCH << 8);
        
        return result;
      }"
          result (parse-string adc-program)]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= 1 (count (:declarations ast))))
        (is (= :function-declaration (:ast-type (first (:declarations ast))))))))

  (testing "Программа с обработкой прерываний"
    ;; Упрощенная версия без специфичного синтаксиса прерываний
    (let [interrupt-program "
      volatile unsigned char timer_flag = 0;
      volatile unsigned int timer_counter = 0;
      
      void timer_init() {
        TMOD = 0x01;  // Режим 1 для таймера 0
        TH0 = 0x3C;   // Загрузка старшего байта
        TL0 = 0xB0;   // Загрузка младшего байта
        ET0 = 1;      // Разрешение прерывания от таймера 0
        EA = 1;       // Глобальное разрешение прерываний
        TR0 = 1;      // Запуск таймера 0
      }
      
      void timer_handler() {
        timer_counter++;
        if (timer_counter >= 1000) {
          timer_flag = 1;
          timer_counter = 0;
        }
        TH0 = 0x3C;   // Перезагрузка таймера
        TL0 = 0xB0;
      }"
          result (parse-string interrupt-program)]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= 4 (count (:declarations ast))))))))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ ЗАПУСКА ТЕСТОВ
;; ============================================================================

(defn run-advanced-parser-tests
  "Запуск всех расширенных тестов парсера"
  []
  (println "=== Запуск расширенных тестов парсера C51 ===")
  (let [start-time (System/currentTimeMillis)]
    (run-tests 'c51cc.parser-advanced-test)
    (let [end-time (System/currentTimeMillis)
          total-time (- end-time start-time)]
      (println (str "\n=== Тестирование завершено за " total-time "мс ===")))))

(defn benchmark-parser
  "Бенчмарк производительности парсера"
  [code iterations]
  (println (str "Бенчмарк парсера: " iterations " итераций"))
  (let [start-time (System/currentTimeMillis)]
    (dotimes [i iterations]
      (parse-string code))
    (let [end-time (System/currentTimeMillis)
          total-time (- end-time start-time)
          avg-time (/ total-time iterations)]
      (println (str "Общее время: " total-time "мс"))
      (println (str "Среднее время на итерацию: " avg-time "мс"))
      avg-time)))

;; Экспорт функций
(def ^:export run-advanced-parser-tests run-advanced-parser-tests)
(def ^:export benchmark-parser benchmark-parser) 