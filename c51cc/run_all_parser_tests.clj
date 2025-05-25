(ns c51cc.run-all-parser-tests
  "Главный файл для запуска всех тестов парсера C51
   Включает базовые, расширенные и специфичные для C51 тесты"
  (:require [clojure.test :refer :all]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]
            [c51cc.parser-test :refer :all]
            [c51cc.parser-advanced-test :refer :all]
            [c51cc.parser-c51-specific-test :refer :all]))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ АНАЛИЗА И ОТЧЕТНОСТИ
;; ============================================================================

(defn parse-and-analyze
  "Парсинг кода с детальным анализом результата"
  [code description]
  (println (str "\n--- " description " ---"))
  (println (str "Код: " code))
  
  (let [start-time (System/currentTimeMillis)
        tokens (lexer/tokenize code)
        parse-result (parser/parse tokens)
        end-time (System/currentTimeMillis)
        parse-time (- end-time start-time)]
    
    (println (str "Токенов: " (count tokens)))
    (println (str "Время парсинга: " parse-time "мс"))
    
    (if (:success parse-result)
      (do
        (println "✓ Парсинг успешен")
        (let [ast (:ast parse-result)]
          (println (str "Тип AST: " (:ast-type ast)))
          (when (:declarations ast)
            (println (str "Объявлений: " (count (:declarations ast)))))
          (when (parser/validate-ast ast)
            (println "✓ AST валиден"))
          true))
      (do
        (println "✗ Ошибка парсинга:")
        (println (str "  " (:error parse-result)))
        false))))

(defn test-parser-capabilities
  "Тестирование возможностей парсера на различных конструкциях"
  []
  (println "\n=== АНАЛИЗ ВОЗМОЖНОСТЕЙ ПАРСЕРА ===")
  
  (let [test-cases [
        ;; Базовые конструкции
        ["42" "Числовой литерал"]
        ["variable_name" "Идентификатор"]
        ["a + b * c" "Арифметическое выражение"]
        ["int x = 42;" "Объявление переменной"]
        ["func(a, b, c)" "Вызов функции"]
        
        ;; Управляющие конструкции
        ["if (x > 0) return x;" "Условный оператор"]
        ["while (condition) { process(); }" "Цикл while"]
        ["for (i = 0; i < 10; i++) sum += i;" "Цикл for"]
        
        ;; Функции
        ["void main() { return; }" "Простая функция"]
        ["int add(int a, int b) { return a + b; }" "Функция с параметрами"]
        
        ;; Сложные выражения
        ["(a + b) * (c - d)" "Выражение со скобками"]
        ["++x + y--" "Префиксные и постфиксные операторы"]
        ["a && b || c" "Логические операторы"]
        
        ;; Специфичные для C51
        ["unsigned char port = 0xFF;" "Беззнаковый тип"]
        ["P1 = (P1 & 0xF0) | 0x0F;" "Битовые операции"]
        ["volatile unsigned int counter = 0;" "Модификатор volatile"]
        
        ;; Массивы и указатели
        ["int array[10];" "Объявление массива"]
        ["*ptr = value;" "Разыменование указателя"]
        ["array[index] = new_value;" "Доступ к элементу массива"]
        
        ;; Структуры (если поддерживаются)
        ["struct point { int x; int y; };" "Объявление структуры"]
        
        ;; Комплексные программы
        ["void delay() { for (int i = 0; i < 1000; i++); }" "Функция задержки"]
        ["int factorial(int n) { return n <= 1 ? 1 : n * factorial(n-1); }" "Рекурсивная функция"]]
        
        successful-tests (atom 0)
        total-tests (count test-cases)]
    
    (doseq [[code description] test-cases]
      (when (parse-and-analyze code description)
        (swap! successful-tests inc)))
    
    (println (str "\n=== ИТОГИ АНАЛИЗА ==="))
    (println (str "Успешных тестов: " @successful-tests "/" total-tests))
    (println (str "Процент успеха: " (int (* 100 (/ @successful-tests total-tests))) "%"))
    
    @successful-tests))

(defn benchmark-parser-performance
  "Бенчмарк производительности парсера на различных типах кода"
  []
  (println "\n=== БЕНЧМАРК ПРОИЗВОДИТЕЛЬНОСТИ ===")
  
  (let [test-programs [
        ;; Простая программа
        ["int x = 42;" "Простое объявление" 1000]
        
        ;; Средняя сложность
        ["void func() { for (int i = 0; i < 10; i++) { sum += i; } }" 
         "Функция с циклом" 500]
        
        ;; Сложная программа
        [(str "int factorial(int n) { "
              "if (n <= 1) return 1; "
              "else return n * factorial(n - 1); "
              "} "
              "void main() { "
              "int result = factorial(10); "
              "}")
         "Рекурсивная функция" 100]
        
        ;; Очень сложная программа
        [(str "unsigned char led_patterns[8] = {0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80}; "
              "void display_pattern(unsigned char pattern) { "
              "P1 = pattern; "
              "for (unsigned int i = 0; i < 1000; i++) { "
              "for (unsigned int j = 0; j < 100; j++); "
              "} "
              "} "
              "void main() { "
              "unsigned char index = 0; "
              "while (1) { "
              "display_pattern(led_patterns[index]); "
              "index = (index + 1) % 8; "
              "} "
              "}")
         "Комплексная программа" 50]]]
    
    (doseq [[code description iterations] test-programs]
      (println (str "\nТест: " description))
      (println (str "Итераций: " iterations))
      
      (let [start-time (System/currentTimeMillis)]
        (dotimes [i iterations]
          (let [tokens (lexer/tokenize code)]
            (parser/parse tokens)))
        (let [end-time (System/currentTimeMillis)
              total-time (- end-time start-time)
              avg-time (/ total-time iterations)]
          (println (str "Общее время: " total-time "мс"))
          (println (str "Среднее время: " (format "%.2f" avg-time) "мс"))
          (println (str "Скорость: " (int (/ iterations (/ total-time 1000))) " парсингов/сек")))))))

(defn generate-test-report
  "Генерация подробного отчета о тестировании"
  []
  (println "\n" (apply str (repeat 80 "=")))
  (println "           ПОЛНЫЙ ОТЧЕТ О ТЕСТИРОВАНИИ ПАРСЕРА C51")
  (println (apply str (repeat 80 "=")))
  
  (let [start-time (System/currentTimeMillis)]
    
    ;; Запуск базовых тестов
    (println "\n1. БАЗОВЫЕ ТЕСТЫ ПАРСЕРА")
    (println (apply str (repeat 40 "-")))
    (let [basic-results (run-tests 'c51cc.parser-test)]
      (println (str "Базовые тесты: " (:pass basic-results) " успешных, " 
                   (:fail basic-results) " неудачных, " 
                   (:error basic-results) " ошибок")))
    
    ;; Запуск расширенных тестов
    (println "\n2. РАСШИРЕННЫЕ ТЕСТЫ ПАРСЕРА")
    (println (apply str (repeat 40 "-")))
    (let [advanced-results (run-tests 'c51cc.parser-advanced-test)]
      (println (str "Расширенные тесты: " (:pass advanced-results) " успешных, " 
                   (:fail advanced-results) " неудачных, " 
                   (:error advanced-results) " ошибок")))
    
    ;; Запуск C51-специфичных тестов
    (println "\n3. C51-СПЕЦИФИЧНЫЕ ТЕСТЫ")
    (println (apply str (repeat 40 "-")))
    (let [c51-results (run-tests 'c51cc.parser-c51-specific-test)]
      (println (str "C51-специфичные тесты: " (:pass c51-results) " успешных, " 
                   (:fail c51-results) " неудачных, " 
                   (:error c51-results) " ошибок")))
    
    ;; Анализ возможностей
    (println "\n4. АНАЛИЗ ВОЗМОЖНОСТЕЙ ПАРСЕРА")
    (println (apply str (repeat 40 "-")))
    (test-parser-capabilities)
    
    ;; Проверка поддержки C51 функций
    (println "\n5. ПОДДЕРЖКА C51 ФУНКЦИЙ")
    (println (apply str (repeat 40 "-")))
    (test-c51-feature-support)
    
    ;; Бенчмарк производительности
    (println "\n6. ПРОИЗВОДИТЕЛЬНОСТЬ")
    (println (apply str (repeat 40 "-")))
    (benchmark-parser-performance)
    
    (let [end-time (System/currentTimeMillis)
          total-time (- end-time start-time)]
      
      (println "\n" (apply str (repeat 80 "=")))
      (println "                        ИТОГОВЫЙ ОТЧЕТ")
      (println (apply str (repeat 80 "=")))
      (println (str "Общее время тестирования: " total-time "мс"))
      (println (str "Дата тестирования: " (java.util.Date.)))
      (println "Статус: Тестирование завершено")
      (println (apply str (repeat 80 "="))))))

(defn run-specific-test-suite
  "Запуск конкретного набора тестов"
  [suite-name]
  (case suite-name
    "basic" (do
              (println "Запуск базовых тестов...")
              (run-tests 'c51cc.parser-test))
    "advanced" (do
                 (println "Запуск расширенных тестов...")
                 (run-tests 'c51cc.parser-advanced-test))
    "c51" (do
            (println "Запуск C51-специфичных тестов...")
            (run-tests 'c51cc.parser-c51-specific-test))
    "all" (generate-test-report)
    "capabilities" (test-parser-capabilities)
    "performance" (benchmark-parser-performance)
    (println (str "Неизвестный набор тестов: " suite-name))))

;; ============================================================================
;; ИНТЕРАКТИВНЫЕ ФУНКЦИИ ДЛЯ ОТЛАДКИ
;; ============================================================================

(defn interactive-parser-test
  "Интерактивное тестирование парсера"
  []
  (println "\n=== ИНТЕРАКТИВНОЕ ТЕСТИРОВАНИЕ ПАРСЕРА ===")
  (println "Введите код C51 для парсинга (или 'exit' для выхода):")
  
  (loop []
    (print "\nC51> ")
    (flush)
    (let [input (read-line)]
      (when (and input (not= input "exit"))
        (parse-and-analyze input "Пользовательский ввод")
        (recur)))))

(defn debug-parser-on-code
  "Отладка парсера на конкретном коде с детальным выводом"
  [code]
  (println "\n=== ОТЛАДКА ПАРСЕРА ===")
  (println (str "Исходный код: " code))
  
  (let [tokens (lexer/tokenize code)]
    (println "\nТокены:")
    (doseq [[i token] (map-indexed vector tokens)]
      (println (str "  " i ": " token)))
    
    (println "\nПарсинг...")
    (let [result (parser/parse tokens)]
      (if (:success result)
        (do
          (println "✓ Парсинг успешен")
          (println "\nAST:")
          (clojure.pprint/pprint (:ast result))
          (when (parser/validate-ast (:ast result))
            (println "\n✓ AST прошел валидацию")))
        (do
          (println "✗ Ошибка парсинга:")
          (println (str "  " (:error result)))
          (when (:position result)
            (println (str "  Позиция: " (:position result)))))))))

;; ============================================================================
;; ГЛАВНАЯ ФУНКЦИЯ
;; ============================================================================

(defn -main
  "Главная функция для запуска тестов"
  [& args]
  (if (empty? args)
    (generate-test-report)
    (let [command (first args)]
      (case command
        "all" (generate-test-report)
        "basic" (run-specific-test-suite "basic")
        "advanced" (run-specific-test-suite "advanced")
        "c51" (run-specific-test-suite "c51")
        "capabilities" (run-specific-test-suite "capabilities")
        "performance" (run-specific-test-suite "performance")
        "interactive" (interactive-parser-test)
        "debug" (if (second args)
                  (debug-parser-on-code (second args))
                  (println "Использование: debug \"код для отладки\""))
        (do
          (println "Доступные команды:")
          (println "  all         - Полный отчет (по умолчанию)")
          (println "  basic       - Базовые тесты")
          (println "  advanced    - Расширенные тесты")
          (println "  c51         - C51-специфичные тесты")
          (println "  capabilities - Анализ возможностей")
          (println "  performance - Бенчмарк производительности")
          (println "  interactive - Интерактивное тестирование")
          (println "  debug \"код\" - Отладка конкретного кода"))))))

;; Экспорт основных функций
(def ^:export generate-test-report generate-test-report)
(def ^:export test-parser-capabilities test-parser-capabilities)
(def ^:export benchmark-parser-performance benchmark-parser-performance)
(def ^:export interactive-parser-test interactive-parser-test)
(def ^:export debug-parser-on-code debug-parser-on-code) 