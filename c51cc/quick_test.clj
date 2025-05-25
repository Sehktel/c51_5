(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(defn test-parse 
  "Быстрое тестирование парсинга кода"
  [code description]
  (println (str "\n=== " description " ==="))
  (println (str "Код: " code))
  (let [tokens (lexer/tokenize code)
        result (parser/parse tokens)]
    (println (str "Токенов: " (count tokens)))
    (println (str "Успех: " (:success result)))
    (when (:success result)
      (let [ast (:ast result)]
        (println (str "AST тип: " (:ast-type ast)))
        (when (:declarations ast)
          (println (str "Объявлений: " (count (:declarations ast)))))
        (when (parser/validate-ast ast)
          (println "✓ AST валиден"))))
    (when (not (:success result))
      (println (str "Ошибка: " (:error result))))
    (:success result)))

(println "=== БЫСТРОЕ ТЕСТИРОВАНИЕ ПАРСЕРА C51 ===")

;; Базовые тесты
(test-parse "42" "Числовой литерал")
(test-parse "variable_name" "Идентификатор")
(test-parse "a + b * c" "Арифметическое выражение")

;; Объявления
(test-parse "int x;" "Простое объявление переменной")
(test-parse "int x = 42;" "Объявление с инициализацией")
(test-parse "unsigned char port = 0xFF;" "Беззнаковый тип")

;; Функции
(test-parse "void main() { }" "Пустая функция")
(test-parse "void main() { return; }" "Функция с return")
(test-parse "int add(int a, int b) { return a + b; }" "Функция с параметрами")

;; Управляющие конструкции
(test-parse "if (x > 0) return x;" "Условный оператор")
(test-parse "while (x > 0) x--;" "Цикл while")
(test-parse "for (i = 0; i < 10; i++) sum += i;" "Цикл for")

;; Вызовы функций
(test-parse "func()" "Вызов функции без аргументов")
(test-parse "func(42)" "Вызов функции с аргументом")
(test-parse "func(a, b, c)" "Вызов функции с несколькими аргументами")

;; Сложные выражения
(test-parse "(a + b) * (c - d)" "Выражение со скобками")
(test-parse "++x + y--" "Префиксные и постфиксные операторы")
(test-parse "a && b || c" "Логические операторы")

;; Специфичные для C51
(test-parse "P1 = 0xFF;" "Работа с портом")
(test-parse "P1 |= (1 << 3);" "Битовые операции с портом")

;; Комплексная программа
(test-parse "
void blink_led() {
  while (1) {
    P1 = 0xFF;
    delay(1000);
    P1 = 0x00;
    delay(1000);
  }
}" "Комплексная функция")

(println "\n=== ТЕСТИРОВАНИЕ ЗАВЕРШЕНО ===")

;; Подсчет успешных тестов
(def test-results (atom []))

(defn count-test 
  [code description]
  (let [result (test-parse code description)]
    (swap! test-results conj result)
    result))

(println "\n=== ПОДСЧЕТ РЕЗУЛЬТАТОВ ===")
(let [total-tests 15
      successful-tests (count (filter true? @test-results))]
  (println (str "Всего тестов: " total-tests))
  (println (str "Успешных: " successful-tests))
  (println (str "Процент успеха: " (int (* 100 (/ successful-tests total-tests))) "%"))) 