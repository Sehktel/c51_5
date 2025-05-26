(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(defn parse-string [input]
  "Парсит строку кода и возвращает результат"
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn detailed-test [description code]
  "Детальный тест с полной диагностикой"
  (println (str "\n=== " description " ==="))
  (println (str "Код: " code))
  
  ;; Сначала проверяем токенизацию
  (let [tokens (lexer/tokenize code)]
    (println (str "Токены (" (count tokens) "): "))
    (doseq [[i token] (map-indexed vector tokens)]
      (println (str "  " i ": " (:type token) " = " (:value token))))
    
    ;; Затем парсинг
    (let [result (parser/parse tokens)]
      (println "\nРезультат парсинга:")
      (if (:success result)
        (do
          (println "✓ УСПЕХ")
          (let [ast (:ast result)]
            (println (str "AST тип: " (:ast-type ast)))
            (when (:declarations ast)
              (println (str "Объявлений: " (count (:declarations ast))))
              (doseq [[i decl] (map-indexed vector (:declarations ast))]
                (println (str "  " i ": " (:ast-type decl)))
                (when (= (:ast-type decl) :function-declaration)
                  (println (str "    Имя: " (:function-name decl)))
                  (println (str "    Параметры: " (count (:parameters decl))))
                  (doseq [[j param] (map-indexed vector (:parameters decl))]
                    (println (str "      " j ": " (:type param) " " (:name param)))))))))
        (do
          (println "✗ ОШИБКА")
          (println (str "Сообщение: " (:error result)))
          (println (str "Позиция: " (:position result)))
          (when (< (:position result) (count tokens))
            (println (str "Токен на позиции: " (nth tokens (:position result))))))))))

;; Тесты простых конструкций (должны работать)
(detailed-test "Простая функция без параметров" "void main() { }")
(detailed-test "Простое объявление переменной" "int x;")

;; Тесты функций с параметрами (проблемные)
(detailed-test "Функция с одним параметром" "int add(int a) { return a; }")
(detailed-test "Функция с двумя параметрами" "int add(int a, int b) { return a + b; }")
(detailed-test "Функция с void параметром" "void func(void) { }")

;; Тесты C51 interrupt (должны работать)
(detailed-test "Interrupt функция" "void timer_isr(void) interrupt 1 { }")
(detailed-test "Interrupt с using" "void ext_int(void) interrupt 0 using 2 { }")

;; Тесты объявлений функций
(detailed-test "Объявление функции" "int add(int a, int b);")
(detailed-test "Объявление void функции" "void func();")

;; Дополнительные тесты
(detailed-test "Функция с телом и операторами" "void test() { int x = 5; x = x + 1; }")
(detailed-test "Функция с return" "int getValue() { return 42; }")

(println "\n=== ЗАВЕРШЕНИЕ ТЕСТОВ ===") 