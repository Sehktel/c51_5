(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "🔍 ДИАГНОСТИКА ПАРСЕРА ПОСЛЕ ИЗМЕНЕНИЙ")
(println "=" (apply str (repeat 50 "=")))

(defn test-basic-parsing [code description]
  (println (str "\n📝 Тест: " description))
  (println (str "Код: " code))
  (try
    (let [tokens (lexer/tokenize code)
          result (parser/parse tokens)]
      (println (str "Токенов: " (count tokens)))
      (println (str "Успех: " (:success result)))
      (if (:success result)
        (let [ast (:ast result)]
          (println (str "AST тип: " (:ast-type ast)))
          (when (:declarations ast)
            (println (str "Объявлений: " (count (:declarations ast)))))
          (when (parser/validate-ast ast)
            (println "✅ AST валиден"))
          ;; Детальный анализ первого объявления
          (when (and (:declarations ast) (> (count (:declarations ast)) 0))
            (let [first-decl (first (:declarations ast))]
              (println (str "Первое объявление: " (:ast-type first-decl)))
              (when (= (:ast-type first-decl) :function-declaration)
                (println (str "  Имя функции: " (:name first-decl)))
                (println (str "  Тип возврата: " (:return-type first-decl)))
                (println (str "  Параметров: " (count (:parameters first-decl))))))))
        (println (str "❌ Ошибка: " (:error result))))
      (:success result))
    (catch Exception e
      (println (str "💥 Исключение: " (.getMessage e)))
      false)))

;; Тестируем базовую функциональность
(test-basic-parsing "42" "Простое число")
(test-basic-parsing "x" "Идентификатор")
(test-basic-parsing "a + b" "Простое выражение")
(test-basic-parsing "int x = 42;" "Объявление переменной")
(test-basic-parsing "void main() { return; }" "Простая функция")

;; Тестируем операторы сдвига (наши изменения)
(test-basic-parsing "1 << 3" "Оператор сдвига влево")
(test-basic-parsing "P1 |= (1 << 3);" "Битовая операция с портом")

;; Тестируем проблемные случаи из тестов
(test-basic-parsing "int add(int a, int b) { return a + b; }" "Функция с параметрами")

(println "\n🎯 ВЫВОДЫ:")
(println "Если базовые тесты проходят, то проблема в тестовом файле")
(println "Если не проходят, то наши изменения сломали парсер") 