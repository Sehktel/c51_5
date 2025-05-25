(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "=== ТЕСТИРОВАНИЕ ПАРСИНГА ОПЕРАТОРОВ СДВИГА ===")

(defn test-parse-shift [code description]
  (println (str "\n--- " description " ---"))
  (println (str "Код: " code))
  (try
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
            (println "✓ AST валиден"))
          ;; Анализ выражения
          (when (and (:declarations ast) (> (count (:declarations ast)) 0))
            (let [first-decl (first (:declarations ast))]
              (when (:expression first-decl)
                (let [expr (:expression first-decl)]
                  (println (str "Выражение тип: " (:ast-type expr)))
                  (when (= (:ast-type expr) :binary-expression)
                    (println (str "Оператор: " (:operator expr))))
                  (when (= (:ast-type expr) :assignment-expression)
                    (println (str "Оператор присваивания: " (:operator expr))))))))))
      (when (not (:success result))
        (println (str "Ошибка: " (:error result))))
      (:success result))
    (catch Exception e
      (println (str "Исключение: " (.getMessage e)))
      false)))

;; Тестируем операторы сдвига
(test-parse-shift "result = 1 << 3;" "Сдвиг влево в присваивании")
(test-parse-shift "mask = 0xFF >> 4;" "Сдвиг вправо в присваивании")
(test-parse-shift "value <<= 2;" "Составной сдвиг влево")
(test-parse-shift "data >>= 1;" "Составной сдвиг вправо")

;; Сложные выражения с операторами сдвига
(test-parse-shift "result = (a + b) << 2;" "Сдвиг выражения в скобках")
(test-parse-shift "mask = value >> shift_count;" "Сдвиг с переменной")
(test-parse-shift "P1 |= (1 << bit_position);" "Битовая операция с портом")

;; Приоритет операторов
(test-parse-shift "result = a + b << 2;" "Приоритет сдвига и сложения")
(test-parse-shift "result = a << 2 + b;" "Приоритет сдвига и сложения (обратный)")
(test-parse-shift "result = a * b >> 2;" "Приоритет умножения и сдвига")

;; Цепочки операторов сдвига
(test-parse-shift "result = value << 2 >> 1;" "Цепочка сдвигов")

(println "\n=== ТЕСТИРОВАНИЕ ЗАВЕРШЕНО ===") 