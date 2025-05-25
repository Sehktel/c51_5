(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(defn test-error-handling
  "Тестирование обработки ошибок парсера"
  [code description should-fail]
  (println (str "\n=== " description " ==="))
  (println (str "Код: " code))
  (println (str "Должен провалиться: " should-fail))
  (try
    (let [tokens (lexer/tokenize code)
          result (parser/parse tokens)]
      (println (str "Токенов: " (count tokens)))
      (println (str "Успех парсинга: " (:success result)))
      (if (:success result)
        (do
          (println "✓ Парсинг прошел успешно")
          (when should-fail
            (println "⚠️  ВНИМАНИЕ: Ожидалась ошибка, но парсинг прошел!")))
        (do
          (println (str "✗ Ошибка: " (:error result)))
          (when (not should-fail)
            (println "⚠️  ВНИМАНИЕ: Ожидался успех, но произошла ошибка!"))))
      (if should-fail
        (not (:success result))  ; Успех теста = провал парсинга
        (:success result)))      ; Успех теста = успех парсинга
    (catch Exception e
      (println (str "Исключение: " (.getMessage e)))
      should-fail))) ; Если ожидали ошибку, то исключение = успех теста

(println "=== ТЕСТИРОВАНИЕ ОБРАБОТКИ ОШИБОК ПАРСЕРА C51 ===")

;; Синтаксические ошибки
(test-error-handling "int x = ;" "Неполное присваивание" true)
(test-error-handling "int = 42;" "Отсутствует имя переменной" true)
(test-error-handling "= x 42;" "Неправильный порядок токенов" true)
(test-error-handling "int x 42;" "Отсутствует оператор присваивания" true)

;; Ошибки в скобках
(test-error-handling "if (x > 0 { return; }" "Несбалансированные скобки в if" true)
(test-error-handling "func(a, b,)" "Лишняя запятая в аргументах" true)
(test-error-handling "func(a b c)" "Отсутствуют запятые в аргументах" true)
(test-error-handling "array[1][2" "Незакрытые квадратные скобки" true)

;; Ошибки в выражениях
(test-error-handling "x + + y" "Двойной оператор" true)
(test-error-handling "x + * y" "Неправильная последовательность операторов" true)
(test-error-handling "(x + y" "Незакрытая скобка в выражении" true)
(test-error-handling "x + y)" "Лишняя закрывающая скобка" true)

;; Ошибки в функциях
(test-error-handling "void func( { }" "Неполный список параметров" true)
(test-error-handling "void func() return; }" "Отсутствует открывающая скобка тела" true)
(test-error-handling "void func() { return" "Отсутствует точка с запятой" true)
(test-error-handling "func() { }" "Отсутствует тип возвращаемого значения" true)

;; Ошибки в циклах
(test-error-handling "for (;;; { }" "Лишняя точка с запятой в for" true)
(test-error-handling "while { }" "Отсутствует условие в while" true)
(test-error-handling "for (i = 0 i < 10; i++) { }" "Отсутствует точка с запятой в for" true)

;; Ошибки типов
(test-error-handling "unknown_type x;" "Неизвестный тип" true)
(test-error-handling "int int x;" "Дублирование типа" true)
(test-error-handling "void x = 42;" "Присваивание void переменной" true)

;; Правильные конструкции (не должны провалиться)
(test-error-handling "int x = 42;" "Правильное объявление" false)
(test-error-handling "void func() { return; }" "Правильная функция" false)
(test-error-handling "if (x > 0) return x;" "Правильный if" false)
(test-error-handling "for (i = 0; i < 10; i++) sum += i;" "Правильный for" false)

(println "\n=== ДЕТАЛЬНЫЙ АНАЛИЗ ТОКЕНИЗАЦИИ ===")

(defn analyze-tokenization [code]
  (println (str "\nАнализ кода: " code))
  (try
    (let [tokens (lexer/tokenize code)]
      (println (str "Количество токенов: " (count tokens)))
      (doseq [[i token] (map-indexed vector tokens)]
        (println (str "  " i ": " token)))
      tokens)
    (catch Exception e
      (println (str "Ошибка токенизации: " (.getMessage e)))
      [])))

;; Анализируем проблемные случаи
(analyze-tokenization "int x = ;")
(analyze-tokenization "if (x > 0 { return; }")
(analyze-tokenization "unknown_type variable;")

(println "\n=== ТЕСТ ГРАНИЧНЫХ СЛУЧАЕВ ===")

;; Пустой код
(test-error-handling "" "Пустой код" true)

;; Только пробелы
(test-error-handling "   \n\t  " "Только пробелы" true)

;; Только комментарии
(test-error-handling "// это комментарий" "Только комментарий" true)

;; Очень длинная строка
(def long-string (apply str (repeat 1000 "a")))
(test-error-handling (str "int " long-string " = 42;") "Очень длинный идентификатор" false)

;; Глубокая вложенность
(def deep-expression (str "(" (apply str (repeat 100 "(x+")) (apply str (repeat 100 ")"))))
(test-error-handling deep-expression "Глубокая вложенность выражений" false)

(println "\n=== ТЕСТИРОВАНИЕ ОШИБОК ЗАВЕРШЕНО ===")

;; Статистика
(def error-tests-passed (atom 0))
(def total-error-tests (atom 0))

(defn count-error-test [result]
  (swap! total-error-tests inc)
  (when result (swap! error-tests-passed inc)))

(println "\n=== СТАТИСТИКА ТЕСТОВ ОШИБОК ===")
(println (str "Всего тестов ошибок: " @total-error-tests))
(println (str "Прошедших тестов: " @error-tests-passed))
(println (str "Процент успеха: " 
              (if (> @total-error-tests 0)
                (int (* 100 (/ @error-tests-passed @total-error-tests)))
                0) "%")) 