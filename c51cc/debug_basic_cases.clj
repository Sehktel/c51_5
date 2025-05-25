(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])
(require '[clojure.pprint :as pp])

(defn debug-parse [input description]
  (println "=== " description " ===")
  (println "Вход:" input)
  (let [tokens (lexer/tokenize input)]
    (println "Токены:" (count tokens))
    (let [result (parser/parse tokens)]
      (println "Успешен:" (:success result))
      (when (:success result)
        (let [ast (:ast result)]
          (println "Деклараций:" (count (:declarations ast)))
          (when (seq (:declarations ast))
            (println "Первая декларация:" (:ast-type (first (:declarations ast)))))))
      (when-not (:success result)
        (println "Ошибка:" (:error result)))
      (println))))

;; Тестируем базовые случаи
(debug-parse "42" "Простое число")
(debug-parse "x" "Простой идентификатор") 
(debug-parse "x + y" "Простое сложение")
(debug-parse "func()" "Вызов функции без аргументов")
(debug-parse "func(42)" "Вызов функции с аргументом")

;; Тестируем операторы
(debug-parse "return;" "Return без выражения")
(debug-parse "return 42;" "Return с выражением")

;; Тестируем объявления
(debug-parse "int x;" "Объявление переменной")
(debug-parse "void main() { }" "Простая функция")

;; Тестируем блоки
(debug-parse "{ return; }" "Простой блок") 