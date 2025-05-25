(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "🔍 ТЕСТИРОВАНИЕ ПАРСИНГА ФУНКЦИЙ")

(defn test-function-parsing [code description]
  (println (str "\n📝 " description))
  (println (str "Код: " code))
  (let [tokens (lexer/tokenize code)
        result (parser/parse tokens)]
    (println (str "Успех: " (:success result)))
    (if (:success result)
      (let [ast (:ast result)]
        (println (str "Объявлений: " (count (:declarations ast))))
        (when (> (count (:declarations ast)) 0)
          (let [first-decl (first (:declarations ast))]
            (println (str "Тип: " (:ast-type first-decl)))
            (when (= (:ast-type first-decl) :function-declaration)
              (println "✅ Это функция!")
              (println (str "  Имя: " (:name first-decl)))))))
      (println (str "❌ Ошибка: " (:error result))))))

;; Простые тесты
(test-function-parsing "void main() { }" "Пустая функция")
(test-function-parsing "int add(int a, int b) { return a + b; }" "Функция с параметрами") 