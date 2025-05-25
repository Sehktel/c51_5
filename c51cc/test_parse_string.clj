(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

;; Копируем функции из тестов
(defn parse-string 
  "Вспомогательная функция для парсинга строки кода"
  [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn parse-successful? 
  "Проверяет, был ли парсинг успешным"
  [result]
  (:success result))

(defn get-ast 
  "Извлекает AST из результата парсинга"
  [result]
  (:ast result))

(println "🔍 ТЕСТИРОВАНИЕ PARSE-STRING")

(let [result (parse-string "int add(int a, int b) { return a + b; }")]
  (println (str "Успех: " (parse-successful? result)))
  (when (parse-successful? result)
    (let [ast (get-ast result)]
      (println (str "AST тип: " (:ast-type ast)))
      (println (str "Объявлений: " (count (:declarations ast))))
      (let [func-decl (first (:declarations ast))]
        (println (str "func-decl: " func-decl))
        (println (str "func-decl nil?: " (nil? func-decl)))
        (when func-decl
          (println (str "func-decl тип: " (:ast-type func-decl)))
          (println (str "func-decl ключи: " (keys func-decl)))))))) 