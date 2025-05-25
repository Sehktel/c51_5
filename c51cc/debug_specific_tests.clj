(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(defn parse-string [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn parse-successful? [result]
  (:success result))

(defn get-ast [result]
  (:ast result))

(defn get-error [result]
  (:error result))

(println "🔍 ТЕСТИРОВАНИЕ ПРОБЛЕМНЫХ СЛУЧАЕВ")
(println "==================================")

;; Проблема 1: Блок операторов как декларация
(println "\n1. Блок операторов:")
(let [result (parse-string "{ int x = 10; return x; }")]
  (println (str "Ввод: '{ int x = 10; return x; }'"))
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (let [ast (get-ast result)]
      (println (str "AST тип: " (:ast-type ast)))
      (println (str "Количество деклараций: " (count (:declarations ast))))
      (when (> (count (:declarations ast)) 0)
        (let [first-decl (first (:declarations ast))]
          (println (str "Первая декларация тип: " (:ast-type first-decl)))
          (println (str "Первая декларация: " first-decl)))))
    (println (str "Ошибка: " (get-error result)))))

;; Проблема 2: Условный оператор if
(println "\n2. Условный оператор if:")
(let [result (parse-string "if (x > 0) return x;")]
  (println (str "Ввод: 'if (x > 0) return x;'"))
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (let [ast (get-ast result)]
      (println (str "AST тип: " (:ast-type ast)))
      (println (str "Количество деклараций: " (count (:declarations ast))))
      (when (> (count (:declarations ast)) 0)
        (let [first-decl (first (:declarations ast))]
          (println (str "Первая декларация тип: " (:ast-type first-decl)))
          (println (str "Первая декларация: " first-decl)))))
    (println (str "Ошибка: " (get-error result)))))

;; Проблема 3: Цикл while
(println "\n3. Цикл while:")
(let [result (parse-string "while (x > 0) x--;")]
  (println (str "Ввод: 'while (x > 0) x--;'"))
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (let [ast (get-ast result)]
      (println (str "AST тип: " (:ast-type ast)))
      (println (str "Количество деклараций: " (count (:declarations ast))))
      (when (> (count (:declarations ast)) 0)
        (let [first-decl (first (:declarations ast))]
          (println (str "Первая декларация тип: " (:ast-type first-decl)))
          (println (str "Первая декларация: " first-decl)))))
    (println (str "Ошибка: " (get-error result)))))

;; Проблема 4: Цикл for
(println "\n4. Цикл for:")
(let [result (parse-string "for (i = 0; i < 10; i++) sum += i;")]
  (println (str "Ввод: 'for (i = 0; i < 10; i++) sum += i;'"))
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (let [ast (get-ast result)]
      (println (str "AST тип: " (:ast-type ast)))
      (println (str "Количество деклараций: " (count (:declarations ast))))
      (when (> (count (:declarations ast)) 0)
        (let [first-decl (first (:declarations ast))]
          (println (str "Первая декларация тип: " (:ast-type first-decl)))
          (println (str "Первая декларация: " first-decl)))))
    (println (str "Ошибка: " (get-error result)))))

;; Проблема 5: Ошибки парсинга
(println "\n5. Тест ошибок:")
(let [result (parse-string "func(")]
  (println (str "Ввод: 'func('"))
  (println (str "Успех: " (parse-successful? result)))
  (if (not (parse-successful? result))
    (println (str "Ошибка: " (get-error result)))
    (println "Неожиданно успешно!")))

(let [result (parse-string "int + 42")]
  (println (str "Ввод: 'int + 42'"))
  (println (str "Успех: " (parse-successful? result)))
  (if (not (parse-successful? result))
    (println (str "Ошибка: " (get-error result)))
    (println "Неожиданно успешно!")))

(println "\n✅ Тестирование завершено") 