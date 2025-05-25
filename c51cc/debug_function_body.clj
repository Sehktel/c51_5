(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(defn parse-string [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn parse-successful? [result]
  (:success result))

(defn get-ast [result]
  (:ast result))

(println "🔍 АНАЛИЗ ПАРСИНГА ТЕЛ ФУНКЦИЙ")
(println "==============================")

;; Тест 1: Простая функция
(println "\n1. Простая функция:")
(let [result (parse-string "void test() { }")]
  (println (str "Ввод: 'void test() { }'"))
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (let [ast (get-ast result)]
      (println (str "AST: " ast))
      (let [func-decl (first (:declarations ast))]
        (println (str "Функция: " func-decl))
        (println (str "Тело функции: " (:body func-decl)))
        (println (str "Тип тела: " (:ast-type (:body func-decl))))
        (when (:body func-decl)
          (println (str "Операторы в теле: " (:statements (:body func-decl)))))))
    (println (str "Ошибка: " (:error result)))))

;; Тест 2: Функция с операторами
(println "\n2. Функция с операторами:")
(let [result (parse-string "void test() { int x = 10; return x; }")]
  (println (str "Ввод: 'void test() { int x = 10; return x; }'"))
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (let [ast (get-ast result)]
      (println (str "AST: " ast))
      (let [func-decl (first (:declarations ast))]
        (println (str "Функция: " func-decl))
        (println (str "Тело функции: " (:body func-decl)))
        (println (str "Тип тела: " (:ast-type (:body func-decl))))
        (when (:body func-decl)
          (println (str "Операторы в теле: " (:statements (:body func-decl))))
          (println (str "Количество операторов: " (count (:statements (:body func-decl))))))))
    (println (str "Ошибка: " (:error result)))))

;; Тест 3: Проверим токены
(println "\n3. Анализ токенов:")
(let [tokens (lexer/tokenize "void test() { int x = 10; return x; }")]
  (println (str "Токены: " tokens))
  (println (str "Количество токенов: " (count tokens))))

;; Тест 4: Пошаговый анализ
(println "\n4. Пошаговый анализ:")
(let [code "void test() { return 42; }"
      tokens (lexer/tokenize code)]
  (println (str "Код: " code))
  (println (str "Токены: " tokens))
  (let [result (parser/parse tokens)]
    (println (str "Результат: " result))
    (when (:success result)
      (let [ast (:ast result)
            func-decl (first (:declarations ast))]
        (println (str "Функция полностью: " func-decl))))))

(println "\n✅ Анализ завершен") 