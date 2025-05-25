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

(println "🔍 ОТЛАДКА СЛОЖНОЙ ПРОГРАММЫ")
(println "============================")

;; Тест проблемной программы из теста
(let [code "int sum(int n) { int result = 0; for (int i = 1; i <= n; i++) { if (i % 2 == 0) result += i; } return result; }"
      tokens (lexer/tokenize code)]
  (println (str "Сложная программа: " code))
  (println (str "Количество токенов: " (count tokens)))
  
  (let [result (parse-string code)]
    (println (str "Успех: " (parse-successful? result)))
    (if (parse-successful? result)
      (let [ast (get-ast result)
            func-decl (first (:declarations ast))
            body (:body func-decl)]
        (println (str "Функция: " (:name func-decl)))
        (println (str "Операторов в теле: " (count (:statements body))))
        (doseq [i (range (count (:statements body)))]
          (let [stmt (nth (:statements body) i)]
            (println (str "  Оператор " (inc i) ": " (:ast-type stmt))))))
      (println (str "Ошибка: " (get-error result))))))

;; Упрощенные версии для тестирования
(println "\n" (str "=" 40))
(println "УПРОЩЕННЫЕ ВЕРСИИ:")

;; Тест 1: Простой for без объявления переменной
(println "\n1. Простой for:")
(let [result (parse-string "void test() { for (i = 0; i < 10; i++) sum = sum + i; }")]
  (println (str "Успех: " (parse-successful? result)))
  (if (not (parse-successful? result))
    (println (str "Ошибка: " (get-error result)))))

;; Тест 2: Модуло оператор
(println "\n2. Модуло оператор:")
(let [result (parse-string "void test() { x = i % 2; }")]
  (println (str "Успех: " (parse-successful? result)))
  (if (not (parse-successful? result))
    (println (str "Ошибка: " (get-error result)))))

;; Тест 3: Составное присваивание
(println "\n3. Составное присваивание:")
(let [result (parse-string "void test() { result += i; }")]
  (println (str "Успех: " (parse-successful? result)))
  (if (not (parse-successful? result))
    (println (str "Ошибка: " (get-error result)))))

(println "\n✅ Отладка завершена") 