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

(println "🔍 ТЕСТИРОВАНИЕ ПАРСЕРА НА ЧИСТОМ КОДЕ")
(println "======================================")

;; Тест 1: Многострочная программа БЕЗ комментариев
(println "\n1. Многострочная программа:")
(let [code "unsigned char led_state = 0;

void delay(unsigned int ms) {
  for (unsigned int i = 0; i < ms; i++) {
  }
}

void main() {
  while (1) {
    led_state = !led_state;
    delay(1000);
  }
}"
      result (parse-string code)]
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (let [ast (get-ast result)]
      (println (str "Количество деклараций: " (count (:declarations ast))))
      (doseq [i (range (count (:declarations ast)))]
        (let [decl (nth (:declarations ast) i)]
          (println (str "Декларация " (inc i) ": " (:ast-type decl) 
                       (when (= (:ast-type decl) :function-declaration)
                         (str " - " (:name decl))))))))
    (println (str "Ошибка: " (get-error result)))))

;; Тест 2: Простая программа с двумя декларациями
(println "\n2. Простая программа с двумя декларациями:")
(let [code "int x = 10; void main() { x = x + 1; }"
      result (parse-string code)]
  (println (str "Успех: " (parse-successful? result)))
  (if (parse-successful? result)
    (let [ast (get-ast result)]
      (println (str "Количество деклараций: " (count (:declarations ast))))
      (doseq [i (range (count (:declarations ast)))]
        (let [decl (nth (:declarations ast) i)]
          (println (str "Декларация " (inc i) ": " (:ast-type decl)
                       (cond 
                         (= (:ast-type decl) :function-declaration) (str " - функция " (:name decl))
                         (= (:ast-type decl) :variable-declaration) (str " - переменная " (:name decl))
                         :else ""))))))
    (println (str "Ошибка: " (get-error result)))))

;; Тест 3: Функция с операторами
(println "\n3. Функция с операторами:")
(let [code "void test() { int x = 10; if (x > 5) return x; }"
      result (parse-string code)]
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
    (println (str "Ошибка: " (get-error result)))))

(println "\n✅ Тестирование завершено") 