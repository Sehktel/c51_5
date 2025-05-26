(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(println "=== ПРОСТОЙ ТЕСТ ИСПРАВЛЕНИЯ ===")

;; Тест простой функции
(let [code "void main() { }"
      tokens (lexer/tokenize code)]
  (println "\nТест: void main() { }")
  (println "Токены:" (count tokens))
  (try
    (let [result (parser/parse tokens)]
      (if (:success result)
        (println "✓ УСПЕХ!")
        (println "✗ ОШИБКА:" (:error result))))
    (catch Exception e
      (println "✗ ИСКЛЮЧЕНИЕ:" (.getMessage e)))))

;; Тест функции с параметрами
(let [code "int add(int a, int b) { return a + b; }"
      tokens (lexer/tokenize code)]
  (println "\nТест: int add(int a, int b) { return a + b; }")
  (println "Токены:" (count tokens))
  (try
    (let [result (parser/parse tokens)]
      (if (:success result)
        (println "✓ УСПЕХ!")
        (println "✗ ОШИБКА:" (:error result))))
    (catch Exception e
      (println "✗ ИСКЛЮЧЕНИЕ:" (.getMessage e)))))

(println "\n=== КОНЕЦ ТЕСТА ===") 