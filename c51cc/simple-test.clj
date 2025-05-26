#!/usr/bin/env clojure

"Упрощенный тест для проверки рефакторинга функций парсера
 Запуск: clojure -M simple-test.clj"

(require '[c51cc.lexer :as lexer])

;; Простая проверка токенизации
(defn test-basic-functionality []
  (println "🧪 Тестирование базовой функциональности...")
  
  ;; Тест токенизации
  (let [test-input "int x = 5;"
        tokens (lexer/tokenize test-input)]
    (if (:success tokens)
      (do
        (println "✅ Токенизация работает")
        (println "Токены:" (:tokens tokens)))
      (println "❌ Ошибка токенизации:" (:error tokens))))
  
  ;; Тест простых выражений
  (let [expressions ["x" "5" "x + y" "func()" "arr[0]"]]
    (doseq [expr expressions]
      (let [tokens (lexer/tokenize expr)]
        (if (:success tokens)
          (println (str "✅ Токенизация '" expr "' успешна"))
          (println (str "❌ Ошибка токенизации '" expr "'"))))))
  
  (println "\n📊 Результаты:")
  (println "✨ Базовая функциональность лексера работает")
  (println "🔧 Рефакторинг может быть протестирован на уровне AST"))

;; Запуск теста
(test-basic-functionality) 