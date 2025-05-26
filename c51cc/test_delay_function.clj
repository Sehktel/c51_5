(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ТЕСТ ФУНКЦИИ DELAY С USING ===")

; Тестируем проблемную функцию из теста
(def delay-code "void delay(unsigned int ms) using 1 { }")

(let [tokens (lexer/tokenize delay-code)]
  (println "Код:" delay-code)
  (println "Токены:")
  (doseq [[i token] (map-indexed vector tokens)]
    (println (format "%2d: %s" i token)))
  
  (println "\nПарсинг:")
  (let [result (parser/parse tokens)]
    (if (:success result)
      (do
        (println "✅ УСПЕХ!")
        (println "AST:" (:ast result))
        (let [func-decl (first (:declarations (:ast result)))]
          (println "Тип узла:" (:ast-type func-decl))
          (println "Имя функции:" (:name func-decl))
          (when (:using-clause func-decl)
            (println "Using clause:" (:using-clause func-decl)))))
      (do
        (println "❌ ОШИБКА!")
        (println "Позиция:" (:position result))
        (println "Ошибка:" (:error result))
        
        (when (:position result)
          (let [pos (:position result)
                remaining (drop pos tokens)]
            (println "\nОставшиеся токены с позиции" pos ":")
            (doseq [[i token] (map-indexed vector (take 5 remaining))]
              (println (format "%2d: %s" (+ pos i) token)))))))))

(println "\n=== ТЕСТ ПРОСТОЙ ФУНКЦИИ БЕЗ USING ===")
(def simple-code "void func() { }")
(let [result (parser/parse (lexer/tokenize simple-code))]
  (println "Код:" simple-code)
  (println "Результат:" (if (:success result) "✅ УСПЕХ" "❌ ОШИБКА")))

(println "\n=== ТЕСТ ФУНКЦИИ С INTERRUPT ===")
(def interrupt-code "void isr() interrupt 1 using 2 { }")
(let [result (parser/parse (lexer/tokenize interrupt-code))]
  (println "Код:" interrupt-code)
  (println "Результат:" (if (:success result) "✅ УСПЕХ" "❌ ОШИБКА"))) 