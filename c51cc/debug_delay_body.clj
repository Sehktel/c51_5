(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ТЕСТ: DELAY БЕЗ ТЕЛА ===")
(def delay-empty "void delay(unsigned int ms) using 1 { }")
(let [result (parser/parse (lexer/tokenize delay-empty))]
  (println "Результат:" (if (:success result) "✅ УСПЕХ" "❌ ОШИБКА")))

(println "\n=== ТЕСТ: DELAY С ПРОСТЫМ ТЕЛОМ ===")
(def delay-simple "void delay(unsigned int ms) using 1 { ms = ms + 1; }")
(let [result (parser/parse (lexer/tokenize delay-simple))]
  (println "Результат:" (if (:success result) "✅ УСПЕХ" "❌ ОШИБКА")))

(println "\n=== ТЕСТ: DELAY С ОБЪЯВЛЕНИЕМ ПЕРЕМЕННОЙ ===")
(def delay-var "void delay(unsigned int ms) using 1 { unsigned int i; }")
(let [result (parser/parse (lexer/tokenize delay-var))]
  (if (:success result)
    (println "✅ УСПЕХ")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result)))))

(println "\n=== ТЕСТ: DELAY С ЦИКЛОМ FOR ===")
(def delay-for "void delay(unsigned int ms) using 1 { for (int i = 0; i < ms; i++) { } }")
(let [result (parser/parse (lexer/tokenize delay-for))]
  (if (:success result)
    (println "✅ УСПЕХ")
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result))))) 