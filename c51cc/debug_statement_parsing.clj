(ns debug-statement-parsing
  (:require [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

(defn parse-string [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn -main []
  (println "=== АНАЛИЗ ПАРСИНГА ОПЕРАТОРОВ ===")
  
  (println "\n1. Блок операторов: { int x = 10; return x; }")
  (let [result (parse-string "{ int x = 10; return x; }")]
    (println "Success?:" (:success result))
    (println "Error:" (:error result)))
  
  (println "\n2. Цикл for: for (i = 0; i < 10; i++) sum += i;")
  (let [result (parse-string "for (i = 0; i < 10; i++) sum += i;")]
    (println "Success?:" (:success result))
    (println "Error:" (:error result)))
  
  (println "\n3. Корректная функция с блоком:")
  (let [result (parse-string "void test() { int x = 10; return x; }")]
    (println "Success?:" (:success result))
    (println "Error:" (:error result)))
  
  (println "\n4. Корректная функция с циклом:")
  (let [result (parse-string "void test() { for (i = 0; i < 10; i++) sum += i; }")]
    (println "Success?:" (:success result))
    (println "Error:" (:error result))))

(-main) 