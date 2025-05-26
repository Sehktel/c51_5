(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(def test-code "void delay(unsigned int ms) using 1 { }")

(println "=== ОТЛАДКА ФУНКЦИИ DELAY ===")
(let [tokens (lexer/tokenize test-code)]
  (println "Токены:")
  (doseq [[i token] (map-indexed vector tokens)]
    (println (format "%2d: %s" i token)))
  
  (println "\nПарсинг:")
  (let [result (parser/parse tokens)]
    (println "Результат:" result)
    
    (when-not (:success result)
      (println "\nОШИБКА:")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result))
      
      (when (:position result)
        (let [pos (:position result)
              remaining (drop pos tokens)]
          (println "\nОставшиеся токены с позиции" pos ":")
          (doseq [[i token] (map-indexed vector (take 5 remaining))]
            (println (format "%2d: %s" (+ pos i) token)))))))) 