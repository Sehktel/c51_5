(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ТЕСТ ФУНКЦИИ DELAY В КОНТЕКСТЕ ===")

;; Тестируем функцию delay после других деклараций
(def context-program "
      sfr P0 = 0x80; 
      sbit LED1 = 0x90; 
      unsigned char timer_count = 0;
      void timer0_isr() interrupt 1 using 1 { }
      void delay(unsigned int ms) using 1 { }")

(let [tokens (lexer/tokenize context-program)
      result (parser/parse tokens)]
  (println "Код:" context-program)
  (println "Количество токенов:" (count tokens))
  (if (:success result)
    (do
      (println "✅ УСПЕХ!")
      (println "Количество деклараций:" (count (:declarations (:ast result))))
      (let [decl-types (map :ast-type (:declarations (:ast result)))]
        (println "Типы деклараций:" decl-types)))
    (do
      (println "❌ ОШИБКА!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result))
      
      (when (:position result)
        (let [pos (:position result)
              remaining (drop pos tokens)]
          (println "\nОставшиеся токены с позиции" pos ":")
          (doseq [[i token] (map-indexed vector (take 10 remaining))]
            (println (format "%3d: %s" (+ pos i) token))))))))

(println "\n=== ТЕСТ ТОЛЬКО ФУНКЦИИ DELAY ===")
(def delay-only "void delay(unsigned int ms) using 1 { }")
(let [result (parser/parse (lexer/tokenize delay-only))]
  (println "Код:" delay-only)
  (println "Результат:" (if (:success result) "✅ УСПЕХ" "❌ ОШИБКА"))) 