(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== ПОШАГОВЫЙ ТЕСТ БОЛЬШОЙ ПРОГРАММЫ ===")

;; Тестируем по частям
(def part1 "sfr P0 = 0x80; sfr P1 = 0x90;")
(def part2 "sbit LED1 = 0x90; sbit LED2 = 0x91;")
(def part3 "unsigned char timer_count = 0;")
(def part4 "void timer0_isr() interrupt 1 using 1 { }")

(defn test-part [name code]
  (println (str "\n=== " name " ==="))
  (println "Код:" code)
  (let [result (parser/parse (lexer/tokenize code))]
    (if (:success result)
      (do
        (println "✅ УСПЕХ!")
        (println "Деклараций:" (count (:declarations (:ast result)))))
      (do
        (println "❌ ОШИБКА!")
        (println "Позиция:" (:position result))
        (println "Ошибка:" (:error result))))))

(test-part "SFR декларации" part1)
(test-part "SBIT декларации" part2)
(test-part "Переменная unsigned char" part3)
(test-part "Interrupt функция" part4)

;; Тестируем комбинации
(test-part "SFR + SBIT" (str part1 " " part2))
(test-part "SFR + SBIT + переменная" (str part1 " " part2 " " part3))
(test-part "Все вместе" (str part1 " " part2 " " part3 " " part4)) 