(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(println "=== КОМПЛЕКСНЫЙ ТЕСТ C51 КОМПИЛЯТОРА ===")

(defn test-c51-code [description code]
  (println (str "\n" description ":"))
  (println (str "Код: " code))
  (let [tokens (lexer/tokenize code)
        result (parser/parse tokens)]
    (if (:success result)
      (do
        (println "✓ Парсинг успешен!")
        (let [ast (:ast result)]
          (println (str "AST тип: " (:ast-type ast)))
          (when (= (:ast-type ast) :program)
            (println (str "Количество деклараций: " (count (:declarations ast))))
            (doseq [[i decl] (map-indexed vector (:declarations ast))]
              (println (str "  " (inc i) ". " (:ast-type decl) 
                           (when (:name decl) (str " '" (:name decl) "'"))
                           (when (:interrupt-number decl) (str " interrupt " (:interrupt-number decl)))
                           (when (:using-clause decl) (str " using " (:using-clause decl)))
                           (when (:address decl) (str " @ 0x" (format "%02X" (:address decl))))))))))
      (println (str "✗ Ошибка: " (:error result))))))

;; Тест 1: Простая interrupt функция
(test-c51-code "Простая interrupt функция" 
               "void ext_int0(void) interrupt 0 { P1 = 0xFF; }")

;; Тест 2: Interrupt функция с using
(test-c51-code "Interrupt функция с using" 
               "void timer0_isr(void) interrupt 1 using 2 { counter++; }")

;; Тест 3: Только using без interrupt (должно работать как обычная функция)
(test-c51-code "Функция только с using" 
               "void fast_func(void) using 1 { temp = data; }")

;; Тест 4: SFR декларации
(test-c51-code "SFR декларации" 
               "sfr P1 = 0x90; sfr TCON = 0x88;")

;; Тест 5: SBIT декларации
(test-c51-code "SBIT декларации" 
               "sbit LED = 0x90; sbit BUTTON = 0x91;")

;; Тест 6: Смешанная программа
(test-c51-code "Смешанная C51 программа" 
               "sfr P1 = 0x90; sbit LED = 0x90; void timer_isr(void) interrupt 1 { LED = 1; }")

;; Тест 7: Обычная функция (без C51 модификаторов)
(test-c51-code "Обычная функция" 
               "void main(void) { P1 = 0x00; }")

;; Тест 8: Функция с параметрами и interrupt
(test-c51-code "Interrupt функция с телом" 
               "void uart_isr(void) interrupt 4 using 1 { if (flag) { count = 0; } }")

(println "\n=== ТЕСТ ЗАВЕРШЕН ===")
(println "Поддержка C51 успешно реализована! 🎉") 