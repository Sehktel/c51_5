(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

;; Точная последовательность из большой программы до функции delay
(def sequence-before-delay "
      sfr P0 = 0x80; sfr P1 = 0x90; sfr P2 = 0xA0; sfr P3 = 0xB0;
      sfr TCON = 0x88; sfr TMOD = 0x89; sfr TL0 = 0x8A; sfr TH0 = 0x8C;
      
      sbit LED1 = 0x90; sbit LED2 = 0x91; sbit BUTTON = 0xB0;
      
      unsigned char timer_count = 0;
      int adc_value = 0;
      
      void timer0_isr() interrupt 1 using 1 {
        timer_count++;
        if (timer_count >= 100) {
          LED1 = !LED1;
          timer_count = 0;
        }
      }
      
      void external_int0() interrupt 0 using 2 {
        if (BUTTON == 0) {
          LED2 = 1;
        } else {
          LED2 = 0;
        }
      }
      
      void serial_isr() interrupt 4 {
        adc_value = P1;
      }")

(println "=== ТЕСТ ПОСЛЕДОВАТЕЛЬНОСТИ ПЕРЕД DELAY ===")
(let [result (parser/parse (lexer/tokenize sequence-before-delay))]
  (if (:success result)
    (do
      (println "✅ УСПЕХ! Последовательность перед delay парсится")
      (println "Деклараций:" (count (:declarations (:ast result)))))
    (do
      (println "❌ ОШИБКА в последовательности перед delay!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result)))))

(println "\n=== ТЕСТ ПОСЛЕДОВАТЕЛЬНОСТИ + DELAY ===")
(def sequence-with-delay (str sequence-before-delay "\nvoid delay(unsigned int ms) using 1 { }"))
(let [result (parser/parse (lexer/tokenize sequence-with-delay))]
  (if (:success result)
    (do
      (println "✅ УСПЕХ! Последовательность + delay парсится")
      (println "Деклараций:" (count (:declarations (:ast result)))))
    (do
      (println "❌ ОШИБКА при добавлении delay!")
      (println "Позиция:" (:position result))
      (println "Ошибка:" (:error result))))) 