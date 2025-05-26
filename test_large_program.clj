(require '[c51cc.lexer :as lexer])
(require '[c51cc.parser :as parser])

(def large-program "
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
      }
      
      void delay(unsigned int ms) using 1 {
        unsigned int i, j;
        for (i = 0; i < ms; i++) {
          for (j = 0; j < 1000; j++) {
            i = i + 1;
          }
        }
      }")

(println "=== ТЕСТ БОЛЬШОЙ ПРОГРАММЫ ===")
(let [tokens (lexer/tokenize large-program)
      result (parser/parse tokens)]
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