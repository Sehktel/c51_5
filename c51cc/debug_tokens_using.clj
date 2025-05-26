(require '[c51cc.lexer :as lexer])

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

(println "=== ОТЛАДКА ТОКЕНОВ ВОКРУГ ПОЗИЦИИ 144 ===")
(let [tokens (lexer/tokenize large-program)]
  (println "Общее количество токенов:" (count tokens))
  (println "\nТокены с 135 по 150:")
  (doseq [i (range 135 (min 151 (count tokens)))]
    (println (format "%3d: %s" i (nth tokens i))))) 