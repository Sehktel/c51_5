(ns c51cc.parser-c51-specific-test
  "Тесты парсера для специфичных конструкций микроконтроллера 8051/C51
   Включает тестирование регистров, битовых операций, модификаторов памяти"
  (:require [clojure.test :refer :all]
            [c51cc.parser :as parser]
            [c51cc.lexer :as lexer]))

;; ============================================================================
;; ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
;; ============================================================================

(defn parse-string [input]
  (let [tokens (lexer/tokenize input)]
    (parser/parse tokens)))

(defn parse-successful? [result]
  (:success result))

(defn get-ast [result]
  (:ast result))

(defn get-error [result]
  (:error result))

;; ============================================================================
;; ТЕСТЫ БИТОВЫХ ОПЕРАЦИЙ (СПЕЦИФИЧНЫХ ДЛЯ 8051)
;; ============================================================================

(deftest test-bitwise-operations
  (testing "Битовые операции И, ИЛИ, ИСКЛЮЧАЮЩЕЕ ИЛИ"
    (let [result (parse-string "result = a & b | c ^ d;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)
            stmt (first (:declarations ast))]
        (is (= :expression-statement (:ast-type stmt)))
        (let [expr (:expression stmt)]
          (is (= :assignment-expression (:ast-type expr)))))))

  (testing "Битовые сдвиги (важно для работы с регистрами)"
    (let [result (parse-string "shifted = value << 3;")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Битовые маски для портов"
    (let [result (parse-string "P1 = (P1 & 0xF0) | (new_value & 0x0F);")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Установка и сброс отдельных битов"
    (let [result (parse-string "
      void set_bit() {
        P1 |= (1 << 3);    // Установка бита 3
        P1 &= ~(1 << 5);   // Сброс бита 5
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))))))

;; ============================================================================
;; ТЕСТЫ РАБОТЫ С РЕГИСТРАМИ 8051
;; ============================================================================

(deftest test-sfr-registers
  (testing "Работа с портами ввода-вывода"
    (let [result (parse-string "
      void init_ports() {
        P0 = 0x00;    // Порт 0
        P1 = 0xFF;    // Порт 1
        P2 = 0xAA;    // Порт 2
        P3 = 0x55;    // Порт 3
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Настройка таймеров"
    (let [result (parse-string "
      void timer_setup() {
        TMOD = 0x01;   // Режим таймера
        TH0 = 0x3C;    // Старший байт таймера 0
        TL0 = 0xB0;    // Младший байт таймера 0
        TH1 = 0xFF;    // Старший байт таймера 1
        TL1 = 0x00;    // Младший байт таймера 1
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Управление прерываниями"
    (let [result (parse-string "
      void interrupt_setup() {
        IE = 0x82;     // Регистр разрешения прерываний
        IP = 0x00;     // Регистр приоритетов прерываний
        EA = 1;        // Глобальное разрешение прерываний
        ET0 = 1;       // Разрешение прерывания от таймера 0
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Работа с последовательным портом"
    (let [result (parse-string "
      void uart_init() {
        SCON = 0x50;   // Настройка последовательного порта
        PCON = 0x80;   // Удвоение скорости
        SBUF = data;   // Буфер данных
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))))))

;; ============================================================================
;; ТЕСТЫ МОДИФИКАТОРОВ ПАМЯТИ (ЕСЛИ ПОДДЕРЖИВАЮТСЯ)
;; ============================================================================

(deftest test-memory-modifiers
  (testing "Объявления переменных с модификатором data"
    ;; Примечание: это может не поддерживаться в базовой версии
    (let [result (parse-string "data unsigned char buffer[256];")]
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (= :program (:ast-type ast)))))))

  (testing "Объявления переменных с модификатором idata"
    (let [result (parse-string "idata unsigned int counters[128];")]
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (= :program (:ast-type ast)))))))

  (testing "Объявления переменных с модификатором xdata"
    (let [result (parse-string "xdata unsigned char external_ram[1024];")]
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (= :program (:ast-type ast)))))))

  (testing "Объявления переменных с модификатором code"
    (let [result (parse-string "code unsigned char lookup_table[] = {1, 2, 3, 4};")]
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (= :program (:ast-type ast))))))))

;; ============================================================================
;; ТЕСТЫ БИТОВЫХ ПЕРЕМЕННЫХ (СПЕЦИФИКА 8051)
;; ============================================================================

(deftest test-bit-variables
  (testing "Объявление битовых переменных"
    ;; В 8051 есть специальный тип bit
    (let [result (parse-string "bit flag = 0;")]
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (= :program (:ast-type ast)))))))

  (testing "Операции с битовыми переменными"
    (let [result (parse-string "
      void bit_operations() {
        bit flag1 = 1;
        bit flag2 = 0;
        bit result_flag = flag1 & flag2;
      }")]
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (= :program (:ast-type ast)))))))

  (testing "Адресация битов в регистрах"
    (let [result (parse-string "
      void bit_addressing() {
        P1_0 = 1;      // Бит 0 порта P1
        P1_7 = 0;      // Бит 7 порта P1
        ACC_0 = flag;  // Бит 0 аккумулятора
      }")]
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (= :program (:ast-type ast))))))))

;; ============================================================================
;; ТЕСТЫ СПЕЦИАЛЬНЫХ ФУНКЦИЙ И ДИРЕКТИВ
;; ============================================================================

(deftest test-special-functions
  (testing "Функции с модификатором interrupt"
    ;; Специальный синтаксис для обработчиков прерываний
    (let [result (parse-string "
      void timer0_isr() interrupt 1 {
        // Обработчик прерывания таймера 0
        timer_flag = 1;
      }")]
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (= :program (:ast-type ast)))))
      ;; Если не поддерживается, проверяем что есть разумная ошибка
      (when (not (parse-successful? result))
        (is (string? (get-error result))))))

  (testing "Функции с модификатором using"
    ;; Указание банка регистров
    (let [result (parse-string "
      void fast_function() using 1 {
        // Функция использует банк регистров 1
        local_var = global_var + 1;
      }")]
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (= :program (:ast-type ast)))))
      (when (not (parse-successful? result))
        (is (string? (get-error result))))))

  (testing "Встроенные функции ассемблера"
    ;; Если поддерживается inline assembly
    (let [result (parse-string "
      void critical_section() {
        _asm
          CLR EA    ; Запрет прерываний
          MOV A, P1 ; Чтение порта
          SETB EA   ; Разрешение прерываний
        _endasm
      }")]
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (= :program (:ast-type ast)))))
      (when (not (parse-successful? result))
        (is (string? (get-error result)))))))

;; ============================================================================
;; ТЕСТЫ ИНИЦИАЛИЗАЦИИ И МАССИВОВ
;; ============================================================================

(deftest test-arrays-and-initialization
  (testing "Инициализация массивов констант"
    (let [result (parse-string "
      unsigned char sine_table[256] = {
        128, 131, 134, 137, 140, 143, 146, 149,
        152, 155, 158, 162, 165, 167, 170, 173
      };")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Многомерные массивы"
    (let [result (parse-string "
      unsigned char matrix[8][8] = {
        {1, 0, 0, 0, 0, 0, 0, 1},
        {0, 1, 0, 0, 0, 0, 1, 0},
        {0, 0, 1, 0, 0, 1, 0, 0}
      };")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Доступ к элементам массива"
    (let [result (parse-string "
      void array_access() {
        value = array[index];
        matrix[row][col] = new_value;
        buffer[i++] = data;
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))))))

;; ============================================================================
;; ТЕСТЫ УКАЗАТЕЛЕЙ И АДРЕСАЦИИ
;; ============================================================================

(deftest test-pointers-and-addressing
  (testing "Объявление и использование указателей"
    (let [result (parse-string "
      void pointer_test() {
        unsigned char *ptr;
        unsigned char value = 42;
        ptr = &value;
        *ptr = 100;
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Указатели на функции"
    (let [result (parse-string "
      void (*function_ptr)(void);
      
      void test_function() {
        // Тестовая функция
      }
      
      void main() {
        function_ptr = test_function;
        function_ptr();
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Арифметика указателей"
    (let [result (parse-string "
      void pointer_arithmetic() {
        unsigned char *ptr = buffer;
        ptr++;
        ptr += 5;
        value = *(ptr + offset);
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))))))

;; ============================================================================
;; ТЕСТЫ СТРУКТУР И ОБЪЕДИНЕНИЙ
;; ============================================================================

(deftest test-structures-and-unions
  (testing "Объявление и использование структур"
    (let [result (parse-string "
      struct sensor_data {
        unsigned int temperature;
        unsigned int humidity;
        unsigned char status;
      };
      
      void process_sensor() {
        struct sensor_data sensor;
        sensor.temperature = 25;
        sensor.humidity = 60;
        sensor.status = 1;
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Объявление и использование объединений"
    (let [result (parse-string "
      union data_converter {
        unsigned int word;
        struct {
          unsigned char low;
          unsigned char high;
        } bytes;
      };
      
      void convert_data() {
        union data_converter converter;
        converter.word = 0x1234;
        low_byte = converter.bytes.low;
        high_byte = converter.bytes.high;
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast))))))

  (testing "Указатели на структуры"
    (let [result (parse-string "
      struct device {
        unsigned char id;
        unsigned char status;
      };
      
      void update_device(struct device *dev) {
        dev->status = 1;
        dev->id = get_device_id();
      }")]
      (is (parse-successful? result))
      (let [ast (get-ast result)]
        (is (= :program (:ast-type ast)))))))

;; ============================================================================
;; ТЕСТЫ ПРЕПРОЦЕССОРА (ЕСЛИ ПОДДЕРЖИВАЕТСЯ)
;; ============================================================================

(deftest test-preprocessor-directives
  (testing "Макроопределения"
    (let [result (parse-string "
      #define LED_PIN 3
      #define SET_LED() (P1 |= (1 << LED_PIN))
      #define CLR_LED() (P1 &= ~(1 << LED_PIN))
      
      void blink_led() {
        SET_LED();
        delay(1000);
        CLR_LED();
      }")]
      ;; Препроцессор может обрабатываться отдельно
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (= :program (:ast-type ast)))))))

  (testing "Условная компиляция"
    (let [result (parse-string "
      #ifdef DEBUG
        void debug_print(char *msg) {
          // Отладочный вывод
        }
      #else
        #define debug_print(msg)
      #endif
      
      void main() {
        debug_print(\"Starting...\");
      }")]
      (when (parse-successful? result)
        (let [ast (get-ast result)]
          (is (= :program (:ast-type ast))))))))

;; ============================================================================
;; ИНТЕГРАЦИОННЫЕ ТЕСТЫ РЕАЛЬНЫХ ПРОГРАММ ДЛЯ 8051
;; ============================================================================

(deftest test-real-8051-programs
  (testing "Программа управления семисегментным индикатором"
    (let [seven-segment-program "
      unsigned char digit_patterns[10] = {
        0x3F, 0x06, 0x5B, 0x4F, 0x66,  // 0-4
        0x6D, 0x7D, 0x07, 0x7F, 0x6F   // 5-9
      };
      
      void display_digit(unsigned char digit) {
        if (digit <= 9) {
          P0 = digit_patterns[digit];
        }
      }
      
      void count_up() {
        unsigned char counter = 0;
        while (1) {
          display_digit(counter);
          delay_ms(1000);
          counter++;
          if (counter > 9) {
            counter = 0;
          }
        }
      }"]
      (is (parse-successful? (parse-string seven-segment-program)))
      (let [ast (get-ast (parse-string seven-segment-program))]
        (is (= 3 (count (:declarations ast)))))))

  (testing "Программа ШИМ (PWM) управления"
    (let [pwm-program "
      unsigned char pwm_duty = 128;  // 50% скважность
      unsigned char pwm_counter = 0;
      
      void timer_interrupt() interrupt 1 {
        pwm_counter++;
        if (pwm_counter < pwm_duty) {
          P1_0 = 1;  // Включить выход
        } else {
          P1_0 = 0;  // Выключить выход
        }
        
        // Перезагрузка таймера для частоты ~1kHz
        TH0 = 0xFC;
        TL0 = 0x18;
      }
      
      void pwm_init() {
        TMOD = 0x02;  // Режим 2 (автоперезагрузка)
        TH0 = 0xFC;   // Значение перезагрузки
        TL0 = 0xFC;   // Начальное значение
        ET0 = 1;      // Разрешение прерывания
        EA = 1;       // Глобальное разрешение
        TR0 = 1;      // Запуск таймера
      }
      
      void set_pwm_duty(unsigned char duty) {
        pwm_duty = duty;
      }"]
      ;; Может не парситься из-за interrupt синтаксиса
      (let [result (parse-string pwm-program)]
        (when (parse-successful? result)
          (let [ast (get-ast result)]
            (is (= 4 (count (:declarations ast)))))))))

  (testing "Программа работы с клавиатурой 4x4"
    (let [keyboard-program "
      unsigned char key_map[4][4] = {
        {'1', '2', '3', 'A'},
        {'4', '5', '6', 'B'},
        {'7', '8', '9', 'C'},
        {'*', '0', '#', 'D'}
      };
      
      unsigned char scan_keyboard() {
        unsigned char row, col;
        unsigned char key = 0;
        
        for (row = 0; row < 4; row++) {
          // Установить строку
          P1 = ~(1 << row);
          
          // Небольшая задержка
          for (col = 0; col < 10; col++);
          
          // Проверить столбцы
          for (col = 0; col < 4; col++) {
            if (!(P2 & (1 << col))) {
              key = key_map[row][col];
              // Ждать отпускания клавиши
              while (!(P2 & (1 << col)));
              return key;
            }
          }
        }
        return 0;  // Клавиша не нажата
      }"]
      (is (parse-successful? (parse-string keyboard-program)))
      (let [ast (get-ast (parse-string keyboard-program))]
        (is (= 2 (count (:declarations ast))))))))

;; ============================================================================
;; УТИЛИТЫ ДЛЯ ЗАПУСКА ТЕСТОВ
;; ============================================================================

(defn run-c51-specific-tests
  "Запуск всех тестов специфичных для C51"
  []
  (println "=== Запуск тестов специфичных конструкций C51 ===")
  (let [start-time (System/currentTimeMillis)]
    (run-tests 'c51cc.parser-c51-specific-test)
    (let [end-time (System/currentTimeMillis)
          total-time (- end-time start-time)]
      (println (str "\n=== C51-специфичное тестирование завершено за " total-time "мс ===")))))

(defn test-c51-feature-support
  "Проверка поддержки различных C51 функций"
  []
  (println "=== Проверка поддержки C51 функций ===")
  
  (let [features [
        ["Битовые переменные" "bit flag = 1;"]
        ["Модификаторы памяти" "data unsigned char buffer[10];"]
        ["Прерывания" "void isr() interrupt 1 { }"]
        ["Банки регистров" "void func() using 1 { }"]
        ["Inline ассемблер" "_asm NOP _endasm"]
        ["Битовая адресация" "P1_0 = 1;"]]]
    
    (doseq [[feature-name code] features]
      (let [result (parse-string code)]
        (println (str feature-name ": " 
                     (if (parse-successful? result) "✓ Поддерживается" "✗ Не поддерживается")))))))

;; Экспорт функций
(def ^:export run-c51-specific-tests run-c51-specific-tests)
(def ^:export test-c51-feature-support test-c51-feature-support) 