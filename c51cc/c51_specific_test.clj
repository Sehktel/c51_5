(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(defn test-c51-feature
  "Тестирование специфичных для C51 возможностей"
  [code description]
  (println (str "\n=== " description " ==="))
  (println (str "Код: " code))
  (try
    (let [tokens (lexer/tokenize code)
          result (parser/parse tokens)]
      (println (str "Токенов: " (count tokens)))
      (println (str "Успех: " (:success result)))
      (when (:success result)
        (let [ast (:ast result)]
          (println (str "AST тип: " (:ast-type ast)))
          (when (:declarations ast)
            (println (str "Объявлений: " (count (:declarations ast)))))
          (when (parser/validate-ast ast)
            (println "✓ AST валиден"))
          ;; Дополнительный анализ AST
          (when (and (:declarations ast) (> (count (:declarations ast)) 0))
            (let [first-decl (first (:declarations ast))]
              (when (:memory-modifier first-decl)
                (println (str "Модификатор памяти: " (:memory-modifier first-decl))))
              (when (:interrupt-number first-decl)
                (println (str "Номер прерывания: " (:interrupt-number first-decl))))
              (when (:bit-address first-decl)
                (println (str "Битовый адрес: " (:bit-address first-decl))))))))
      (when (not (:success result))
        (println (str "Ошибка: " (:error result))))
      (:success result))
    (catch Exception e
      (println (str "Исключение: " (.getMessage e)))
      false)))

(println "=== ТЕСТИРОВАНИЕ СПЕЦИФИЧНЫХ ДЛЯ C51 КОНСТРУКЦИЙ ===")

;; Модификаторы памяти
(test-c51-feature "unsigned char data buffer[128];" "Модификатор data")
(test-c51-feature "unsigned char idata stack[64];" "Модификатор idata")
(test-c51-feature "unsigned char xdata external_ram[1024];" "Модификатор xdata")
(test-c51-feature "unsigned char code lookup_table[] = {1,2,3,4,5};" "Модификатор code")
(test-c51-feature "unsigned char pdata page_memory[256];" "Модификатор pdata")

;; Битовые переменные
(test-c51-feature "bit flag1, flag2, flag3;" "Битовые переменные")
(test-c51-feature "sbit LED = P1^0;" "Специальный битовый регистр")
(test-c51-feature "sbit BUTTON = P3^2;" "Битовый доступ к порту")
(test-c51-feature "sbit RS = P2^0; sbit EN = P2^1;" "Множественные sbit")

;; Регистры специальных функций (SFR)
(test-c51-feature "P0 = 0xFF;" "Порт P0")
(test-c51-feature "P1 = 0x00;" "Порт P1")
(test-c51-feature "P2 = 0xAA;" "Порт P2")
(test-c51-feature "P3 = 0x55;" "Порт P3")
(test-c51-feature "TMOD = 0x01;" "Регистр режима таймера")
(test-c51-feature "TCON = 0x50;" "Регистр управления таймером")
(test-c51-feature "TH0 = 0xFC; TL0 = 0x18;" "Регистры таймера 0")
(test-c51-feature "TH1 = 0xFD; TL1 = 0x00;" "Регистры таймера 1")
(test-c51-feature "SCON = 0x50;" "Регистр управления UART")
(test-c51-feature "SBUF = 'A';" "Буфер UART")
(test-c51-feature "IE = 0x82;" "Регистр разрешения прерываний")
(test-c51-feature "IP = 0x04;" "Регистр приоритета прерываний")

;; Функции прерываний
(test-c51-feature "void ext_int0() interrupt 0 { P1 ^= 0x01; }" "Внешнее прерывание 0")
(test-c51-feature "void timer0_isr() interrupt 1 { TH0 = 0xFC; TL0 = 0x18; }" "Прерывание таймера 0")
(test-c51-feature "void ext_int1() interrupt 2 { /* обработка */ }" "Внешнее прерывание 1")
(test-c51-feature "void timer1_isr() interrupt 3 { /* обработка */ }" "Прерывание таймера 1")
(test-c51-feature "void serial_isr() interrupt 4 { if (RI) RI = 0; }" "Прерывание UART")

;; Битовые операции с портами
(test-c51-feature "P1 |= (1 << 3);" "Установка бита порта")
(test-c51-feature "P1 &= ~(1 << 3);" "Сброс бита порта")
(test-c51-feature "P1 ^= (1 << 3);" "Инверсия бита порта")
(test-c51-feature "if (P1 & (1 << 3)) { /* бит установлен */ }" "Проверка бита порта")

;; Работа с таймерами
(test-c51-feature "
void timer0_init() {
  TMOD = 0x01;    // Режим 1, 16-битный таймер
  TH0 = 0xFC;     // Старший байт
  TL0 = 0x18;     // Младший байт
  ET0 = 1;        // Разрешить прерывание таймера 0
  TR0 = 1;        // Запустить таймер 0
}" "Инициализация таймера 0")

;; Работа с UART
(test-c51-feature "
void uart_init() {
  SCON = 0x50;    // Режим 1, разрешить прием
  TMOD |= 0x20;   // Таймер 1, режим 2
  TH1 = 0xFD;     // Скорость 9600 бод
  TR1 = 1;        // Запустить таймер 1
  ES = 1;         // Разрешить прерывание UART
}" "Инициализация UART")

;; Работа с внешней памятью
(test-c51-feature "
void external_memory_test() {
  unsigned char xdata *ptr = (unsigned char xdata *)0x8000;
  *ptr = 0xAA;
  if (*ptr == 0xAA) {
    // Память работает
  }
}" "Тест внешней памяти")

;; Реальные программы для 8051
(test-c51-feature "
// Программа мигания светодиода
void led_blink() {
  sbit LED = P1^0;
  
  while (1) {
    LED = 1;
    delay_ms(500);
    LED = 0;
    delay_ms(500);
  }
}" "Программа мигания светодиода")

(test-c51-feature "
// Программа сканирования клавиатуры 4x4
unsigned char scan_keyboard() {
  unsigned char key = 0xFF;
  unsigned char row, col;
  
  for (row = 0; row < 4; row++) {
    P1 = ~(1 << row);  // Активировать строку
    delay_us(10);      // Задержка для стабилизации
    
    col = P2 & 0x0F;   // Читать столбцы
    if (col != 0x0F) { // Если нажата кнопка
      for (col = 0; col < 4; col++) {
        if (!(P2 & (1 << col))) {
          key = row * 4 + col;
          break;
        }
      }
      break;
    }
  }
  
  P1 = 0xFF;  // Деактивировать все строки
  return key;
}" "Сканирование клавиатуры 4x4")

(test-c51-feature "
// Программа управления семисегментным индикатором
unsigned char code seven_segment[] = {
  0x3F, 0x06, 0x5B, 0x4F, 0x66, 0x6D, 0x7D, 0x07,
  0x7F, 0x6F, 0x77, 0x7C, 0x39, 0x5E, 0x79, 0x71
};

void display_hex(unsigned char value) {
  P0 = seven_segment[value & 0x0F];  // Младший разряд
  P2 = seven_segment[value >> 4];    // Старший разряд
}" "Семисегментный индикатор")

(test-c51-feature "
// Программа ШИМ (PWM)
void pwm_init() {
  TMOD = 0x02;    // Таймер 0, режим 2 (8-битный автоперезагрузка)
  TH0 = 0x00;     // Период ШИМ
  TL0 = 0x00;
  ET0 = 1;        // Разрешить прерывание таймера 0
  TR0 = 1;        // Запустить таймер 0
  EA = 1;         // Глобальное разрешение прерываний
}

unsigned char pwm_duty = 128;  // Коэффициент заполнения (50%)
sbit PWM_OUT = P1^0;

void timer0_pwm() interrupt 1 {
  static unsigned char counter = 0;
  counter++;
  PWM_OUT = (counter < pwm_duty) ? 1 : 0;
}" "Генератор ШИМ")

(println "\n=== ТЕСТИРОВАНИЕ C51 ЗАВЕРШЕНО ===")

;; Анализ токенизации специфичных конструкций
(println "\n=== АНАЛИЗ ТОКЕНИЗАЦИИ C51 ===")

(defn analyze-c51-tokens [code description]
  (println (str "\n--- " description " ---"))
  (println (str "Код: " code))
  (let [tokens (lexer/tokenize code)]
    (println (str "Токенов: " (count tokens)))
    (doseq [[i token] (map-indexed vector (take 10 tokens))]
      (println (str "  " i ": " token)))
    (when (> (count tokens) 10)
      (println "  ... (показаны первые 10 токенов)"))))

(analyze-c51-tokens "sbit LED = P1^0;" "sbit объявление")
(analyze-c51-tokens "unsigned char xdata buffer[256];" "xdata модификатор")
(analyze-c51-tokens "void timer0_isr() interrupt 1 { }" "interrupt функция")
(analyze-c51-tokens "P1 |= (1 << 3);" "битовые операции с портом")

(println "\n=== АНАЛИЗ ЗАВЕРШЕН ===") 