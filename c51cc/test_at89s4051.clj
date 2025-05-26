(ns test-at89s4051
  (:require [c51cc.preprocessor :as pp]
            [clojure.string :as str]
            [clojure.java.io :as io]))

(defn test-at89s4051-preprocessing []
  "Тестирует препроцессирование примера программы для AT89S4051"
  (println "=== Тест препроцессирования для AT89S4051 ===")
  
  ;; Читаем исходный файл
  (let [source-file "example_program.c"]
    (if (.exists (io/file source-file))
      (let [source-code (slurp source-file)
            ;; Обрабатываем через препроцессор
            processed-code (pp/preprocess source-code 
                                         {:current-file source-file
                                          :include-paths ["." "include"]})
            ;; Сохраняем результат
            output-file "example_program_preprocessed.c"]
        
        (println "Исходный файл:" source-file)
        (println "Размер исходного кода:" (count source-code) "символов")
        
        ;; Сохраняем обработанный код
        (spit output-file processed-code)
        
        (println "Обработанный файл сохранен как:" output-file)
        (println "Размер обработанного кода:" (count processed-code) "символов")
        
        ;; Показываем первые несколько строк результата
        (println "\nПервые 20 строк обработанного кода:")
        (doseq [line (take 20 (str/split-lines processed-code))]
          (println line))
        
        (println "\n... (полный результат сохранен в файле)")
        processed-code)
      
      (println "Ошибка: файл" source-file "не найден"))))

(defn test-simple-at89s4051-code []
  "Тестирует простой код для AT89S4051 с макросами"
  (println "\n=== Тест простого кода AT89S4051 ===")
  
  (let [code "#include \"c51_types.h\"

#define LED_PIN 0
#define DELAY_COUNT 100

void main() {
    uint8_t counter = 0;
    
    // Инициализация
    P1 = 0xFF;
    
    while (1) {
        // Мигание светодиода
        LED_TOGGLE(LED_PIN);
        
        // Задержка
        DELAY_MS(DELAY_COUNT);
        
        counter++;
        if (counter > MAX_UINT8) {
            counter = 0;
        }
    }
}"
        result (pp/preprocess code {:include-paths ["." "include"]})]
    
    (println "Исходный код:")
    (println code)
    (println "\nРезультат препроцессирования:")
    (println result)))

(defn test-conditional-compilation-at89s4051 []
  "Тестирует условную компиляцию для различных конфигураций AT89S4051"
  (println "\n=== Тест условной компиляции AT89S4051 ===")
  
  (let [code "#include \"c51_sfr.h\"

#define AT89S4051_VERSION 1
#define ENABLE_UART 1
#define ENABLE_TIMER 1

void main() {
    #if AT89S4051_VERSION >= 1
        // Код для AT89S4051
        P1 = 0xFF;  // Инициализация порта P1
        
        #ifdef ENABLE_TIMER
            TMOD = TIMER0_MODE_16BIT;
            ET0 = 1;
            TR0 = 1;
        #endif
        
        #ifdef ENABLE_UART
            SCON = 0x50;
            ES = 1;
        #endif
        
        EA = 1;  // Глобальное разрешение прерываний
    #else
        #error \"Неподдерживаемая версия микроконтроллера\"
    #endif
    
    while (1) {
        #ifdef ENABLE_TIMER
            // Работа с таймером
            if (TF0) {
                TF0 = 0;
                LED_TOGGLE(0);
            }
        #endif
        
        #ifdef ENABLE_UART
            // Работа с UART
            if (RI) {
                uint8_t data = SBUF;
                RI = 0;
                SBUF = data;  // Эхо
            }
        #endif
    }
}"
        result (pp/preprocess code {:include-paths ["." "include"]})]
    
    (println "Исходный код:")
    (println code)
    (println "\nРезультат препроцессирования:")
    (println result)))

(defn test-macro-expansion []
  "Тестирует расширение макросов для AT89S4051"
  (println "\n=== Тест расширения макросов ===")
  
  (let [code "#define F_CPU 11059200U
#define BAUD_RATE 9600
#define TIMER_VALUE (65536U - (F_CPU / (12U * 1000U)))

// Функциональные макросы
#define SET_TIMER(high, low) TH0 = high; TL0 = low
#define INIT_UART(baud) TH1 = (256U - (F_CPU / (32U * 12U * baud)))

void timer_init() {
    uint16_t reload_value = TIMER_VALUE;
    SET_TIMER(HIGH_BYTE(reload_value), LOW_BYTE(reload_value));
}

void uart_init() {
    INIT_UART(BAUD_RATE);
}"
        result (pp/preprocess code)]
    
    (println "Исходный код:")
    (println code)
    (println "\nРезультат препроцессирования:")
    (println result)))

(defn run-at89s4051-tests []
  "Запускает все тесты для AT89S4051"
  (println "ТЕСТИРОВАНИЕ ПРЕПРОЦЕССОРА ДЛЯ AT89S4051")
  (println "==========================================")
  
  (test-at89s4051-preprocessing)
  (test-simple-at89s4051-code)
  (test-conditional-compilation-at89s4051)
  (test-macro-expansion)
  
  (println "\nВсе тесты AT89S4051 завершены!")
  (println "Проверьте файл example_program_preprocessed.c для полного результата."))

;; Запуск тестов
(run-at89s4051-tests) 