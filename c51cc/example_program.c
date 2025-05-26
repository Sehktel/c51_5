#include "c51_sfr.h"

// Конфигурационные макросы для AT89S4051
#define F_CPU        11059200U   // Частота кристалла в Гц (16-битное значение достаточно для расчетов)
#define BAUD_RATE    9600        // Скорость UART

// Условная компиляция для различных режимов
#define DEBUG_MODE   1
#define USE_TIMER0   1
#define USE_UART     1

// Вычисляемые макросы (адаптированы под 16-битную арифметику)
#define TIMER_RELOAD_VALUE  (65536U - (F_CPU / (12U * 1000U)))  // Для 1мс
#define UART_RELOAD_VALUE   (256U - (F_CPU / (32U * 12U * BAUD_RATE)))

// Функциональные макросы для работы с единственным доступным портом P1
#define LED_ON(pin)     SET_BIT(P1, pin)
#define LED_OFF(pin)    CLEAR_BIT(P1, pin)
#define LED_TOGGLE(pin) TOGGLE_BIT(P1, pin)

#ifdef DEBUG_MODE
    #define DEBUG_PRINT(msg) uart_send_string(msg)
#else
    #define DEBUG_PRINT(msg) // Пустой макрос в release режиме
#endif

// Глобальные переменные (используем только 8/16-битные типы)
volatile uint16_t timer_counter = 0;
volatile uint8_t  uart_buffer[16];  // Уменьшенный буфер для экономии памяти

#ifdef USE_TIMER0
void timer0_init() {
    TMOD &= 0xF0;  // Очищаем биты таймера 0
    TMOD |= TIMER0_MODE_16BIT;  // Режим 1 для таймера 0 (16-битный)
    
    // Загружаем значение перезагрузки
    TH0 = HIGH_BYTE(TIMER_RELOAD_VALUE);
    TL0 = LOW_BYTE(TIMER_RELOAD_VALUE);
    
    ET0 = 1;       // Разрешение прерывания от таймера 0
    TR0 = 1;       // Запуск таймера 0
}

void timer0_isr() interrupt 1 {
    // Перезагрузка таймера
    TH0 = HIGH_BYTE(TIMER_RELOAD_VALUE);
    TL0 = LOW_BYTE(TIMER_RELOAD_VALUE);
    
    timer_counter++;
    
    #ifdef DEBUG_MODE
    // Мигание светодиода каждую секунду (используем только порт P1)
    if (timer_counter >= 1000) {
        LED_TOGGLE(0);  // Мигание светодиода на P1.0
        timer_counter = 0;
    }
    #endif
}
#endif

#ifdef USE_UART
void uart_init() {
    SCON = 0x50;   // Режим 1, разрешение приема
    TMOD &= 0x0F;  // Очищаем биты таймера 1
    TMOD |= TIMER1_MODE_8BIT;  // Таймер 1 в режиме 2 (автоперезагрузка)
    
    TH1 = (uint8_t)UART_RELOAD_VALUE;
    TL1 = (uint8_t)UART_RELOAD_VALUE;
    
    TR1 = 1;       // Запуск таймера 1
    ES = 1;        // Разрешение прерывания от UART
}

void uart_send_char(char c) {
    SBUF = c;
    while (!TI);   // Ожидание завершения передачи
    TI = 0;
}

void uart_send_string(char* str) {
    while (*str) {
        uart_send_char(*str++);
    }
}

void uart_isr() interrupt 4 {
    if (RI) {
        // Обработка принятого символа
        uint8_t received = SBUF;
        RI = 0;
        
        // Простое эхо
        uart_send_char(received);
    }
}
#endif

void system_init() {
    EA = 0;  // Запрет всех прерываний
    
    // Инициализация единственного доступного порта P1
    P1 = 0xFF;  // Все выводы порта P1 в высокое состояние
    
    #ifdef USE_TIMER0
    timer0_init();
    DEBUG_PRINT("Timer0 OK\r\n");
    #endif
    
    #ifdef USE_UART
    uart_init();
    DEBUG_PRINT("UART OK\r\n");
    #endif
    
    EA = 1;  // Разрешение всех прерываний
    
    DEBUG_PRINT("System ready\r\n");
}

void main() {
    uint8_t counter = 0;  // Локальный счетчик
    
    system_init();
    
    DEBUG_PRINT("AT89S4051 started\r\n");
    
    while (1) {
        // Основной цикл программы
        
        #ifdef DEBUG_MODE
        // Простая индикация работы каждые 50 итераций
        if (++counter >= 50) {
            LED_TOGGLE(1);  // Мигание светодиода на P1.1
            counter = 0;
        }
        #endif
        
        // Простая задержка (адаптированная под AT89S4051)
        DELAY_MS(10);
        
        #ifndef DEBUG_MODE
        // В release режиме выполняем энергосберегающие операции
        PCON |= 0x01;  // Переход в режим Idle
        #endif
    }
} 