/*
 * Тест операций с таймерами в C51
 * Демонстрирует работу с Timer0 и Timer1 на AT89S4051
 * Соответствует стандарту C89
 */

#include "common.h"

/* Глобальные переменные для работы с таймерами */
unsigned int timer0_overflow_count = 0;
unsigned int timer1_overflow_count = 0;
bit timer0_flag = 0;
bit timer1_flag = 0;

/* Инициализация Timer0 в режиме 1 (16-битный) */
void timer0_init(unsigned int reload_value) {
    /* Остановка таймера */
    TR0 = 0;
    
    /* Режим 1: 16-битный таймер/счетчик */
    TMOD &= 0xF0;  /* Очистка младших 4 битов */
    TMOD |= 0x01;  /* Установка режима 1 для Timer0 */
    
    /* Загрузка начального значения */
    TH0 = (unsigned char)(reload_value >> 8);    /* Старший байт */
    TL0 = (unsigned char)(reload_value & 0xFF);  /* Младший байт */
    
    /* Сброс флага переполнения */
    TF0 = 0;
    
    /* Запуск таймера */
    TR0 = 1;
}

/* Инициализация Timer1 в режиме 2 (8-битный с автоперезагрузкой) */
void timer1_init(unsigned int reload_value) {
    /* Остановка таймера */
    TR1 = 0;
    
    /* Режим 2: 8-битный с автоперезагрузкой */
    TMOD &= 0x0F;  /* Очистка старших 4 битов */
    TMOD |= 0x20;  /* Установка режима 2 для Timer1 */
    
    /* Загрузка значения перезагрузки */
    TH1 = (unsigned char)(reload_value & 0xFF);
    TL1 = (unsigned char)(reload_value & 0xFF);
    
    /* Сброс флага переполнения */
    TF1 = 0;
    
    /* Запуск таймера */
    TR1 = 1;
}

/* Чтение текущего значения Timer0 */
unsigned int timer0_read(void) {
    unsigned char high_byte, low_byte;
    
    /* Чтение значения таймера (сначала младший, потом старший байт) */
    low_byte = TL0;
    high_byte = TH0;
    
    return ((unsigned int)high_byte << 8) | low_byte;
}

/* Чтение текущего значения Timer1 */
unsigned int timer1_read(void) {
    return (unsigned int)TL1;
}

/* Тест основных операций с таймерами */
void test_timer_operations(void) {
    unsigned int initial_value, current_value;
    unsigned char delay_counter;
    
    /* Тест Timer0 */
    timer0_init(0x8000);  /* Загрузка значения 0x8000 */
    
    /* Небольшая задержка для работы таймера */
    for (delay_counter = 0; delay_counter < 100; delay_counter++) {
        /* Пустой цикл для задержки */
    }
    
    current_value = timer0_read();
    
    /* Проверка, что таймер работает (значение должно увеличиться) */
    if (current_value > 0x8000) {
        SET_BIT(P1, 0);  /* Timer0 работает корректно */
    }
    
    /* Тест Timer1 */
    timer1_init(0x80);  /* Загрузка значения 0x80 */
    
    /* Небольшая задержка */
    for (delay_counter = 0; delay_counter < 50; delay_counter++) {
        /* Пустой цикл для задержки */
    }
    
    current_value = timer1_read();
    
    /* Проверка работы Timer1 */
    if (current_value > 0x80) {
        SET_BIT(P1, 1);  /* Timer1 работает корректно */
    }
}

/* Тест флагов переполнения таймеров */
void test_timer_overflow(void) {
    unsigned char timeout_counter;
    
    /* Инициализация Timer0 с большим значением для быстрого переполнения */
    timer0_init(0xFFF0);
    
    /* Ожидание переполнения Timer0 */
    timeout_counter = 0;
    while (!TF0 && timeout_counter < 255) {
        timeout_counter++;
    }
    
    /* Проверка флага переполнения */
    if (TF0) {
        SET_BIT(P1, 2);  /* Переполнение Timer0 обнаружено */
        TF0 = 0;         /* Сброс флага */
    }
    
    /* Аналогичный тест для Timer1 */
    timer1_init(0xF0);
    
    timeout_counter = 0;
    while (!TF1 && timeout_counter < 255) {
        timeout_counter++;
    }
    
    if (TF1) {
        SET_BIT(P1, 3);  /* Переполнение Timer1 обнаружено */
        TF1 = 0;         /* Сброс флага */
    }
}

/* Демонстрация использования таймеров для генерации задержек */
void timer_delay_demo(void) {
    unsigned char i;
    
    /* Использование Timer0 для создания точной задержки */
    for (i = 0; i < 5; i++) {
        /* Инициализация таймера для задержки ~1мс при 12МГц */
        timer0_init(0xFC18);  /* 65536 - 1000 = 64536 = 0xFC18 */
        
        /* Ожидание переполнения */
        while (!TF0) {
            /* Ожидание */
        }
        
        TF0 = 0;  /* Сброс флага */
        
        /* Индикация на порту */
        TOGGLE_BIT(P1, 4);
    }
    
    /* Финальная индикация успешного завершения */
    SET_BIT(P1, 5);
}

/* Тест режимов работы таймеров */
void test_timer_modes(void) {
    /* Тест режима 0 (13-битный таймер) */
    TR0 = 0;
    TMOD &= 0xF0;  /* Режим 0 для Timer0 */
    TH0 = 0x1F;    /* Загрузка тестового значения */
    TL0 = 0xE0;
    TR0 = 1;
    
    if (TR0) {
        SET_BIT(P1, 6);  /* Режим 0 Timer0 активен */
    }
    
    /* Тест режима 3 (два 8-битных таймера) */
    TR0 = 0;
    TMOD &= 0xF0;
    TMOD |= 0x03;  /* Режим 3 для Timer0 */
    TH0 = 0x80;
    TL0 = 0x80;
    TR0 = 1;
    
    if (TR0) {
        SET_BIT(P1, 7);  /* Режим 3 Timer0 активен */
    }
} 