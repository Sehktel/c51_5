/*
 * Тест операций с портами ввода-вывода в C51
 * Демонстрирует работу с портами P1, P3 на AT89S4051
 * Соответствует стандарту C89
 */

#include "common.h"

/* Глобальные переменные для работы с портами */
unsigned char port_test_pattern = 0x55;
bit port_test_flag = 0;

/* Инициализация портов */
void port_init(void) {
    /* Инициализация P1 как выходной порт */
    P1 = 0xFF;  /* Установка всех линий в высокий уровень */
    
    /* P3 используется для специальных функций на AT89S4051 */
    /* P3.0 - RXD, P3.1 - TXD, P3.2 - INT0, P3.3 - INT1 */
    /* P3.4 - T0, P3.5 - T1 */
}

/* Чтение порта P1 */
unsigned char read_port1(void) {
    return P1;
}

/* Запись в порт P1 */
void write_port1(unsigned char value) {
    P1 = value;
}

/* Тест основных операций с портами */
void test_port_operations(void) {
    unsigned char test_value;
    unsigned char original_value;
    
    /* Сохранение исходного состояния */
    original_value = P1;
    
    /* Тест записи в порт */
    write_port1(0xAA);
    test_value = read_port1();
    
    if (test_value == 0xAA) {
        SET_BIT(P1, 0);  /* Тест записи пройден */
    }
    
    /* Тест инверсии */
    write_port1(0x55);
    test_value = read_port1();
    
    if (test_value == 0x55) {
        SET_BIT(P1, 1);  /* Тест инверсии пройден */
    }
    
    /* Тест установки отдельных битов */
    write_port1(0x00);
    SET_BIT(P1, 2);
    SET_BIT(P1, 4);
    SET_BIT(P1, 6);
    
    test_value = read_port1();
    if ((test_value & 0x54) == 0x54) {
        SET_BIT(P1, 3);  /* Тест установки битов пройден */
    }
}

/* Тест работы с отдельными битами порта */
void test_port_bits(void) {
    /* Очистка порта */
    P1 = 0x00;
    
    /* Тест установки отдельных битов через битовые переменные */
    P1_0 = 1;
    P1_2 = 1;
    P1_4 = 1;
    P1_6 = 1;
    
    /* Проверка результата */
    if (P1 == 0x55) {
        P1_7 = 1;  /* Индикация успеха */
    }
    
    /* Тест сброса битов */
    P1_0 = 0;
    P1_4 = 0;
    
    if (P1 == 0xA4) {
        P1_1 = 1;  /* Индикация успеха сброса */
    }
}

/* Демонстрация сканирования портов */
void port_scan_demo(void) {
    unsigned char i;
    unsigned char pattern;
    
    /* Бегущий огонек */
    pattern = 0x01;
    for (i = 0; i < 8; i++) {
        P1 = pattern;
        pattern = pattern << 1;
        
        /* Простая задержка */
        {
            unsigned int delay;
            for (delay = 0; delay < 1000; delay++) {
                /* Пустой цикл */
            }
        }
    }
    
    /* Обратный бегущий огонек */
    pattern = 0x80;
    for (i = 0; i < 8; i++) {
        P1 = pattern;
        pattern = pattern >> 1;
        
        /* Простая задержка */
        {
            unsigned int delay;
            for (delay = 0; delay < 1000; delay++) {
                /* Пустой цикл */
            }
        }
    }
}

/* Тест чтения входных сигналов */
void test_input_reading(void) {
    unsigned char input_state;
    
    /* Установка порта в режим ввода (все биты в 1) */
    P1 = 0xFF;
    
    /* Чтение состояния порта */
    input_state = P1;
    
    /* Анализ входных сигналов */
    if (TEST_BIT(input_state, 0)) {
        /* Бит 0 установлен */
        port_test_flag = 1;
    }
    
    if (TEST_BIT(input_state, 7)) {
        /* Бит 7 установлен */
        TOGGLE_BIT(P1, 1);
    }
}

/* Демонстрация работы с масками портов */
void port_mask_demo(void) {
    unsigned char mask_lower;
    unsigned char mask_upper;
    unsigned char port_value;
    
    mask_lower = 0x0F;  /* Маска для младших 4 битов */
    mask_upper = 0xF0;  /* Маска для старших 4 битов */
    
    /* Установка только младших битов */
    P1 = 0x00;
    P1 |= (0x0A & mask_lower);
    
    /* Установка только старших битов */
    P1 |= (0x50 & mask_upper);
    
    port_value = P1;
    
    /* Проверка результата */
    if (port_value == 0x5A) {
        TOGGLE_BIT(P1, 7);  /* Индикация успеха */
    }
} 