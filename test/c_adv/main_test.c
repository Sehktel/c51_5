/*
 * Главный тестовый файл для C51
 * Запускает все тесты для AT89S4051
 * Соответствует стандарту C89
 */

#include "common.h"

/* Главная функция */
void main(void) {
    /* Инициализация системы */
    port_init();
    
    /* Индикация начала тестирования */
    P1 = 0x00;
    
    /* Запуск тестов памяти */
    test_memory_types();
    
    /* Небольшая задержка между тестами */
    {
        unsigned int delay;
        for (delay = 0; delay < 5000; delay++) {
            /* Пустой цикл */
        }
    }
    
    /* Запуск тестов битовых операций */
    test_bit_operations();
    
    /* Задержка */
    {
        unsigned int delay;
        for (delay = 0; delay < 5000; delay++) {
            /* Пустой цикл */
        }
    }
    
    /* Запуск тестов таймеров */
    test_timer_operations();
    
    /* Задержка */
    {
        unsigned int delay;
        for (delay = 0; delay < 5000; delay++) {
            /* Пустой цикл */
        }
    }
    
    /* Запуск тестов портов */
    test_port_operations();
    
    /* Задержка */
    {
        unsigned int delay;
        for (delay = 0; delay < 5000; delay++) {
            /* Пустой цикл */
        }
    }
    
    /* Запуск тестов прерываний */
    test_interrupt_setup();
    
    /* Финальная индикация завершения всех тестов */
    P1 = 0xFF;
    
    /* Бесконечный цикл */
    while (1) {
        /* Программа завершена, ожидание */
    }
} 