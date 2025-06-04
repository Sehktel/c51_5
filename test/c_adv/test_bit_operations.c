/*
 * Тест битовых операций в C51
 * Демонстрирует работу с битовыми переменными и операциями
 * Соответствует стандарту C89
 */

#include "common.h"

/* Битовые переменные - уникальная особенность C51 */
bit system_ready;
bit error_flag;
bit test_mode;

/* Тест основных битовых операций */
void test_bit_operations(void) {
    unsigned char test_byte;
    unsigned char result;
    
    /* Инициализация тестового байта */
    test_byte = 0x00;
    
    /* Тест установки битов */
    SET_BIT(test_byte, 0);
    SET_BIT(test_byte, 2);
    SET_BIT(test_byte, 4);
    SET_BIT(test_byte, 6);
    
    /* Проверка результата (должно быть 0x55) */
    if (test_byte == 0x55) {
        SET_BIT(P1, 0);  /* Тест установки битов пройден */
    }
    
    /* Тест сброса битов */
    CLEAR_BIT(test_byte, 0);
    CLEAR_BIT(test_byte, 4);
    
    /* Проверка результата (должно быть 0x44) */
    if (test_byte == 0x44) {
        SET_BIT(P1, 1);  /* Тест сброса битов пройден */
    }
    
    /* Тест инверсии битов */
    TOGGLE_BIT(test_byte, 1);
    TOGGLE_BIT(test_byte, 3);
    TOGGLE_BIT(test_byte, 5);
    TOGGLE_BIT(test_byte, 7);
    
    /* Проверка результата (должно быть 0xEE) */
    if (test_byte == 0xEE) {
        SET_BIT(P1, 2);  /* Тест инверсии битов пройден */
    }
    
    /* Тест проверки битов */
    result = 0;
    if (TEST_BIT(test_byte, 1)) result |= 0x01;
    if (TEST_BIT(test_byte, 3)) result |= 0x02;
    if (TEST_BIT(test_byte, 5)) result |= 0x04;
    if (TEST_BIT(test_byte, 7)) result |= 0x08;
    
    /* Все проверяемые биты должны быть установлены */
    if (result == 0x0F) {
        SET_BIT(P1, 3);  /* Тест проверки битов пройден */
    }
}

/* Тест работы с битовыми переменными C51 */
void test_bit_variables(void) {
    /* Инициализация битовых переменных */
    system_ready = 0;
    error_flag = 0;
    test_mode = 1;
    
    /* Логические операции с битовыми переменными */
    if (test_mode && !error_flag) {
        system_ready = 1;
    }
    
    /* Проверка результата */
    if (system_ready) {
        SET_BIT(P1, 4);  /* Тест битовых переменных пройден */
    }
    
    /* Тест условных операций */
    error_flag = system_ready && test_mode;
    if (error_flag) {
        SET_BIT(P1, 5);  /* Тест условных операций пройден */
    }
}

/* Тест работы с регистрами специальных функций */
void test_sfr_bits(void) {
    /* Сохранение исходного состояния */
    unsigned char original_p1 = P1;
    
    /* Тест установки отдельных битов порта */
    P1_0 = 1;  /* Установка бита 0 порта P1 */
    P1_1 = 0;  /* Сброс бита 1 порта P1 */
    P1_2 = 1;  /* Установка бита 2 порта P1 */
    
    /* Проверка состояния битов */
    if (P1_0 && !P1_1 && P1_2) {
        SET_BIT(P1, 6);  /* Тест SFR битов пройден */
    }
    
    /* Восстановление исходного состояния */
    P1 = original_p1;
}

/* Демонстрация эффективного использования битов */
void bit_manipulation_demo(void) {
    unsigned char status_register;
    unsigned char mask;
    
    /* Создание маски для работы с несколькими битами */
    mask = 0x0F;  /* Маска для младших 4 битов */
    
    /* Установка статусного регистра */
    status_register = 0xA5;
    
    /* Очистка младших 4 битов */
    status_register &= ~mask;
    
    /* Установка новых значений в младшие 4 бита */
    status_register |= (0x0C & mask);
    
    /* Проверка результата (должно быть 0xAC) */
    if (status_register == 0xAC) {
        SET_BIT(P1, 7);  /* Демонстрация битовых операций пройдена */
    }
} 