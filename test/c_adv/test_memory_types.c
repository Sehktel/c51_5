/*
 * Тест различных типов памяти в C51
 * Демонстрирует работу с data, idata, xdata и code памятью
 * Соответствует стандарту C89
 */

#include "common.h"

/* Переменные в различных типах памяти */
unsigned char INTERNAL_DATA internal_var = 0x55;           /* Внутренняя RAM (прямая адресация) */
unsigned char INDIRECT_DATA indirect_var = 0xAA;           /* Внутренняя RAM (косвенная адресация) */
unsigned char EXTERNAL_DATA external_var = 0x33;           /* Внешняя RAM (если доступна) */

/* Константы в программной памяти */
unsigned char CODE_MEMORY lookup_table[] = {
    0x00, 0x11, 0x22, 0x33, 0x44, 0x55, 0x66, 0x77,
    0x88, 0x99, 0xAA, 0xBB, 0xCC, 0xDD, 0xEE, 0xFF
};

/* Массив в стеке (автоматические переменные) */
void test_stack_memory(void) {
    unsigned char local_array[8];  /* Локальный массив в стеке */
    unsigned char i;
    
    /* Инициализация локального массива */
    for (i = 0; i < 8; i++) {
        local_array[i] = i * 2;
    }
    
    /* Проверка корректности записи */
    if (local_array[3] == 6 && local_array[7] == 14) {
        TEST_PASS();
    } else {
        TEST_FAIL();
    }
}

/* Тест работы с различными типами памяти */
void test_memory_types(void) {
    unsigned char temp_value;
    unsigned char INTERNAL_DATA *data_ptr;
    unsigned char CODE_MEMORY *code_ptr;
    
    /* Тест внутренней памяти (data) */
    internal_var = 0x12;
    if (internal_var == 0x12) {
        SET_BIT(P1, 0);  /* Сигнал успеха на бит 0 порта P1 */
    }
    
    /* Тест косвенной адресации (idata) */
    indirect_var = 0x34;
    if (indirect_var == 0x34) {
        SET_BIT(P1, 1);  /* Сигнал успеха на бит 1 порта P1 */
    }
    
    /* Тест чтения из программной памяти (code) */
    temp_value = lookup_table[5];  /* Должно быть 0x55 */
    if (temp_value == 0x55) {
        SET_BIT(P1, 2);  /* Сигнал успеха на бит 2 порта P1 */
    }
    
    /* Тест работы с указателями на различные типы памяти */
    data_ptr = &internal_var;
    code_ptr = lookup_table;
    
    if (*data_ptr == 0x12 && code_ptr[0] == 0x00) {
        SET_BIT(P1, 3);  /* Сигнал успеха на бит 3 порта P1 */
    }
    
    /* Тест стековой памяти */
    test_stack_memory();
    
    /* Финальная проверка - если все биты установлены */
    if ((P1 & 0x0F) == 0x0F) {
        P1 = 0xAA;  /* Все тесты пройдены */
    } else {
        P1 = 0x55;  /* Есть ошибки */
    }
}

/* Демонстрация эффективного использования памяти */
void memory_optimization_demo(void) {
    bit flag1, flag2, flag3;  /* Использование битовых переменных для экономии памяти */
    TestData8051 INTERNAL_DATA test_data;  /* Упаковка данных в структуру */
    
    test_data.id = 1;
    test_data.status = 0x80;
    test_data.value = 0x1234;
    
    /* Работа с битовыми флагами */
    flag1 = 1;
    flag2 = 0;
    flag3 = flag1 && !flag2;
    
    if (flag3 && test_data.id == 1) {
        TOGGLE_BIT(P1, 7);  /* Индикация успешного теста оптимизации */
    }
} 