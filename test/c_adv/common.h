#ifndef COMMON_H
#define COMMON_H

#include <reg2051.h>

/* Простые макросы для работы с битами */
#define SET_BIT(reg, bit)    (reg |= (1 << bit))
#define CLEAR_BIT(reg, bit)  (reg &= ~(1 << bit))
#define TOGGLE_BIT(reg, bit) (reg ^= (1 << bit))
#define TEST_BIT(reg, bit)   (reg & (1 << bit))

/* Простые макросы для тестирования */
#define TEST_PASS()  (P1 = 0xAA)
#define TEST_FAIL()  (P1 = 0x55)

/* Константы для AT89S4051 */
#define INTERNAL_RAM_SIZE   128
#define SFR_START_ADDR      0x80
#define MAX_TIMER_VALUE     0xFFFF

/* Простые определения типов памяти */
#define INTERNAL_DATA   data
#define INDIRECT_DATA   idata
#define EXTERNAL_DATA   xdata
#define CODE_MEMORY     code

/* Структуры для тестирования */
typedef struct {
    unsigned char id;
    unsigned char status;
    unsigned int value;
} TestData8051;

typedef union {
    unsigned int word;
    struct {
        unsigned char low;
        unsigned char high;
    } bytes;
} TimerValue;

/* Объявления функций тестирования */
void test_memory_types(void);
void test_bit_operations(void);
void test_timer_operations(void);
void test_port_operations(void);
void test_interrupt_setup(void);

/* Функции для работы с таймерами */
void timer0_init(unsigned int reload_value);
void timer1_init(unsigned int reload_value);
unsigned int timer0_read(void);
unsigned int timer1_read(void);

/* Функции для работы с портами */
void port_init(void);
unsigned char read_port1(void);
void write_port1(unsigned char value);

#endif /* COMMON_H */ 