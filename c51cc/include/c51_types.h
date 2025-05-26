#ifndef __C51_TYPES_H__
#define __C51_TYPES_H__

// Базовые типы данных для микроконтроллера AT89S4051 (8-битный)
typedef unsigned char  uint8_t;   // 8-битное беззнаковое целое
typedef signed char    int8_t;    // 8-битное знаковое целое
typedef unsigned int   uint16_t;  // 16-битное беззнаковое целое (максимум для AT89S4051)
typedef signed int     int16_t;   // 16-битное знаковое целое

// Специфичные для C51 типы
typedef bit            bool_t;    // Битовый тип (специфично для 8051)
typedef unsigned char  byte_t;    // Байт (8 бит)

// Константы для работы с портами AT89S4051
// AT89S4051 имеет только порты P1 и P3 (частично)
#define PORT_P1  0x90   // Порт P1 (8 бит, полностью доступен)
#define PORT_P3  0xB0   // Порт P3 (частично доступен)

// Макросы для работы с битами (оптимизированы для 8-битной архитектуры)
#define SET_BIT(reg, bit)    ((reg) |= (1 << (bit)))
#define CLEAR_BIT(reg, bit)  ((reg) &= ~(1 << (bit)))
#define TOGGLE_BIT(reg, bit) ((reg) ^= (1 << (bit)))
#define TEST_BIT(reg, bit)   ((reg) & (1 << (bit)))

// Макросы для задержек (адаптированы под AT89S4051 с типичной частотой 11.0592 МГц)
// Примерные значения для коротких задержек
#define DELAY_MS(ms) for(volatile uint8_t i = 0; i < (ms); i++) \
                         for(volatile uint8_t j = 0; j < 100; j++)
#define DELAY_US(us) for(volatile uint8_t i = 0; i < (us); i++)

// Константы для AT89S4051
#define MAX_UINT8   255U     // Максимальное значение для uint8_t
#define MAX_UINT16  65535U   // Максимальное значение для uint16_t
#define MAX_INT8    127      // Максимальное значение для int8_t
#define MIN_INT8    -128     // Минимальное значение для int8_t
#define MAX_INT16   32767    // Максимальное значение для int16_t
#define MIN_INT16   -32768   // Минимальное значение для int16_t

// Макросы для работы с младшими и старшими байтами 16-битных значений
#define LOW_BYTE(x)   ((uint8_t)((x) & 0xFF))
#define HIGH_BYTE(x)  ((uint8_t)(((x) >> 8) & 0xFF))
#define MAKE_WORD(h, l) (((uint16_t)(h) << 8) | (uint8_t)(l))

#endif // __C51_TYPES_H__ 