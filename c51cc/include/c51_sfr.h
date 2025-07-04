#ifndef __C51_SFR_H__
#define __C51_SFR_H__

#include "c51_types.h"

// Определения SFR регистров микроконтроллера AT89S4051
// AT89S4051 имеет ограниченный набор SFR по сравнению с полным 8051

// Доступные порты в AT89S4051
sfr P1    = 0x90;  // Порт P1 (полностью доступен, 8 бит)
sfr P3    = 0xB0;  // Порт P3 (частично доступен)

// Управление таймерами (AT89S4051 имеет Timer0 и Timer1)
sfr TCON  = 0x88;  // Управление таймерами
sfr TMOD  = 0x89;  // Режимы таймеров
sfr TL0   = 0x8A;  // Младший байт таймера 0
sfr TL1   = 0x8B;  // Младший байт таймера 1
sfr TH0   = 0x8C;  // Старший байт таймера 0
sfr TH1   = 0x8D;  // Старший байт таймера 1

// UART (последовательный порт)
sfr SCON  = 0x98;  // Управление UART
sfr SBUF  = 0x99;  // Буфер UART

// Управление прерываниями
sfr IE    = 0xA8;  // Регистр разрешения прерываний
sfr IP    = 0xB8;  // Регистр приоритетов прерываний

// Основные регистры процессора
sfr PSW   = 0xD0;  // Слово состояния программы
sfr ACC   = 0xE0;  // Аккумулятор
sfr B     = 0xF0;  // Регистр B
sfr SP    = 0x81;  // Указатель стека
sfr DPL   = 0x82;  // Младший байт указателя данных
sfr DPH   = 0x83;  // Старший байт указателя данных

// Управление питанием
sfr PCON  = 0x87;  // Управление питанием

// Биты регистра TCON
sbit TF1  = TCON^7;  // Флаг переполнения таймера 1
sbit TR1  = TCON^6;  // Запуск таймера 1
sbit TF0  = TCON^5;  // Флаг переполнения таймера 0
sbit TR0  = TCON^4;  // Запуск таймера 0
sbit IE1  = TCON^3;  // Флаг внешнего прерывания 1
sbit IT1  = TCON^2;  // Тип внешнего прерывания 1
sbit IE0  = TCON^1;  // Флаг внешнего прерывания 0
sbit IT0  = TCON^0;  // Тип внешнего прерывания 0

// Биты регистра IE
sbit EA   = IE^7;    // Глобальное разрешение прерываний
sbit ES   = IE^4;    // Разрешение прерывания от UART
sbit ET1  = IE^3;    // Разрешение прерывания от таймера 1
sbit EX1  = IE^2;    // Разрешение внешнего прерывания 1
sbit ET0  = IE^1;    // Разрешение прерывания от таймера 0
sbit EX0  = IE^0;    // Разрешение внешнего прерывания 0

// Биты регистра SCON
sbit SM0  = SCON^7;  // Режим UART бит 0
sbit SM1  = SCON^6;  // Режим UART бит 1
sbit SM2  = SCON^5;  // Режим UART бит 2
sbit REN  = SCON^4;  // Разрешение приема
sbit TB8  = SCON^3;  // 9-й бит передачи
sbit RB8  = SCON^2;  // 9-й бит приема
sbit TI   = SCON^1;  // Флаг завершения передачи
sbit RI   = SCON^0;  // Флаг приема данных

// Биты регистра PSW
sbit CY   = PSW^7;   // Флаг переноса
sbit AC   = PSW^6;   // Флаг вспомогательного переноса
sbit F0   = PSW^5;   // Пользовательский флаг 0
sbit RS1  = PSW^4;   // Выбор банка регистров бит 1
sbit RS0  = PSW^3;   // Выбор банка регистров бит 0
sbit OV   = PSW^2;   // Флаг переполнения
sbit P    = PSW^0;   // Флаг четности

// Биты порта P3 (доступные в AT89S4051)
sbit RXD  = P3^0;    // Вход UART
sbit TXD  = P3^1;    // Выход UART
sbit INT0 = P3^2;    // Внешнее прерывание 0
sbit INT1 = P3^3;    // Внешнее прерывание 1
sbit T0   = P3^4;    // Внешний вход таймера 0
sbit T1   = P3^5;    // Внешний вход таймера 1

// Макросы для работы с таймерами AT89S4051
#define TIMER0_MODE_13BIT  0x00  // 13-битный таймер/счетчик
#define TIMER0_MODE_16BIT  0x01  // 16-битный таймер/счетчик
#define TIMER0_MODE_8BIT   0x02  // 8-битный автоперезагружаемый
#define TIMER0_MODE_SPLIT  0x03  // Разделенный режим

#define TIMER1_MODE_13BIT  0x00  // 13-битный таймер/счетчик
#define TIMER1_MODE_16BIT  0x10  // 16-битный таймер/счетчик
#define TIMER1_MODE_8BIT   0x20  // 8-битный автоперезагружаемый
#define TIMER1_MODE_STOP   0x30  // Остановлен

#endif // __C51_SFR_H__ 