// reg51.h - Определения регистров для AT89S4051
// Стандартный заголовочный файл для микроконтроллеров 8051

#ifndef __REG51_H__
#define __REG51_H__

// Special Function Registers (SFR)
sfr ACC    = 0xE0;  // Аккумулятор
sfr B      = 0xF0;  // Регистр B
sfr PSW    = 0xD0;  // Program Status Word
sfr SP     = 0x81;  // Stack Pointer
sfr DPL    = 0x82;  // Data Pointer Low
sfr DPH    = 0x83;  // Data Pointer High
sfr PCON   = 0x87;  // Power Control
sfr TCON   = 0x88;  // Timer Control
sfr TMOD   = 0x89;  // Timer Mode
sfr TL0    = 0x8A;  // Timer 0 Low
sfr TL1    = 0x8B;  // Timer 1 Low
sfr TH0    = 0x8C;  // Timer 0 High
sfr TH1    = 0x8D;  // Timer 1 High
sfr P1     = 0x90;  // Port 1
sfr SCON   = 0x98;  // Serial Control
sfr SBUF   = 0x99;  // Serial Buffer
sfr P3     = 0xB0;  // Port 3
sfr IE     = 0xA8;  // Interrupt Enable
sfr IP     = 0xB8;  // Interrupt Priority

// Битовые адреса для портов
// Port 1 bits
sbit P1_0  = P1^0;
sbit P1_1  = P1^1;
sbit P1_2  = P1^2;
sbit P1_3  = P1^3;
sbit P1_4  = P1^4;
sbit P1_5  = P1^5;
sbit P1_6  = P1^6;
sbit P1_7  = P1^7;

// Port 3 bits
sbit P3_0  = P3^0;  // RXD
sbit P3_1  = P3^1;  // TXD
sbit P3_2  = P3^2;  // INT0
sbit P3_3  = P3^3;  // INT1
sbit P3_4  = P3^4;  // T0
sbit P3_5  = P3^5;  // T1

// Timer Control bits
sbit TF1   = TCON^7;  // Timer 1 overflow flag
sbit TR1   = TCON^6;  // Timer 1 run control
sbit TF0   = TCON^5;  // Timer 0 overflow flag
sbit TR0   = TCON^4;  // Timer 0 run control
sbit IE1   = TCON^3;  // External interrupt 1 flag
sbit IT1   = TCON^2;  // External interrupt 1 type
sbit IE0   = TCON^1;  // External interrupt 0 flag
sbit IT0   = TCON^0;  // External interrupt 0 type

// Serial Control bits
sbit SM0   = SCON^7;  // Serial mode bit 0
sbit SM1   = SCON^6;  // Serial mode bit 1
sbit SM2   = SCON^5;  // Serial mode bit 2
sbit REN   = SCON^4;  // Receive enable
sbit TB8   = SCON^3;  // Transmit bit 8
sbit RB8   = SCON^2;  // Receive bit 8
sbit TI    = SCON^1;  // Transmit interrupt flag
sbit RI    = SCON^0;  // Receive interrupt flag

// Interrupt Enable bits
sbit EA    = IE^7;    // Global interrupt enable
sbit ES    = IE^4;    // Serial interrupt enable
sbit ET1   = IE^3;    // Timer 1 interrupt enable
sbit EX1   = IE^2;    // External interrupt 1 enable
sbit ET0   = IE^1;    // Timer 0 interrupt enable
sbit EX0   = IE^0;    // External interrupt 0 enable

// Program Status Word bits
sbit CY    = PSW^7;   // Carry flag
sbit AC    = PSW^6;   // Auxiliary carry flag
sbit F0    = PSW^5;   // User flag 0
sbit RS1   = PSW^4;   // Register bank select 1
sbit RS0   = PSW^3;   // Register bank select 0
sbit OV    = PSW^2;   // Overflow flag
sbit P     = PSW^0;   // Parity flag

// Константы для режимов таймера
#define TIMER_MODE_0  0x00  // 13-bit timer
#define TIMER_MODE_1  0x01  // 16-bit timer
#define TIMER_MODE_2  0x02  // 8-bit auto-reload
#define TIMER_MODE_3  0x03  // Split timer mode

// Константы для последовательного порта
#define SERIAL_MODE_0 0x00  // Shift register
#define SERIAL_MODE_1 0x40  // 8-bit UART, variable baud
#define SERIAL_MODE_2 0x80  // 9-bit UART, fixed baud
#define SERIAL_MODE_3 0xC0  // 9-bit UART, variable baud

#endif // __REG51_H__ 