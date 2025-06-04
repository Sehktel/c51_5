#include <reg51.h>

#define uchar unsigned char

void wait() {
	int i = 0; 
	int j = 0; 
	for (i = 0; i < 256; i++){ for (j = 0; j < 32; j++) { ; } }
}

void serial_read(void) interrupt 4 {
  char A1 = 0;
  if (!RI) return; // Check RI flag before accessing SBUF

  // P3.7 toggle for debugging (optional)
  P3 ^= 0x80;

  A1 = SBUF; // Read received data
  RI = 0;    // Clear RI flag (optional, usually cleared by reading SBUF)
  P1 = A1;   // Directly assign received data to P1 for display
  TI = 0;    // Clear TI flag (optional, usually cleared by hardware)
  wait();
  return;
}

void main() {
  P1 = 0;
  SCON = 0x50; /* SCON: mode 1, 8-bit UART, enable receiver */
  TMOD |= 0x20; /* TMOD: timer 1, mode 2, 8-bit reload */
  TH1 = 252;   /* TH1: reload value for 19200 baud @ 16MHz */
  TL1 = 252;   /* TL1: reload value for 19200 baud @ 16MHz */
  TR1 = 1;     /* TR1: timer 1 run */
  TI = 0;      /* TI: set TI to send first char of UART */
  REN = 1;     /* Set Receive mode on */
  EA = 1;      /* Enable interrupts */
  ES = 1;      /* Enable serial interrupt */
  while (1);   /* Infinite loop (replace with your main program) */
}