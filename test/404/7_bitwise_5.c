#include <reg51.h>
#include <stdio.h>

int iA = 0, iB = 0; // operands A, B
int op = 0;         // operation AND, OR, XOR

void setAB (void) interrupt 0  {
	iA = (P1 & 0xE0) >> 5;
	iB = (P1 & 0x1C) >> 2;
	op = (P3 & 0x30) >> 4;
}

void clear (void) interrupt 2  {
	iA = 0;
	iB = 0;
	op = 0;
}

void main(void){
	int result = 0;

	#ifndef MONITOR51
		SCON  = 0x50;               /* SCON: mode 1, 8-bit UART, enable rcvr      */
		TMOD |= 0x20;               /* TMOD: timer 1, mode 2, 8-bit reload        */
		TH1   = 221;                /* TH1:  reload value for 1200 baud @ 16MHz   */
		TR1   = 1;                  /* TR1:  timer 1 run                          */
		TI    = 1;                  /* TI:   set TI to send first char of UART    */
	#endif

	EA  = 1;
	EX0 = 1;
	EX1 = 1;
	IT0 = 1;
	IT1 = 1;
	
	P3 &= 0xCF;
	P3 |= 0x80;
	while(1){
		if (op == 0) { result = 0xF0; }
		if (op == 1) { result = iA & iB; }
		if (op == 2) { result = iA | iB; }
		if (op == 3) { result = iA ^ iB; }
		P3 ^= 0x80;
		printf (" A  =  %u  B  =  %u  op =  %u  r  =  %u \n", iA, iB, op, result);
	}
}
