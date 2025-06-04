#include <reg2051.h>

void P14 (void) interrupt 0 using 2 {
	while(1){
		P1 = 0x04;
	}
}

void main(void){
	char a, b;
	a = 5;
	b = 15;
	EA = 1;
	EX0 = 1;
	IT0 = 1;
	while(1){
		P1 = 0x08;
	}
}