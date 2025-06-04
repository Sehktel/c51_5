#include <reg51.h>

#define BIT_0 (1     )
#define BIT_1 (1 << 1)
#define BIT_2 (1 << 2)
#define BIT_3 (1 << 3)
#define BIT_4 (1 << 4)
#define BIT_5 (1 << 5)
#define BIT_6 (1 << 6)
#define BIT_7 (1 << 7)

//void comp() interrupt 6 {
//	// P3.6 -- is comparator output for P1.0 and P1.1 pins
//	if (P3 & 0x40) {
//		P1 ^= BIT_2;
//		P1 ^= BIT_3;
//	}
//	else {
//		P1 ^= BIT_2;
//		P1 ^= BIT_3;
//	}
//}

void comp() interrupt 6 {
	// P3.6 -- is comparator output for P1.0 and P1.1 pins
	P1 &= ~BIT_2;
	P1 &= ~BIT_3;
	if   (P3 & 0x40) { while(1) { P1 |= BIT_2; } }
	else             { while(1) { P1 |= BIT_3; } }
}


void wait(){
	int i = 0; int j = 0;
	for (i = 0; i < 255; i++){ 
	for (j = 0; j < 255; j++){ ; } }
//	for (i = 0; i < 64; i++){
//		for (j = 0; j < 64; j++){
//			;
//		}
//	}
}

void setup(){
//	P1 |=  BIT_2; // Set  P1.2 
//	P1 &= ~BIT_3; // Drop P1.3 
	P1 &= ~BIT_2; // Drop P1.2 
	P1 &= ~BIT_3; // Drop P1.3 

	EA = 1; // Enable Interrupt
	EC = 1; // Enable Comparator Interrupt

	ACSR &= ~BIT_4 ; //  Clear CF Comparator Interrupt Flag
	ACSR |=  BIT_3 ; //  Comparator Enable

	ACSR &= ~BIT_2 ; //  CM[2:0]
	ACSR |=  BIT_1 ; //  0 1 0 
	ACSR &= ~BIT_0 ; //  Toggle with debounce

	//Comparator Interrupt Mode
	//2 1 0 Interrupt Mode
	//--- ---- ---- ---------------------------------------
	//0 0 0 Negative (Low) level
	//0 0 1 Positive edge
	//0 1 0 Toggle with debounce
	//0 1 1 Positive edge with debounce
	//1 0 0 Negative edge
	//1 0 1 Toggle
	//1 1 0 Negative edge with debounce
	//1 1 1 Positive (High) level
}

void main(void){
	setup();
	while (1) {
		P1 ^= BIT_4; 
		wait(); // ^ -- XOR 
	}
}