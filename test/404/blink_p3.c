#include <reg51.h>

void wait() {
	unsigned char i, j;
	for (i = 0; i < 255; i++){
		for (j = 0; j < 255; j++){
			;
		} 
	}
}


void main(void){
	while(1){
		P1 = 0x00; wait();
		P1 = 0xFF; wait();
	}
}