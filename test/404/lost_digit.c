# include <reg51.h>

void light(char s){
	if (s == 1){ P1 = 0;} 
	if (s == 2){ P1 = 4;}
	if (s == 3){ P1 = 8;}
	if (s == 4){ P1 = 12;}
	if (s == 5){ P1 = 16;}
	if (s == 6){ P1 = 20;}
	if (s == 7){ P1 = 24;}
	if (s == 8){ P1 = 28;}

	P3 = 62; // 0x3E  0011 1110
}

//void lwait(){
//	unsigned char w;
//	for (w = 0 ; w < 16; w++){ ; }
//	; ; ; ; 
//	; ; ; ; 
//	; ; ; ; 
//	; ; ; ; //16
//}

// middle for 2 3 4 5 6 7 -- is 14 cycles + -

void led_letter(char d){
	int w = 0;
//	unsigned char w = 0;
//	for (w = 0; w <= 255; w++){ //128 //256 //512 //2048
	for (w = 0; w < 2048; w++){ //128 //256 //512 //2048
		if (d ==  0){
			light(1);
			light(2);
			light(3);
			light(4);
			light(5);
			light(6);
		} // 0
		if (d ==  1){
			light(2);
			light(3);
			light(2);
			light(3);
			light(2);
			light(3);
		} // 1
		if (d ==  2){
			light(1);
			light(2);
			light(7);
			light(5);
			light(4);
		} // 2
		if (d ==  3){
			light(1);
			light(2);
			light(7);
			light(3);
			light(4);
		} // 3
		if (d ==  4){
			light(2);
			light(3);
			light(7);
			light(6);

			light(2);
			light(3);
			light(7);
			light(6);
		} // 4
		if (d ==  5){ //broken
			light(1);
			light(2); // 2 --> 6
			light(7);
			light(3);
			light(4);

//			light(1);
//			light(6);
//			light(7);
//			light(3);
//			light(4);
		} // 5

		if (d ==  6){
			light(1);
			light(6);
			light(5);
			light(4);
			light(3);
			light(7);
		} // 6
		if (d ==  7){
			light(1);
			light(2);
			light(3);

			light(1);
			light(2);
			light(3);
		} // 7
		if (d ==  8){
			light(1);
			light(2);
			light(3);
			light(4);
			light(5);
			light(6);
			light(7);
		} // 8
		if (d ==  9){
			light(4);
			light(3);
			light(2);
			light(1);
			light(6);
			light(7);
		} // 9
	}
}

void main(void){
	char s;
	for (s = 0; s <= 9; s++){
		led_letter(s);
	}
}