#include <reg51.h>

char counter = 82; // 16 MHz -- 100s
//char counter = 64; // 24 MHz -- 60s
//char counter = 20; // 24 MHz -- 60s

void wait(){
	int w = 0;
	int ww = 0;
//	for (w = 0; w < 1024; w++){
//	for (w = 0; w < 2048; w++){
	for (w = 0; w < 4096; w++){
//	for (w = 0; w < 8192; w++){
//	for (w = 0; w < 16384; w++){
//	for (w = 0; w < 32768; w++){

//	for (w = 0; w < 256; w++){
		for (ww = 0; ww < 4; ww++){ ; }
	}
}

// void lwait() { int i; for (i = 0; i < 64; i++){ ; } }
void lwait() { int i; for (i = 0; i < 256; i++){ ; } }

void light(char value, char digit)
{
	P3 = digit;
	if (value == 0x0){ P1 =  0; lwait(); P1 =  4; lwait(); P1 =  8; lwait(); P1 = 12; lwait(); P1 = 16; lwait(); P1 = 20; lwait();} // 0
	if (value == 0x1){ P1 =  4; lwait(); P1 =  8; lwait(); P1 =  4; lwait(); P1 =  8; lwait(); P1 =  4; lwait(); P1 =  8; lwait();} // 1
	if (value == 0x2){ P1 =  0; lwait(); P1 =  4; lwait(); P1 = 12; lwait(); P1 = 16; lwait(); P1 = 24; lwait();} // 2
	if (value == 0x3){ P1 =  0; lwait(); P1 =  4; lwait(); P1 =  8; lwait(); P1 = 12; lwait(); P1 = 24; lwait();} // 3
	if (value == 0x4){ P1 =  4; lwait(); P1 =  8; lwait(); P1 = 20; lwait(); P1 = 24; lwait();} // 4
	if (value == 0x5){ P1 =  0; lwait(); P1 =  8; lwait(); P1 = 12; lwait(); P1 = 20; lwait(); P1 = 24; lwait();} // 5
	if (value == 0x6){ P1 =  0; lwait(); P1 =  8; lwait(); P1 = 12; lwait(); P1 = 16; lwait(); P1 = 20; lwait(); P1 = 24; lwait();} // 6
	if (value == 0x7){ P1 =  0; lwait(); P1 =  4; lwait(); P1 =  8; lwait(); P1 =  0; lwait(); P1 =  4; lwait(); P1 =  8; lwait();} // 7
	if (value == 0x8){ P1 =  0; lwait(); P1 =  4; lwait(); P1 =  8; lwait(); P1 = 12; lwait(); P1 = 16; lwait(); P1 = 20; lwait(); P1 = 24; lwait();} // 8
	if (value == 0x9){ P1 =  0; lwait(); P1 =  4; lwait(); P1 =  8; lwait(); P1 = 12; lwait(); P1 = 20; lwait(); P1 = 24; lwait();} // 9
	if (value == 0xa){ P1 =  0; lwait(); P1 =  4; lwait(); P1 =  8; lwait(); P1 = 16; lwait(); P1 = 20; lwait(); P1 = 24; lwait();} // A
	if (value == 0xb){ P1 =  8; lwait(); P1 = 12; lwait(); P1 = 16; lwait(); P1 = 20; lwait(); P1 = 24; lwait();} // b
	if (value == 0xc){ P1 =  0; lwait(); P1 = 12; lwait(); P1 = 16; lwait(); P1 = 20; lwait();} // c
	if (value == 0xd){ P1 =  4; lwait(); P1 =  8; lwait(); P1 = 12; lwait(); P1 = 16; lwait(); P1 = 24; lwait();} // d
	if (value == 0xe){ P1 =  0; lwait(); P1 = 12; lwait(); P1 = 16; lwait(); P1 = 20; lwait(); P1 = 24; lwait();} // E
	if (value == 0xf){ P1 =  0; lwait(); P1 = 16; lwait(); P1 = 20; lwait(); P1 = 24; lwait();} // F
}

void print_counter()
{
	char c1 =  counter & 0x0F;
	char c2 = (counter & 0xF0) >> 4;
	int i = 0;
	int j = 0;
	for (i = 0; i < 64; i++){
//	for (i = 0; i < 256; i++){
//	for (i = 0; i < 1024; i++){
//	for (i = 0; i < 2048; i++){
//	for (i = 0; i < 4096; i++){
//	for (i = 0; i < 8192; i++){
//	for (i = 0; i < 16384; i++){
//	for (i = 0; i < 32768; i++){
		for (j = 0; j < 2; j++){
			light (c2, 47);
			lwait();
			light (c1, 31); 
		}
	}
}

void boom()
{
	char i = 0;
	EA  = 0;
	while (1){ // DEAD
		P3 = 62; P1 =  4 ; lwait(); P1 =  8 ; lwait(); P1 = 12 ; lwait(); P1 = 16 ; lwait(); P1 = 24 ; lwait(); // d
		P3 = 61; P1 =  0 ; lwait(); P1 = 12 ; lwait(); P1 = 16 ; lwait(); P1 = 20 ; lwait(); P1 = 24 ; lwait(); // e
		P3 = 47; P1 =  0 ; lwait(); P1 =  4 ; lwait(); P1 =  8 ; lwait(); P1 = 16 ; lwait(); P1 = 20 ; lwait(); P1 = 24; lwait(); // a
		P3 = 31; P1 =  4 ; lwait(); P1 =  8 ; lwait(); P1 = 12 ; lwait(); P1 = 16 ; lwait(); P1 = 24 ; lwait(); // d

//		P3 = 62; for (i = 0; i < 16; i++) { P1 =  4 ; lwait(); P1 =  8 ; lwait(); P1 = 12 ; lwait(); P1 = 16 ; lwait(); P1 = 24 ; lwait();} // d
//		P3 = 61; for (i = 0; i < 16; i++) { P1 =  0 ; lwait(); P1 =  4 ; lwait(); P1 =  8 ; lwait(); P1 = 12 ; lwait(); P1 = 16 ; lwait(); P1 = 20 ; lwait(); P1 = 24 ; lwait();} // e
//		P3 = 47; for (i = 0; i < 16; i++) { P1 =  0 ; lwait(); P1 =  4 ; lwait(); P1 =  8 ; lwait(); P1 = 12 ; lwait(); P1 = 16 ; lwait(); P1 = 24 ; lwait(); P1 = 28 ; lwait();} // a
//		P3 = 31; for (i = 0; i < 16; i++) { P1 =  4 ; lwait(); P1 =  8 ; lwait(); P1 = 12 ; lwait(); P1 = 16 ; lwait(); P1 = 24 ; lwait();} // d
	}
}

void tictac()
{ 
	char i = 0;
	P3 = 62; for (i = 0; i < 16; i++) { P1 = 28 ; } wait();
	P3 = 61; for (i = 0; i < 16; i++) { P1 = 28 ; } wait();
	P3 = 47; for (i = 0; i < 16; i++) { P1 = 28 ; } wait();
	P3 = 31; for (i = 0; i < 16; i++) { P1 = 28 ; } wait();
}

void ooops(void) interrupt 0
{
	if (counter > 16) { counter = counter - 14; } 
	else {
		if (counter > 8) { counter = counter - 4; }
		else {
			if (counter > 1) { counter = counter - 1; };
		}
	}
	return;
}

void save(void) interrupt 2
{
	char i = 0;
	EA = 0;
	while (1){
		P3 = 62; P1 =  0 ; lwait(); P1 = 20 ; lwait(); P1 = 24 ; lwait(); P1 =  8 ; lwait(); P1 = 12 ; lwait(); //S
		P3 = 61; P1 =  0 ; lwait(); P1 =  4 ; lwait(); P1 =  8 ; lwait(); P1 = 16 ; lwait(); P1 = 20 ; lwait(); P1 = 24 ; lwait();  //A
		P3 = 47; P1 =  4 ; lwait(); P1 =  8 ; lwait(); P1 = 12 ; lwait(); P1 = 16 ; lwait(); P1 = 20 ; lwait(); P1 = 28 ; lwait();  //V
		P3 = 31; P1 =  0 ; lwait(); P1 = 12 ; lwait(); P1 = 16 ; lwait(); P1 = 20 ; lwait(); P1 = 24 ; lwait();  // E

//		P3 = 62; for (i = 0; i < 16; i++) { P1 =  0 ; lwait(); P1 = 20 ; lwait(); P1 = 24 ; lwait(); P1 =  8 ; lwait(); P1 = 12 ; } //s 
//		P3 = 61; for (i = 0; i < 16; i++) { P1 =  0 ; lwait(); P1 =  4 ; lwait(); P1 =  8 ; lwait(); P1 = 16 ; lwait(); P1 = 20 ; lwait(); P1 = 24 ; }
//		P3 = 47; for (i = 0; i < 16; i++) { P1 = 20 ; lwait(); P1 = 16 ; lwait(); P1 = 12 ; lwait(); P1 =  8 ; lwait(); P1 =  4 ; lwait(); P1 = 28 ; }
//		P3 = 31; for (i = 0; i < 16; i++) { P1 =  0 ; lwait(); P1 = 20 ; lwait(); P1 = 16 ; lwait(); P1 = 12 ; lwait(); P1 = 24 ; }

	}
}

void main(void){
	EA  = 1;
	EX0 = 1;
	EX1 = 1;
	IT0 = 1;
	IT1 = 1;
	while (counter > 1) {
		tictac();
		if (counter > 1) { counter = counter - 1 ; }
		wait(); 
		print_counter(); 
		wait();
	}
	boom();
}