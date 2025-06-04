# include <reg2051.h>

void wait(){
	int i = 0;
	int j = 0;
//	for (i = 0; i < 128; i++){
//	for (i = 0; i < 256; i++){
//	for (i = 0; i < 512; i++){
//	for (i = 0; i < 1024; i++){
	for (i = 0; i < 2048; i++){
//	for (i = 0; i < 4096; i++){
//	for (i = 0; i < 8192; i++){
//	for (i = 0; i < 16384; i++){
//	for (i = 0; i < 32768; i++){
		
		for (j = 0; j < 4; j++){
//		for (j = 0; j < 8; j++){
//		for (j = 0; j < 16; j++){
//		for (j = 0; j < 32; j++){
//		for (j = 0; j < 64; j++){
			;
		}
	}
}


void light(char s, char d){
	if (s == 1){ P1 = 0;} 
	if (s == 2){ P1 = 4;}
	if (s == 3){ P1 = 8;}
	if (s == 4){ P1 = 12;}
	if (s == 5){ P1 = 16;}
	if (s == 6){ P1 = 20;}
	if (s == 7){ P1 = 24;}
	if (s == 8){ P1 = 28;}

	if (d == 1){P3 = 62;}
	if (d == 2){P3 = 61;}
	if (d == 3){P3 = 47;}
	if (d == 4){P3 = 31;}
	wait();
}

void main(void){
	while(1){
		light(1, 1);
		light(1, 2);
		light(1, 3);
		light(1, 4);
		light(2, 4);
		light(2, 4);
		light(4, 4);
		light(4, 3);
		light(4, 2);
		light(4, 1);
		light(5, 1);
		light(6, 1);
	}
}