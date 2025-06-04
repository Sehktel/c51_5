#include <reg51.h>

//void wait(){
//	int w = 0;
//	int ww = 0;
//	for (w = 0; w < 1024; w++){
//	for (w = 0; w < 2048; w++){
//	for (w = 0; w < 4096; w++){
//	for (w = 0; w < 8192; w++){
//	for (w = 0; w < 16384; w++){
//	for (w = 0; w < 32768; w++){

//	for (w = 0; w < 256; w++){
//		for (ww = 0; ww < 128; ww++){
//			;
//		}
//	}
//}

void light(char s, d){
	if (s == 1){ P1 =  0 ;} 
	if (s == 2){ P1 =  4 ;}
	if (s == 3){ P1 =  8 ;}
	if (s == 4){ P1 = 12 ;}
	if (s == 5){ P1 = 16 ;}
	if (s == 6){ P1 = 20 ;}
	if (s == 7){ P1 = 24 ;}
	if (s == 8){ P1 = 28 ;}

	if (d == 1){ P3 = 62 ;}
	if (d == 2){ P3 = 61 ;}
	if (d == 3){ P3 = 47 ;}
	if (d == 4){ P3 = 31 ;}
}

void printb(){
	int  i = 0;
	for (i = 0; i < 16; i++)
	{
		light(3, 1);
		light(4, 1);
		light(5, 1);
		light(6, 1);
		light(7, 1);
	}
}

void printd(){
	int  i = 0;
	for (i = 0; i < 16; i++)
	{
		light(2, 3);
		light(3, 3);
		light(4, 3);
		light(5, 3);
		light(7, 3);
	}
	//wait();
}

void printf() interrupt 0
{
	int  i = 0;
	for (i = 0; i < 2048; i++)
	{
		light(1, 4);
		light(5, 4);
		light(6, 4);
		light(7, 4);
	}
	return;
}

void print2() interrupt 2
{
	int  i = 0;
	for (i = 0; i < 2048; i++)
	{
		light(1, 4);
		light(2, 4);
		light(4, 4);
		light(5, 4);
		light(7, 4);
	}
	return;
}

void main(void)
{
	EA  = 1;
	EX0 = 1;
	EX1 = 1;
	IT0 = 1;
	IT1 = 1;
	
	while(1){
		printd();
		light(7,2);
		light(7,2);
		light(7,2);
		light(7,2);
		light(7,2);
		light(7,2);
		light(7,2);
		light(7,2);
		printb();
	}
}
