#include <reg2051.h>

void lwait() { int i; for (i = 0; i < 8; i++){ ; } }

void p0404reverse() interrupt 0
{
	int  i = 0;
	int  j = 0;
	int  k = 0;
//	for (j = 0; j < 4; j++){
		for (i = 0; i < 128; i++)
		{
			for (k = 0; k < 6; k++) {   P3 = 62;   P1 =  4;  lwait();  P1 = 16 ;  lwait();  P1 = 20 ;  lwait();  P1 =  24 ;  lwait(); } // 4
			for (k = 0; k < 6; k++) {   P3 = 61;   P1 =  0;  lwait();  P1 =  4 ;  lwait();  P1 =  8 ;  lwait();  P1 =  12 ;  lwait();  P1 =  16 ;  lwait();  P1 =  20 ;  lwait(); } //0
			for (k = 0; k < 6; k++) {   P3 = 47;   P1 =  4;  lwait();  P1 = 16 ;  lwait();  P1 = 20 ;  lwait();  P1 =  24 ;  lwait(); } // 4
			for (k = 0; k < 6; k++) {   P3 = 31;   P1 =  0;  lwait();  P1 =  4 ;  lwait();  P1 =  8 ;  lwait();  P1 =  12 ;  lwait();  P1 =  16 ;  lwait();  P1 =  20 ;  lwait(); } //0
		}
//	}
}

void p2024reverse() interrupt 2
{
	int  i = 0;
	int  j = 0;
	int  k = 0;
//	for (j = 0; j < 4; j++){
		for (i = 0; i < 128; i++)
		{
			for (k = 0; k < 6; k++) {   P3 = 62;   P1 =  4;  lwait();  P1 = 16 ;  lwait();  P1 =  20 ;  lwait();  P1 =  24 ;  lwait(); } //4 reverse !!
			for (k = 0; k < 6; k++) {   P3 = 61;   P1 =  0;  lwait();  P1 =  8 ;  lwait();  P1 =  12 ;  lwait();  P1 =  20 ;  lwait();  P1 =  24 ;  lwait(); } // 2 reverse !!
			for (k = 0; k < 6; k++) {   P3 = 47;   P1 =  0;  lwait();  P1 =  4 ;  lwait();  P1 =   8 ;  lwait();  P1 =  12 ;  lwait();  P1 =  16 ;  lwait();  P1 =  20 ;  lwait(); }
			for (k = 0; k < 6; k++) {   P3 = 31;   P1 =  0;  lwait();  P1 =  8 ;  lwait();  P1 =  12 ;  lwait();  P1 =  20 ;  lwait();  P1 =  24 ;  lwait(); } // 2 reverse !! 
		}
//	}
}

void p0404straight()
{
	int  i = 0;
	int  j = 0;
	int  k = 0;
//	for (j = 0; j < 4; j++){
		for (i = 0; i < 128; i++)
		{
			for (k = 0; k < 6; k++) {   P3 = 62;   P1 =  0;  lwait();  P1 =  4 ;  lwait();  P1 =  8 ;  lwait();  P1 =  12 ;  lwait();  P1 =  16 ;  lwait();  P1 =  20 ;  lwait(); } // 0
			for (k = 0; k < 6; k++) {   P3 = 61;   P1 =  4;  lwait();  P1 =  8 ;  lwait();  P1 = 20 ;  lwait();  P1 =  24 ;  lwait(); } //4
			for (k = 0; k < 6; k++) {   P3 = 47;   P1 =  0;  lwait();  P1 =  4 ;  lwait();  P1 =  8 ;  lwait();  P1 =  12 ;  lwait();  P1 =  16 ;  lwait();  P1 =  20 ;  lwait(); } // 0
			for (k = 0; k < 6; k++) {   P3 = 31;   P1 =  4;  lwait();  P1 =  8 ;  lwait();  P1 = 20 ;  lwait();  P1 =  24 ;  lwait(); } //4
		}
//	}
}

void p2024straight()
{
	int  i = 0;
	int  j = 0;
	int  k = 0;
//	for (j = 0; j < 4; j++){
		for (i = 0; i < 128; i++)
		{
			for (k = 0; k < 6; k++) {   P3 = 62;   P1 =  0;  lwait();  P1 = 16 ;  lwait();  P1 =  12 ;  lwait();  P1 =   8 ;  lwait();  P1 =  24 ;  lwait(); } //2
			for (k = 0; k < 6; k++) {   P3 = 61;   P1 =  0;  lwait();  P1 =  4 ;  lwait();  P1 =   8 ;  lwait();  P1 =  12 ;  lwait();  P1 =  16 ;  lwait();  P1 =  20 ;  lwait(); } // 0
			for (k = 0; k < 6; k++) {   P3 = 47;   P1 =  0;  lwait();  P1 = 16 ;  lwait();  P1 =  12 ;  lwait();  P1 =   8 ;  lwait();  P1 =  24 ;  lwait(); } // 2
			for (k = 0; k < 6; k++) {   P3 = 31;   P1 =  4;  lwait();  P1 =  9 ;  lwait();  P1 =  20 ;  lwait();  P1 =  24 ;  lwait(); } // 4 
		}
//	}
}

void dash4(){
	int  i = 0;
	for (i = 0; i < 6; i++) {   P3 = 62;   P1 = 24 ; }
	for (i = 0; i < 6; i++) {   P3 = 61;   P1 = 24 ; }
	for (i = 0; i < 6; i++) {   P3 = 47;   P1 = 24 ; }
	for (i = 0; i < 6; i++) {   P3 = 31;   P1 = 24 ; }
}

void main(void)
{	
	EA  = 1;
	EX0 = 1;
	EX1 = 1;
	IT0 = 1;
	IT1 = 1;
	while(1){
		dash4();
	}
}
