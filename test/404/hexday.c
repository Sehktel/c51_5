#include <reg51.h>

void lwait() { int i; for (i = 0; i < 8; i++){ ; } }

void hexday(bit flag){
	int  i = 0;

	if (flag){ // 5F54
		for (i = 0; i < 16; i++)
		{   P3 = 62;   P1 =  0 ;  lwait();  P1 =  8 ;  lwait();  P1 = 12 ;  lwait();  P1 = 20 ;  lwait();  P1 = 24 ;  lwait(); }
		for (i = 0; i < 16; i++) //F
		{   P3 = 61;   P1 =  0 ;  lwait();  P1 = 16 ;  lwait();  P1 = 20 ;  lwait();  P1 = 24 ;  lwait(); }
		for (i = 0; i < 16; i++) //5
		{   P3 = 47;   P1 =  0 ;  lwait();  P1 =  8 ;  lwait();  P1 = 12 ;  lwait();  P1 = 20 ;  lwait();  P1 = 24 ;  lwait(); }
		for (i = 0; i < 16; i++) //4
		{   P3 = 31;   P1 =  4 ;  lwait();  P1 =  8 ;  lwait();  P1 = 20 ;  lwait();  P1 = 24 ;  lwait(); }
	}
	else{ // 9DE8
		for (i = 0; i < 16; i++) // 9
		{   P3 = 62;   P1 =  0 ;  lwait();  P1 =  4 ;  lwait();  P1 =  8 ;  lwait();  P1 = 12 ;  lwait();  P1 = 20 ;  lwait();  P1 = 24 ;  lwait(); }
		for (i = 0; i < 16; i++) // d
		{   P3 = 61;   P1 =  4 ;  lwait();  P1 =  8 ;  lwait();  P1 = 12 ;  lwait();  P1 = 16 ;  lwait();  P1 = 24 ;  lwait(); }
		for (i = 0; i < 16; i++) // E
		{   P3 = 47;   P1 =  0 ;  lwait();  P1 = 12 ;  lwait();  P1 = 16 ;  lwait();  P1 = 20 ;  lwait();  P1 = 24 ;  lwait(); }
		for (i = 0; i < 16; i++) // 8
		{   P3 = 31;   P1 =  0 ;  	lwait();  P1 =  4 ;  lwait();  P1 =  8 ;  lwait();  P1 = 12 ;  lwait();  P1 = 16 ;  lwait();  P1 = 20 ;  lwait();  P1 = 24 ;  lwait();  }
	}
}

void main(void)
{	
	while(1){
		hexday(1);
	}
}
