#include <reg2051.h>


/*
9. Flag.
The letters FLAG light up on the display, but some of them are upside down, others are mirrored, and one section is missing.
The task is to correct the sections and get the word FLAG.
The solution is to change the sections -- or get a mirror! =)
*/

void flag(){
	int i = 0;
	P3 = 62; 
	for (i = 0; i < 16; i++) 
		{ P1 =  0 ;  P1 =  4 ;  P1 =  8 ;  P1 = 24 ; } // F -- mirror 
//		{ P1 =  0 ;  P1 = 20 ;  P1 = 16 ;  P1 = 24 ; } // F -- correct
	
	P3 = 61;
	for (i = 0; i < 16; i++) // L 
		{  P1 = 12 ;  P1 = 16 ;  P1 = 20 ;  }

	P3 = 47;
	for (i = 0; i < 16; i++) 
		{ P1 =  4 ;  P1 =  8 ;  P1 = 12 ;  P1 = 16 ;  P1 = 20 ;  P1 = 24 ; } // A -- upsidedown
//		{ P1 =  4 ;  P1 =  8 ;  P1 =  0 ;  P1 = 16 ;  P1 = 20 ;  P1 = 24 ; } // A -- correct
	
	P3 = 31;
	for (i = 0; i < 16; i++) // G --> 9.
		{ P1 =  0 ;  P1 =  4 ;  P1 =  8 ;  P1 = 12 ;  P1 = 16 ;  P1 = 20 ;  } // G as 0
//		{ P1 =  0 ;  P1 =  4 ;  P1 =  8 ;  P1 = 12 ;  P1 = 24 ;  P1 = 20 ;  } // G - correct
}

void main(void)
{	
	while(1){
		flag();
	}
}

/*
void flag(){
	int i = 0;
	for (i = 0; i < 16; i++) 
//	{   P3 = 62;   P1 =  0 ;  P1 =  4 ;  P1 =  8 ;  P1 = 24 ; } // F -- mirror 
	{   P3 = 62;   P1 =  0 ;  P1 = 20 ;  P1 = 16 ;  P1 = 24 ; } // F -- correct
	for (i = 0; i < 16; i++) // L 
	{   P3 = 61;   P1 = 12 ;  P1 = 16 ;  P1 = 20 ;  }
	for (i = 0; i < 16; i++) 
//	{   P3 = 47;   P1 =  4 ;  P1 =  8 ;  P1 = 12 ;  P1 = 16 ;  P1 = 20 ;  P1 = 24 ; } // A -- upsidedown
	{   P3 = 47;   P1 =  4 ;  P1 =  8 ;  P1 =  0 ;  P1 = 16 ;  P1 = 20 ;  P1 = 24 ; } // A -- correct
	for (i = 0; i < 16; i++) // G --> 9.
//	{   P3 = 31;   P1 =  0 ;  P1 =  4 ;  P1 =  8 ;  P1 = 12 ;  P1 = 16 ;  P1 = 20 ;  } // G as 0
	{   P3 = 31;   P1 =  0 ;  P1 =  4 ;  P1 =  8 ;  P1 = 12 ;  P1 = 24 ;  P1 = 20 ;  } // G - correct
}
*/