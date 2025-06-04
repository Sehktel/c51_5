#include <reg2051.h>

bit snake_flag = 0;
int digits[4]; // 0, 1, 2
int digit = 1; // 0, 1, 2, 3

void lwait() { int i; for (i = 0; i < 256; i++){ ; } }

void wait(void){
	int i = 0;
	int j = 0;
	for (i = 0; i < 512; i++){
		for (j = 0; j < 4; j++){
			;
		}
	}
}

void changeSegment(void) interrupt 0
{ 
	digits[digit] = digits[digit] + 1;
	if (digits[digit] >= 4){ digits[digit] = 1; }
}

void changeDigit(void) interrupt 2
{ 
	digit = digit + 1;
	if (digit >= 4){ digit = 0; }
}

int dash(char d){
	if (digits[d] == 1) { return  0; }
	if (digits[d] == 2) { return 24; }
	if (digits[d] == 3) { return 12; }
	return 8;
}

void game(void){
	if (digit == 0) { P1 = 28; P3 = 62; lwait(); }
	if (digit == 1) { P1 = 28; P3 = 61; lwait(); }
	if (digit == 2) { P1 = 28; P3 = 47; lwait(); }
	if (digit == 3) { P1 = 28; P3 = 31; lwait(); }

	P1 = dash(0); P3 = 62; lwait(); 
	P1 = dash(1); P3 = 61; lwait();
	P1 = dash(2); P3 = 47; lwait();
	P1 = dash(3); P3 = 31; lwait();
}

void init(void){
	digits[0] = 2;
	digits[1] = 2;
	digits[2] = 2;
	digits[3] = 2;
	snake_flag = 0;
}

void check(void){
	if (digits[0] == 3) {
	if (digits[1] == 2) {
	if (digits[2] == 1) {
	if (digits[3] == 2) {
		snake_flag = 1;
	}}}}

}

void light(int s, int d){
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

void snake(void){
	while(1){
		light(1, 1);
		light(1, 2);
		light(1, 3);
		light(1, 4);
		light(2, 4);
		light(3, 4);
		light(4, 4);
		light(4, 3);
		light(4, 2);
		light(4, 1);
		light(5, 1);
		light(6, 1);
	}
}

void main(void)
{
	EA  = 1;
	EX0 = 1;
	EX1 = 1;
	IT0 = 1;
	IT1 = 1;
	
	init();
	while(1){ 
		game();
		check();
		if (snake_flag) { snake(); }
	}
}
