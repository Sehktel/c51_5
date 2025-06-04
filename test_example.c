// Простой тест для HCL компилятора
// Тестируем базовую функциональность

#define LED_PORT P1
#define DELAY_TIME 1000

// Глобальные переменные
int counter = 0;
char status = 'R';

// Функция задержки
void delay(int ms) {
    int i, j;
    for(i = 0; i < ms; i++) {
        for(j = 0; j < 100; j++) {
            // пустой цикл
        }
    }
}

// Главная функция
int main(void) {
    // Инициализация портов
    LED_PORT = 0x00;
    
    while(1) {
        // Включить светодиод
        LED_PORT = 0xFF;
        delay(DELAY_TIME);
        
        // Выключить светодиод
        LED_PORT = 0x00;
        delay(DELAY_TIME);
        
        counter++;
    }
    
    return 0;
} 