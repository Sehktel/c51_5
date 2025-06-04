/* Строгий тест C89 для AT89S4051 микроконтроллера */
/* Только поддерживаемые типы: char(8 бит), int(16 бит) */
/* БЕЗ: long, short, float, double, printf, scanf */

#define F_CPU 12000000U    /* Частота кристалла 12 МГц (unsigned int) */
#define BAUD_RATE 9600     /* Скорость UART */
#define MAX_COUNT 255      /* Максимальное значение для unsigned char */

/* Глобальные переменные - только допустимые типы */
char temperature;               /* 8-битное знаковое число */
unsigned char port_state;      /* 8-битное беззнаковое число */  
int sensor_value;               /* 16-битное знаковое число */
unsigned int timer_counter;    /* 16-битное беззнаковое число */
static char error_code;        /* статическая переменная */
register unsigned char reg_temp; /* переменная в регистре */

/* Специальные регистры AT89S4051 */
sfr P1 = 0x90;                 /* Порт 1 */
sfr P3 = 0xB0;                 /* Порт 3 */
sfr TMOD = 0x89;               /* Режим таймера */
sfr TCON = 0x88;               /* Управление таймером */
sbit LED = P1^0;               /* Светодиод на P1.0 */
sbit BUTTON = P3^2;            /* Кнопка на P3.2 */

/* Перечисления C89 */
enum system_state {
    STATE_INIT = 0,
    STATE_RUNNING = 1,
    STATE_ERROR = 2,
    STATE_SLEEP = 3
};

/* Структуры C89 */
struct sensor_reading {
    unsigned char id;           /* ID датчика */
    int value;                  /* Значение датчика */
    enum system_state status;   /* Состояние */
};

/* Объединения C89 */
union data_word {
    unsigned int word;          /* 16-битное слово */
    struct {
        unsigned char low;      /* Младший байт */
        unsigned char high;     /* Старший байт */  
    } bytes;
};

/* Функция задержки - только целые числа */
void delay_cycles(unsigned int cycles) {
    unsigned int i;
    for(i = 0; i < cycles; i++) {
        /* Пустой цикл для задержки */
    }
}

/* Математические операции без float */
unsigned int scale_value(unsigned char input) {
    /* Умножение на 10 для эмуляции фиксированной запятой */
    return (unsigned int)input * 10;
}

/* Целочисленное деление с проверкой */
unsigned char safe_divide(unsigned int dividend, unsigned char divisor) {
    if(divisor == 0) {
        error_code = -1;  /* Ошибка деления на ноль */
        return 0;
    }
    return (unsigned char)(dividend / divisor);
}

/* Битовые операции */
unsigned char set_bit(unsigned char value, unsigned char bit_num) {
    return value | (1 << bit_num);
}

unsigned char clear_bit(unsigned char value, unsigned char bit_num) {
    return value & ~(1 << bit_num);
}

/* Функция инициализации */
void init_system(void) {
    /* Настройка портов */
    P1 = 0x00;          /* Очистка порта 1 */
    P3 = 0xFF;          /* Подтяжка порта 3 */
    
    /* Инициализация переменных */
    temperature = 25;    /* 25 градусов */
    port_state = 0x00;
    sensor_value = 0;
    timer_counter = 0;
    error_code = 0;
}

/* Чтение датчика температуры */
int read_temperature(void) {
    /* Эмуляция АЦП - возвращаем значение в десятых долях градуса */
    static unsigned char adc_counter = 0;
    adc_counter++;
    
    /* Простая эмуляция изменения температуры */
    if(adc_counter > 50) {
        adc_counter = 0;
        temperature = (temperature < 30) ? temperature + 1 : 20;
    }
    
    return (int)temperature * 10;  /* Возвращаем в десятых долях */
}

/* Обработка кнопки */
void process_button(void) {
    static unsigned char button_state = 1;  /* Предыдущее состояние */
    unsigned char current_state;
    
    current_state = BUTTON;
    
    /* Обнаружение нажатия (переход 1->0) */
    if(button_state == 1 && current_state == 0) {
        /* Кнопка нажата - переключаем LED */
        LED = !LED;
        port_state = LED ? 0xFF : 0x00;
    }
    
    button_state = current_state;
}

/* Главная функция */
int main(void) {
    struct sensor_reading sensor;
    union data_word data;
    unsigned char loop_counter;
    enum system_state current_state;
    
    /* Инициализация системы */
    init_system();
    current_state = STATE_INIT;
    
    /* Настройка датчика */
    sensor.id = 1;
    sensor.value = 0;
    sensor.status = STATE_RUNNING;
    
    loop_counter = 0;
    
    /* Основной цикл программы */
    while(1) {
        /* Обработка состояний */
        switch(current_state) {
            case STATE_INIT:
                LED = 1;
                delay_cycles(1000);
                LED = 0;
                current_state = STATE_RUNNING;
                break;
                
            case STATE_RUNNING:
                /* Чтение датчика */
                sensor.value = read_temperature();
                
                /* Обработка кнопки */
                process_button();
                
                /* Обновление данных */
                data.word = (unsigned int)sensor.value;
                port_state = data.bytes.high;
                
                /* Проверка на переполнение */
                if(loop_counter < MAX_COUNT) {
                    loop_counter++;
                } else {
                    loop_counter = 0;
                    /* Мигание для индикации цикла */
                    LED = 1;
                    delay_cycles(100);
                    LED = 0;
                }
                
                /* Задержка основного цикла */
                delay_cycles(5000);
                break;
                
            case STATE_ERROR:
                /* Быстрое мигание в случае ошибки */
                LED = 1;
                delay_cycles(200);
                LED = 0;
                delay_cycles(200);
                
                /* Возврат к работе после ошибки */
                if(loop_counter > 10) {
                    current_state = STATE_RUNNING;
                    error_code = 0;
                }
                break;
                
            case STATE_SLEEP:
                /* Энергосберегающий режим */
                LED = 0;
                P1 = 0x00;
                delay_cycles(10000);
                break;
                
            default:
                current_state = STATE_ERROR;
                break;
        }
        
        /* Проверка ошибок */
        if(error_code != 0) {
            current_state = STATE_ERROR;
        }
    }
    
    return 0;
} 