(require '[c51cc.parser :as parser])
(require '[c51cc.lexer :as lexer])

(defn test-parse-advanced
  "Тестирование сложных случаев с детальным анализом"
  [code description expected-success]
  (println (str "\n=== " description " ==="))
  (println (str "Код: " code))
  (println (str "Ожидается успех: " expected-success))
  (try
    (let [tokens (lexer/tokenize code)
          result (parser/parse tokens)]
      (println (str "Токенов: " (count tokens)))
      (println (str "Фактический успех: " (:success result)))
      (if (:success result)
        (let [ast (:ast result)]
          (println (str "AST тип: " (:ast-type ast)))
          (when (:declarations ast)
            (println (str "Объявлений: " (count (:declarations ast)))))
          (when (parser/validate-ast ast)
            (println "✓ AST валиден"))
          (println "✓ Парсинг успешен"))
        (println (str "Ошибка: " (:error result))))
      (= (:success result) expected-success))
    (catch Exception e
      (println (str "Исключение: " (.getMessage e)))
      false)))

(println "=== ТЕСТИРОВАНИЕ СЛОЖНЫХ СЛУЧАЕВ ПАРСЕРА C51 ===")

;; Тест 1: Глубокая вложенность выражений
(test-parse-advanced 
  "((((a + b) * c) - d) / e)" 
  "Глубокая вложенность скобок" 
  true)

;; Тест 2: Сложные операторы присваивания
(test-parse-advanced 
  "x += y *= z /= w %= v;" 
  "Цепочка составных операторов присваивания" 
  true)

;; Тест 3: Многомерные массивы
(test-parse-advanced 
  "int matrix[10][20][30];" 
  "Трехмерный массив" 
  true)

;; Тест 4: Указатели на функции
(test-parse-advanced 
  "int (*func_ptr)(int, char*);" 
  "Указатель на функцию" 
  true)

;; Тест 5: Структуры с битовыми полями
(test-parse-advanced 
  "struct { unsigned int a:3; unsigned int b:5; } flags;" 
  "Структура с битовыми полями" 
  true)

;; Тест 6: Вложенные циклы и условия
(test-parse-advanced 
  "for (i = 0; i < 10; i++) { 
     for (j = 0; j < 20; j++) { 
       if (matrix[i][j] > threshold) { 
         if (flag) break; 
         else continue; 
       } 
     } 
   }" 
  "Глубоко вложенные циклы и условия" 
  true)

;; Тест 7: Специфичные для 8051 регистры
(test-parse-advanced 
  "TMOD = 0x01; TH0 = 0xFC; TL0 = 0x18; TR0 = 1;" 
  "Работа с таймерами 8051" 
  true)

;; Тест 8: Прерывания
(test-parse-advanced 
  "void timer0_isr() interrupt 1 { P1 ^= 0x01; }" 
  "Функция обработки прерывания" 
  true)

;; Тест 9: Модификаторы памяти
(test-parse-advanced 
  "unsigned char xdata buffer[256]; 
   unsigned char code lookup_table[] = {0, 1, 4, 9, 16};" 
  "Модификаторы памяти xdata и code" 
  true)

;; Тест 10: Битовые переменные
(test-parse-advanced 
  "bit flag1, flag2; sbit LED = P1^0;" 
  "Битовые переменные и sbit" 
  true)

;; Тест 11: Макросы и препроцессор (должен игнорироваться)
(test-parse-advanced 
  "#define LED_ON P1 |= 0x01
   #define LED_OFF P1 &= ~0x01
   void toggle_led() { LED_ON; delay(100); LED_OFF; }" 
  "Код с макросами препроцессора" 
  true)

;; Тест 12: Очень длинное выражение
(test-parse-advanced 
  "result = a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + 
           b1 * b2 * b3 * b4 * b5 * b6 * b7 * b8 * b9 * b10;" 
  "Очень длинное арифметическое выражение" 
  true)

;; Тест 13: Ошибочный синтаксис (должен провалиться)
(test-parse-advanced 
  "int x = ;" 
  "Неполное объявление (ошибка)" 
  false)

;; Тест 14: Несбалансированные скобки (должен провалиться)
(test-parse-advanced 
  "if (x > 0 { return x; }" 
  "Несбалансированные скобки (ошибка)" 
  false)

;; Тест 15: Неизвестный тип (должен провалиться)
(test-parse-advanced 
  "unknown_type variable;" 
  "Неизвестный тип данных (ошибка)" 
  false)

;; Тест 16: Реальная программа для 8051
(test-parse-advanced 
  "
#include <reg51.h>

unsigned char code seven_seg[] = {
  0x3F, 0x06, 0x5B, 0x4F, 0x66, 0x6D, 0x7D, 0x07,
  0x7F, 0x6F
};

void delay(unsigned int ms) {
  unsigned int i, j;
  for (i = 0; i < ms; i++) {
    for (j = 0; j < 1000; j++);
  }
}

void display_digit(unsigned char digit) {
  if (digit < 10) {
    P0 = seven_seg[digit];
  }
}

void main() {
  unsigned char counter = 0;
  
  while (1) {
    display_digit(counter);
    delay(1000);
    counter++;
    if (counter > 9) counter = 0;
  }
}
" 
  "Полная программа семисегментного индикатора" 
  true)

(println "\n=== ТЕСТИРОВАНИЕ СЛОЖНЫХ СЛУЧАЕВ ЗАВЕРШЕНО ===")

;; Дополнительные тесты производительности
(println "\n=== ТЕСТЫ ПРОИЗВОДИТЕЛЬНОСТИ ===")

(defn performance-test [code description iterations]
  (println (str "\n--- " description " ---"))
  (let [start-time (System/currentTimeMillis)]
    (dotimes [i iterations]
      (let [tokens (lexer/tokenize code)]
        (parser/parse tokens)))
    (let [end-time (System/currentTimeMillis)
          total-time (- end-time start-time)]
      (println (str "Итераций: " iterations))
      (println (str "Общее время: " total-time " мс"))
      (println (str "Время на итерацию: " (/ total-time iterations) " мс")))))

(performance-test "int x = 42;" "Простое объявление" 1000)
(performance-test "for (i = 0; i < 100; i++) sum += array[i];" "Цикл for" 1000)
(performance-test "
void complex_function(int a, int b, int c) {
  int result = 0;
  for (int i = 0; i < a; i++) {
    for (int j = 0; j < b; j++) {
      if ((i + j) % c == 0) {
        result += i * j;
      }
    }
  }
  return result;
}" "Сложная функция" 100)

(println "\n=== ВСЕ ТЕСТЫ ЗАВЕРШЕНЫ ===") 