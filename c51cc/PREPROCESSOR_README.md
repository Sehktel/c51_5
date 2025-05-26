# Препроцессор C51

## Обзор

Полнофункциональный препроцессор для компилятора C51, реализованный на Clojure. Поддерживает стандарт C89 и специфичные для микроконтроллера AT89S4051 возможности.

## Основные возможности

### 1. Простые макросы (#define)
```c
#define MAX_SIZE 100
#define PI 3.14159
#define GREETING "Hello, World!"
```

### 2. Функциональные макросы
```c
#define MAX(a, b) ((a) > (b) ? (a) : (b))
#define SET_BIT(reg, bit) ((reg) |= (1 << (bit)))
#define HIGH_BYTE(x) ((uint8_t)(((x) >> 8) & 0xFF))
```

### 3. Условная компиляция
```c
#ifdef DEBUG_MODE
    printf("Debug information");
#else
    // Release code
#endif

#ifndef FEATURE_DISABLED
    void feature_function();
#endif

#if VERSION >= 2
    #define NEW_FEATURES_ENABLED
#endif
```

### 4. Включение файлов (#include)
```c
#include "c51_types.h"    // Пользовательские заголовки
#include <stdio.h>        // Системные заголовки (поиск в системных путях)
```

### 5. Защитные макросы (Include Guards)
Автоматическое создание защитных макросов для предотвращения повторного включения:
```c
#ifndef __FILENAME_H__
#define __FILENAME_H__
// содержимое файла
#endif // __FILENAME_H__
```

### 6. Предопределенные макросы
- `__LINE__` - номер текущей строки
- `__FILE__` - имя текущего файла
- `__DATE__` - дата компиляции
- `__TIME__` - время компиляции
- `__STDC__` - соответствие стандарту C
- `__C51__` - специфично для C51

### 7. Обработка ошибок
```c
#error "Неподдерживаемая конфигурация"
#warning "Устаревшая функция"
```

## Специализация для AT89S4051

### Адаптированные типы данных
```c
typedef unsigned char  uint8_t;   // 8-битное беззнаковое
typedef signed char    int8_t;    // 8-битное знаковое
typedef unsigned int   uint16_t;  // 16-битное беззнаковое (максимум)
typedef signed int     int16_t;   // 16-битное знаковое
// НЕТ 32-битных типов (long) - неподходящие для AT89S4051
```

### Доступные порты
```c
sfr P1 = 0x90;  // Порт P1 (полностью доступен)
sfr P3 = 0xB0;  // Порт P3 (частично доступен)
// НЕТ портов P0, P2 - недоступны в AT89S4051
```

### Оптимизированные макросы
```c
#define DELAY_MS(ms) for(volatile uint8_t i = 0; i < (ms); i++) \
                         for(volatile uint8_t j = 0; j < 100; j++)

#define LED_TOGGLE(pin) TOGGLE_BIT(P1, pin)
#define HIGH_BYTE(x) ((uint8_t)(((x) >> 8) & 0xFF))
#define LOW_BYTE(x) ((uint8_t)((x) & 0xFF))
```

## Архитектура препроцессора

### Состояние препроцессора
```clojure
{:defines {}           ;; Определенные макросы
 :include-stack []     ;; Стек включаемых файлов
 :include-guards #{}   ;; Защитные макросы
 :include-paths ["." "include" "lib"]  ;; Пути поиска
 :conditional-stack [] ;; Стек условной компиляции
 :line-number 1        ;; Номер строки
 :current-file ""}     ;; Текущий файл
```

### Основные функции

#### `preprocess [code options]`
Главная функция препроцессирования:
```clojure
(preprocess source-code {:current-file "main.c"
                        :include-paths ["." "include"]
                        :defines {"DEBUG" "1"}})
```

#### `remove-comments [code]`
Удаляет комментарии с сохранением номеров строк:
```clojure
(remove-comments "/* комментарий */ код // строчный комментарий")
```

#### `wrap-with-include-guard [content filename]`
Создает защитные макросы:
```clojure
(wrap-with-include-guard "typedef int mytype;" "types.h")
;; => "#ifndef __TYPES_H__\n#define __TYPES_H__\n\ntypedef int mytype;\n\n#endif"
```

## Примеры использования

### Базовое использование
```clojure
(require '[c51cc.preprocessor :as pp])

(def source "#define LED_PIN 0\nvoid main() { LED_TOGGLE(LED_PIN); }")
(def result (pp/preprocess source))
```

### С включением файлов
```clojure
(def result (pp/preprocess source-code 
                          {:include-paths ["." "include" "lib"]
                           :current-file "main.c"}))
```

### С предопределенными макросами
```clojure
(def result (pp/preprocess source-code 
                          {:defines {"DEBUG" "1" 
                                   "VERSION" "2"}}))
```

## Ограничения и особенности

### Поддерживаемые директивы
- ✅ `#include` - включение файлов
- ✅ `#define` - определение макросов
- ✅ `#undef` - отмена определения
- ✅ `#ifdef` / `#ifndef` - условная компиляция
- ✅ `#if` / `#elif` / `#else` / `#endif` - условные блоки
- ✅ `#error` / `#warning` - сообщения об ошибках
- ❌ `#pragma` - не реализовано
- ❌ `#line` - не реализовано

### Ограничения функциональных макросов
- Базовая поддержка параметров
- Нет поддержки вариативных макросов (`...`)
- Нет поддержки операторов `#` и `##`

### Вычисление условий
- Поддержка `defined(MACRO)`
- Простые числовые константы
- Базовые операторы сравнения
- Нет полной поддержки арифметических выражений

## Тестирование

Запуск тестов:
```bash
clojure -M test_preprocessor.clj      # Общие тесты
clojure -M test_at89s4051.clj        # Тесты для AT89S4051
```

## Интеграция с компилятором

Препроцессор интегрирован в основной pipeline компилятора:
```clojure
(defn compile-c51 [source-file]
  (-> source-file
      slurp
      preprocessor/preprocess
      lexer/tokenize
      parser/parse
      ast/optimize
      codegen/generate))
```

## Соответствие стандартам

- **C89/C90**: Базовая совместимость
- **C51 Extensions**: Поддержка специфичных для 8051 конструкций
- **AT89S4051**: Оптимизация под ограничения микроконтроллера 