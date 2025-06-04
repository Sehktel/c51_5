# Автоматизированное тестирование Peephole оптимизаций

## Обзор

Система автоматизированного тестирования peephole оптимизаций для HCL компилятора обеспечивает комплексную проверку корректности и эффективности оптимизаций "замочной скважины" на реальных примерах C кода.

## Архитектура системы

### Компоненты

1. **PeepholeOptimizer** - основной модуль оптимизации
2. **PeepholeOptimizerSpec** - автоматизированные тесты
3. **test_peephole_optimization.sh** - скрипт массового тестирования
4. **Примеры C кода** - коллекция тестовых программ в `test/c_code/`

### Типы оптимизаций

#### Встроенные правила AT89S4051

1. **Удаление избыточных пересылок**
   ```asm
   MOV A, R0    ; Исходный код
   MOV R0, A    ; 
   ; ↓ Оптимизация
   ; (удаляется)
   ```

2. **Оптимизация цепочек аккумулятора**
   ```asm
   MOV A, R1    ; Исходный код
   ADD A, #5    ;
   MOV R2, A    ;
   ; ↓ Оптимизация
   ADD R1, #5   ; Прямая операция
   MOV R2, R1   ;
   ```

3. **Свёртка констант**
   ```asm
   MOV A, #5    ; Исходный код
   ADD A, #3    ;
   ; ↓ Оптимизация
   MOV A, #8    ; Предвычисленное значение
   ```

4. **Удаление мёртвого кода**
   ```asm
   MOV R0, #10  ; Исходный код
   NOP          ; Мёртвая инструкция
   MOV R0, #20  ; Перезаписывает R0
   ; ↓ Оптимизация
   MOV R0, #20  ; Только финальное значение
   ```

5. **Оптимизация ветвлений**
   ```asm
   JZ label     ; Исходный код
   SJMP next    ;
   ; ↓ Оптимизация
   JNZ next     ; Инвертированное условие
   ```

## Автоматическое обнаружение паттернов

### Алгоритмы сопоставления

- **Полный перебор** - для коротких паттернов
- **KMP** - для эффективного поиска повторяющихся последовательностей
- **Хэш-основанное** - для быстрого сравнения сигнатур
- **Суффиксное дерево** - для сложных паттернов

### Метрики обнаружения

```haskell
data PatternMetrics = PatternMetrics
  { frequency :: Int           -- Частота встречаемости
  , savings :: OptimizationSavings  -- Потенциальная экономия
  , confidence :: Double       -- Уверенность в корректности
  , complexity :: Int          -- Сложность применения
  }
```

## Валидация оптимизаций

### Проверки безопасности

1. **Семантическая эквивалентность**
   - Сравнение результатов выполнения
   - Проверка побочных эффектов
   - Анализ потока данных

2. **Корректность регистров**
   - Отслеживание использования регистров
   - Проверка конфликтов
   - Валидация времени жизни

3. **Сохранение флагов**
   - Анализ влияния на флаги процессора
   - Проверка зависимостей от флагов
   - Валидация условных переходов

### Архитектурные ограничения AT89S4051

```haskell
data AT89S4051Constraints = AT89S4051Constraints
  { romSize :: Word16        -- 4KB ROM
  , ramSize :: Word8         -- 128 байт RAM
  , stackSize :: Word8       -- Размер стека
  , interruptVectors :: [InterruptVector]
  , specialRegisters :: Map Text Word8
  }
```

## Метрики производительности

### Измеряемые параметры

1. **Размер кода**
   - Исходный размер в байтах
   - Оптимизированный размер
   - Процент экономии

2. **Количество циклов**
   - Исходное количество циклов
   - Оптимизированное количество
   - Ускорение выполнения

3. **Использование ресурсов**
   - Регистры
   - Память
   - Стек

### Пример метрик

```json
{
  "file": "01_basic_led.c",
  "optimization_level": 2,
  "original_size": 156,
  "optimized_size": 128,
  "size_savings": 28,
  "original_cycles": 45,
  "optimized_cycles": 37,
  "cycle_savings": 8,
  "rules_applied": 4,
  "optimizations": [
    {
      "rule": "redundant_move_elimination",
      "applications": 2,
      "savings": {"cycles": 4, "bytes": 4}
    },
    {
      "rule": "constant_folding",
      "applications": 1,
      "savings": {"cycles": 2, "bytes": 2}
    }
  ]
}
```

## Тестирование на реальных примерах

### Коллекция тестовых программ

#### Базовые примеры
- `01_basic_led.c` - мигание светодиодом
- `02_bit_operations.c` - битовые операции
- `03_timer_basic.c` - работа с таймером
- `04_serial_comm.c` - последовательная связь
- `05_external_interrupt.c` - внешние прерывания

#### Конструкции языка C
- `test_01_var_decl.c` - объявления переменных
- `test_02_assignment.c` - присваивания
- `test_03_arithmetic.c` - арифметические операции
- `test_04_for_loop.c` - циклы for
- `test_05_while_loop.c` - циклы while
- `test_06_if_statement.c` - условные операторы

#### Специфика 8051
- `test_10_interrupt.c` - обработчики прерываний
- `test_15_bits.c` - битовые переменные
- `test_19_regbank.c` - банки регистров
- `test_22_memory.c` - работа с памятью

#### Комплексные примеры
- `test_all_constructs.c` - все конструкции языка
- `test_21_funcptr.c` - указатели на функции
- `test_25_singleton_guard.c` - защита от множественного включения

### Автоматизированное тестирование

```bash
# Запуск всех тестов
./scripts/test_peephole_optimization.sh

# Очистка результатов
./scripts/test_peephole_optimization.sh --clean

# Создание новых базовых результатов
./scripts/test_peephole_optimization.sh --baseline
```

### Структура результатов

```
test_results/peephole/
├── reports/
│   ├── optimization_summary.txt
│   ├── regression_report.txt
│   └── *_O*_report.txt
├── metrics/
│   ├── optimization_metrics.csv
│   └── *_O*_metrics.json
└── final_report.html
```

## Property-based тестирование

### Инварианты оптимизации

1. **Монотонность производительности**
   ```haskell
   property_performance_monotonic :: AssemblyCode -> Bool
   property_performance_monotonic code =
     let optimized = optimize code
     in cycles optimized <= cycles code
   ```

2. **Монотонность размера**
   ```haskell
   property_size_monotonic :: AssemblyCode -> Bool
   property_size_monotonic code =
     let optimized = optimize code
     in size optimized <= size code
   ```

3. **Идемпотентность**
   ```haskell
   property_idempotent :: AssemblyCode -> Bool
   property_idempotent code =
     let opt1 = optimize code
         opt2 = optimize opt1
     in opt1 == opt2
   ```

### Генерация тестовых данных

```haskell
instance Arbitrary AssemblyCode where
  arbitrary = do
    numInstructions <- choose (1, 20)
    instructions <- vectorOf numInstructions arbitrary
    return $ AssemblyCode instructions [] [] Map.empty

instance Arbitrary AssemblyInstruction where
  arbitrary = do
    mnemonic <- elements at89s4051Instructions
    operands <- listOf arbitrary
    return $ AssemblyInstruction mnemonic operands 0 1 1 Nothing Nothing
```

## Регрессионное тестирование

### Базовые результаты

Система сохраняет базовые результаты оптимизации для каждого примера и сравнивает новые результаты с базовыми для обнаружения регрессий.

### Критерии регрессии

1. **Ухудшение производительности** - увеличение количества циклов
2. **Увеличение размера кода** - рост размера в байтах
3. **Снижение количества применённых оптимизаций**
4. **Появление новых ошибок компиляции**

### Отчёт о регрессиях

```
=== Отчёт о регрессиях ===
Дата: 2024-01-15 14:30:00

Сравнение 01_basic_led_O2_metrics.json:
  ✓ Улучшение или без изменений

Сравнение test_04_for_loop_O2_metrics.json:
  ✗ Возможная регрессия
  Размер: 89 → 95 байт (+6)
  Циклы: 34 → 36 (+2)

Итого:
  Улучшений: 23
  Регрессий: 1

⚠️  ВНИМАНИЕ: Обнаружены возможные регрессии!
```

## Интеграция с CI/CD

### GitHub Actions

```yaml
name: Peephole Optimization Tests

on: [push, pull_request]

jobs:
  peephole-tests:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v2
      - name: Setup Haskell
        uses: haskell/actions/setup@v1
      - name: Build project
        run: stack build
      - name: Run peephole tests
        run: ./scripts/test_peephole_optimization.sh
      - name: Upload results
        uses: actions/upload-artifact@v2
        with:
          name: peephole-test-results
          path: test_results/peephole/
```

### Уведомления о регрессиях

При обнаружении регрессий система может:
- Отправлять уведомления в Slack/Discord
- Создавать GitHub Issues
- Блокировать merge в основную ветку

## Расширение системы

### Добавление новых правил

1. **Определение паттерна**
   ```haskell
   newOptimizationRule :: OptimizationRule
   newOptimizationRule = OptimizationRule
     { orId = "new_optimization"
     , orName = "Новая оптимизация"
     , orPattern = definePattern
     , orAction = defineAction
     , orConditions = [OptimizationLevelCondition 1]
     , orMetrics = RuleMetrics 0 0 0 0.0 0.0
     , orPriority = 5
     , orEnabled = True
     }
   ```

2. **Добавление тестов**
   ```haskell
   it "применяет новую оптимизацию" $ do
     let originalCode = createTestCode
     result <- runOptimizer config originalCode
     -- Проверки результата
   ```

3. **Обновление документации**

### Адаптивное обучение

Система может автоматически обнаруживать новые паттерны оптимизации на основе анализа частоты встречаемости определённых последовательностей инструкций.

```haskell
discoverNewPatterns :: [AssemblyCode] -> IO [OptimizationRule]
discoverNewPatterns codes = do
  patterns <- extractFrequentPatterns codes
  candidates <- generateOptimizationCandidates patterns
  validated <- validateCandidates candidates
  return $ map patternToRule validated
```

## Заключение

Автоматизированная система тестирования peephole оптимизаций обеспечивает:

1. **Высокое качество** - комплексная проверка корректности
2. **Производительность** - измерение реальных улучшений
3. **Надёжность** - регрессионное тестирование
4. **Расширяемость** - простое добавление новых правил
5. **Автоматизацию** - минимальное участие человека

Система является критически важной частью инфраструктуры компилятора, обеспечивающей стабильность и качество генерируемого кода для микроконтроллеров AT89S4051. 