# План миграции C51 Clojure Compiler → HCL (Haskell C51 Compiler)

## Обзор миграции

Этот документ описывает детальный план миграции существующего C51 Clojure Compiler на Haskell. Миграция направлена на использование преимуществ функционального программирования и системы типов Haskell для создания более надежного и производительного компилятора.

## Анализ существующей архитектуры

### Компоненты Clojure версии

1. **Препроцессор** (`preprocessor.clj`, `preprocessor_v2.clj`)
   - Обработка директив `#include`, `#define`
   - Удаление комментариев
   - Обработка условной компиляции

2. **Лексер** (`lexer.clj`)
   - Токенизация исходного кода
   - Обработка строковых и числовых литералов
   - Распознавание ключевых слов и операторов

3. **Парсер** (`parser.clj`)
   - Построение AST из токенов
   - Обработка приоритетов операторов
   - Поддержка специфичных для C51 конструкций

4. **AST** (`ast.clj`)
   - Определения узлов дерева
   - Функции создания и валидации
   - Обход дерева (visitor pattern)

5. **IR** (`ir/hir.clj`)
   - High-level промежуточное представление
   - Типизированные операции
   - Платформо-независимые конструкции

6. **Утилиты** (`logger.clj`, `config.clj`, `core.clj`)
   - Логирование
   - Конфигурация
   - Точка входа

## Стратегия миграции

### Принципы миграции

1. **Поэтапность**: Миграция по компонентам с возможностью тестирования каждого этапа
2. **Типобезопасность**: Максимальное использование системы типов Haskell
3. **Функциональность**: Сохранение всех возможностей оригинального компилятора
4. **Производительность**: Улучшение производительности за счет оптимизаций GHC
5. **Тестируемость**: Создание comprehensive test suite

### Подход к переводу

| Clojure концепция | Haskell эквивалент | Обоснование |
|-------------------|-------------------|-------------|
| Динамические карты | Алгебраические типы данных | Типобезопасность, pattern matching |
| Мультиметоды | Функции высшего порядка + типы | Композиция, переиспользование |
| Атомы/Refs | STM/IORef | Контролируемые побочные эффекты |
| Исключения | Either/ExceptT | Явная обработка ошибок |
| Макросы | Template Haskell (при необходимости) | Метапрограммирование |

## Детальный план по фазам

### Фаза 1: Инфраструктура и базовые типы ✅

**Цель**: Создать фундамент для всех остальных компонентов

**Задачи**:
- [x] Настройка проекта Haskell (Stack/Cabal)
- [x] Определение базовых типов данных (`HCL.Types`)
- [x] Система обработки ошибок (`HCL.Error`)
- [ ] Система логирования (`HCL.Logger`)
- [ ] Система конфигурации (`HCL.Config`)
- [ ] Утилиты (`HCL.Utils`)

**Детали реализации**:

#### HCL.Types ✅
```haskell
-- Типы данных AT89S4051
data C51Type = UInt8Type | Int8Type | BitType | ...

-- Позиции в исходном коде
data SourcePos = SourcePos Text Int Int

-- Операторы
data BinaryOp = Add | Sub | Mul | ...
```

#### HCL.Error ✅
```haskell
-- Иерархия ошибок
data CompilerError = LexError | ParseError | SemanticError | ...

-- Монада компилятора
type CompilerM = ExceptT CompilerError (WriterT [Diagnostic] IO)
```

#### HCL.Logger (TODO)
```haskell
-- Уровни логирования
data LogLevel = Debug | Info | Warning | Error

-- Монада логирования
class MonadLogger m where
  logDebug :: Text -> m ()
  logInfo :: Text -> m ()
  logWarning :: Text -> m ()
  logError :: Text -> m ()
```

#### HCL.Config (TODO)
```haskell
-- Конфигурация компилятора
data CompilerConfig = CompilerConfig
  { cfgOptimizationLevel :: OptimizationLevel
  , cfgTargetDevice :: TargetDevice
  , cfgIncludePaths :: [FilePath]
  , cfgDefines :: Map Text Text
  }
```

### Фаза 2: Лексический анализ

**Цель**: Перевести лексер с ручной реализации на Megaparsec

**Задачи**:
- [ ] Определение типов токенов
- [ ] Лексер с использованием Megaparsec (`HCL.Lexer`)
- [ ] Обработка позиций в исходном коде
- [ ] Препроцессор (`HCL.Preprocessor`)

**Сравнение подходов**:

| Аспект | Clojure | Haskell |
|--------|---------|---------|
| Парсинг | Ручной парсер | Megaparsec combinators |
| Ошибки | Исключения | ParseError в Either |
| Позиции | Ручное отслеживание | Автоматическое в Megaparsec |
| Тестирование | Ручные тесты | QuickCheck properties |

**Детали реализации**:

#### Типы токенов
```haskell
data Token
  = TokIdentifier Identifier
  | TokKeyword Keyword
  | TokOperator Operator
  | TokLiteral Literal
  | TokPunctuation Punctuation
  | TokComment Text
  | TokWhitespace
  | TokEOF
  deriving (Eq, Show)

data Keyword
  = KwVoid | KwChar | KwInt | KwIf | KwElse | KwWhile | KwFor
  | KwReturn | KwBreak | KwContinue
  -- C51 специфичные
  | KwInterrupt | KwUsing | KwSfr | KwSbit
  deriving (Eq, Show, Enum, Bounded)
```

#### Лексер на Megaparsec
```haskell
type Lexer = Parsec Void Text

-- Основная функция лексера
tokenize :: Text -> Either LexerError [Located Token]

-- Парсеры для различных токенов
identifier :: Lexer (Located Token)
keyword :: Lexer (Located Token)
operator :: Lexer (Located Token)
literal :: Lexer (Located Token)
```

#### Препроцессор
```haskell
-- Директивы препроцессора
data Directive
  = Include FilePath
  | Define Identifier (Maybe [Identifier]) Text
  | Ifdef Identifier
  | Ifndef Identifier
  | Endif
  deriving (Eq, Show)

-- Состояние препроцессора
data PreprocessorState = PreprocessorState
  { psDefines :: Map Identifier MacroDefinition
  , psIncludePaths :: [FilePath]
  , psConditionalStack :: [Bool]
  }

-- Основная функция препроцессора
preprocess :: [FilePath] -> Text -> CompilerM Text
```

### Фаза 3: Синтаксический анализ

**Цель**: Создать типобезопасный парсер с использованием parser combinators

**Задачи**:
- [ ] Определение AST типов (`HCL.AST`)
- [ ] Парсер с использованием parser combinators (`HCL.Parser`)
- [ ] Обработка ошибок парсинга

**Преимущества Haskell подхода**:
- **Композиция**: Parser combinators легко комбинируются
- **Типобезопасность**: Невозможно создать некорректное AST
- **Переиспользование**: Общие паттерны выносятся в отдельные функции
- **Тестирование**: Каждый combinator можно тестировать отдельно

**Детали реализации**:

#### AST типы
```haskell
-- Корневой узел программы
data Program = Program [Declaration] SourceSpan
  deriving (Eq, Show)

-- Объявления
data Declaration
  = FunctionDecl FunctionDeclaration
  | VariableDecl VariableDeclaration
  | TypedefDecl TypedefDeclaration
  -- C51 специфичные
  | InterruptDecl InterruptDeclaration
  | SfrDecl SfrDeclaration
  | SbitDecl SbitDeclaration
  deriving (Eq, Show)

-- Выражения
data Expression
  = BinaryExpr BinaryOp Expression Expression SourceSpan
  | UnaryExpr UnaryOp Expression SourceSpan
  | CallExpr Expression [Expression] SourceSpan
  | IdentifierExpr Identifier SourceSpan
  | LiteralExpr Literal SourceSpan
  | ArrayAccessExpr Expression Expression SourceSpan
  deriving (Eq, Show)

-- Операторы
data Statement
  = ExpressionStmt Expression SourceSpan
  | BlockStmt [Statement] SourceSpan
  | IfStmt Expression Statement (Maybe Statement) SourceSpan
  | WhileStmt Expression Statement SourceSpan
  | ForStmt (Maybe Expression) (Maybe Expression) (Maybe Expression) Statement SourceSpan
  | ReturnStmt (Maybe Expression) SourceSpan
  | BreakStmt SourceSpan
  | ContinueStmt SourceSpan
  deriving (Eq, Show)
```

#### Parser combinators
```haskell
type Parser = Parsec Void [Located Token]

-- Основная функция парсера
parseProgram :: [Located Token] -> Either ParserError Program

-- Парсеры для различных конструкций
parseDeclaration :: Parser Declaration
parseExpression :: Parser Expression
parseStatement :: Parser Statement

-- Утилиты для парсинга
token :: (Token -> Maybe a) -> Parser a
keyword :: Keyword -> Parser ()
operator :: Operator -> Parser ()
```

### Фаза 4: Семантический анализ и IR

**Цель**: Создать типизированное промежуточное представление

**Задачи**:
- [ ] Трансформация AST в HIR (`HCL.IR.HIR`)
- [ ] Проверка типов
- [ ] Анализ областей видимости
- [ ] Mid-level IR (`HCL.IR.MIR`)

**Ключевые улучшения**:
- **Типизированный IR**: Каждый узел содержит информацию о типе
- **Монадический анализ**: Использование State/Reader монад для контекста
- **Композиция анализаторов**: Модульная архитектура анализаторов

**Детали реализации**:

#### HIR (High-level IR)
```haskell
-- Узел HIR с типовой информацией
data HIRNode = HIRNode
  { hirOp :: HIROp
  , hirType :: C51Type
  , hirOperands :: [HIRNode]
  , hirSourcePos :: Maybe SourcePos
  } deriving (Eq, Show)

-- Операции HIR
data HIROp
  = Add | Sub | Mul | Div | Mod
  | Eq | Ne | Lt | Le | Gt | Ge
  | And | Or | Not
  | Load | Store
  | Call | Return
  | If | While | For
  | Constant | Variable
  deriving (Eq, Show)

-- Трансформация AST -> HIR
astToHir :: Program -> CompilerM HIRProgram
```

#### Проверка типов
```haskell
-- Контекст проверки типов
data TypeContext = TypeContext
  { tcVariables :: Map Identifier C51Type
  , tcFunctions :: Map Identifier FunctionType
  , tcCurrentFunction :: Maybe Identifier
  }

-- Монада проверки типов
type TypeChecker = ReaderT TypeContext CompilerM

-- Основные функции проверки типов
checkExpression :: Expression -> TypeChecker (Expression, C51Type)
checkStatement :: Statement -> TypeChecker Statement
checkDeclaration :: Declaration -> TypeChecker Declaration
```

#### Анализ областей видимости
```haskell
-- Область видимости
data Scope = Scope
  { scopeVariables :: Map Identifier VariableInfo
  , scopeParent :: Maybe Scope
  }

-- Информация о переменной
data VariableInfo = VariableInfo
  { varType :: C51Type
  , varSourcePos :: SourcePos
  , varIsUsed :: Bool
  }

-- Монада анализа областей видимости
type ScopeAnalyzer = StateT [Scope] CompilerM

-- Основные функции анализа
enterScope :: ScopeAnalyzer ()
exitScope :: ScopeAnalyzer ()
declareVariable :: Identifier -> C51Type -> SourcePos -> ScopeAnalyzer ()
lookupVariable :: Identifier -> ScopeAnalyzer (Maybe VariableInfo)
```

### Фаза 5: Оптимизации и кодогенерация

**Цель**: Реализовать оптимизации и генерацию кода для AT89S4051

**Задачи**:
- [ ] Оптимизации на уровне HIR (`HCL.Optimizer`)
- [ ] Генерация кода для AT89S4051 (`HCL.CodeGen`)
- [ ] Интеграция с ассемблером

**Оптимизации**:
1. **Constant folding**: Вычисление константных выражений
2. **Dead code elimination**: Удаление недостижимого кода
3. **Register allocation**: Оптимальное распределение регистров
4. **Peephole optimization**: Локальные оптимизации

**Детали реализации**:

#### Оптимизатор
```haskell
-- Проход оптимизации
type OptimizationPass = HIRProgram -> CompilerM HIRProgram

-- Основные оптимизации
constantFolding :: OptimizationPass
deadCodeElimination :: OptimizationPass
registerAllocation :: OptimizationPass

-- Композиция оптимизаций
optimize :: [OptimizationPass] -> HIRProgram -> CompilerM HIRProgram
optimize passes program = foldM (flip ($)) program passes
```

#### Генератор кода
```haskell
-- Ассемблерная инструкция
data Instruction
  = MOV Operand Operand
  | ADD Operand Operand
  | SUB Operand Operand
  | JMP Label
  | JZ Label
  | CALL Label
  | RET
  | NOP
  deriving (Eq, Show)

-- Операнд
data Operand
  = Register Register
  | Immediate Word8
  | Memory Address
  | Label Text
  deriving (Eq, Show)

-- Генерация кода
generateCode :: HIRProgram -> CompilerM [Instruction]
```

### Фаза 6: Тестирование и документация

**Цель**: Обеспечить качество и документированность кода

**Задачи**:
- [ ] Модульные тесты для всех компонентов
- [ ] Интеграционные тесты
- [ ] Бенчмарки производительности
- [ ] Документация API

**Стратегия тестирования**:

#### Модульные тесты
```haskell
-- Тесты лексера
spec_lexer :: Spec
spec_lexer = describe "Lexer" $ do
  it "tokenizes identifiers" $ do
    tokenize "variable" `shouldBe` Right [TokIdentifier "variable"]
  
  it "handles keywords" $ do
    tokenize "if" `shouldBe` Right [TokKeyword KwIf]

-- Property-based тесты
prop_lexer_roundtrip :: Text -> Property
prop_lexer_roundtrip input = 
  case tokenize input of
    Right tokens -> untokenize tokens === input
    Left _ -> property True
```

#### Интеграционные тесты
```haskell
-- Тест полного пайплайна
test_full_compilation :: FilePath -> IO ()
test_full_compilation inputFile = do
  result <- compileFile inputFile
  case result of
    Right output -> do
      -- Проверяем, что вывод корректен
      validateAssembly output
    Left err -> 
      expectationFailure $ "Compilation failed: " ++ show err
```

#### Бенчмарки
```haskell
-- Бенчмарки производительности
benchmarks :: [Benchmark]
benchmarks =
  [ bench "lexer/small" $ nf tokenize smallInput
  , bench "lexer/large" $ nf tokenize largeInput
  , bench "parser/small" $ nf parseProgram smallTokens
  , bench "parser/large" $ nf parseProgram largeTokens
  ]
```

## Критерии успеха миграции

### Функциональные критерии
- [ ] Компиляция всех тестовых программ из оригинального проекта
- [ ] Генерация корректного ассемблерного кода для AT89S4051
- [ ] Поддержка всех специфичных для C51 конструкций
- [ ] Совместимость с существующими заголовочными файлами

### Нефункциональные критерии
- [ ] Производительность не хуже оригинального компилятора
- [ ] Покрытие тестами не менее 90%
- [ ] Отсутствие memory leaks
- [ ] Понятные сообщения об ошибках

### Качественные критерии
- [ ] Модульная архитектура
- [ ] Типобезопасность
- [ ] Документированность API
- [ ] Простота расширения

## Риски и митигация

### Технические риски
1. **Сложность миграции парсера**
   - *Митигация*: Поэтапная миграция с тестированием каждого компонента

2. **Производительность Haskell**
   - *Митигация*: Профилирование и оптимизация критических участков

3. **Совместимость с существующим кодом**
   - *Митигация*: Comprehensive test suite с примерами из оригинального проекта

### Проектные риски
1. **Недооценка сложности**
   - *Митигация*: Детальное планирование и буферное время

2. **Недостаток экспертизы в Haskell**
   - *Митигация*: Изучение best practices и консультации с экспертами

## Заключение

Миграция C51 Clojure Compiler на Haskell представляет собой значительное улучшение архитектуры компилятора. Использование системы типов Haskell, функционального программирования и современных библиотек позволит создать более надежный, производительный и расширяемый компилятор.

Поэтапный подход к миграции минимизирует риски и позволяет тестировать каждый компонент отдельно. Результатом станет современный компилятор, демонстрирующий преимущества функционального программирования в области разработки компиляторов. 