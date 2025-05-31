# План рефакторинга: От Clojure к Haskell

## 🎯 Цель проекта

Создать полноценный учебно-тренировочный проект компилятора C для микроконтроллера AT89S4051, пройдя все стадии развития компиляторостроения от языка C до кодогенерации.

## 📊 Анализ текущего состояния

### Clojure проект (исходная база)

**Препроцессор v2** - зрелая архитектура:
- ✅ **Transducers** для эффективной обработки потоков данных
- ✅ **Protocols** для полиморфизма директив препроцессора  
- ✅ **Функциональная архитектура** без callback hell
- ✅ **Композиция функций** вместо императивного кода
- ✅ **Неизменяемые структуры данных**

**Ключевые принципы Clojure v2:**
```clojure
;; Протоколы для директив
(defprotocol PreprocessorDirective
  (matches? [this line])
  (process [this line state]))

;; Transducers для обработки
(defn preprocessor-xf [initial-state]
  (fn [rf]
    (let [state (volatile! initial-state)]
      ;; Композиция обработки...)))

;; Функциональная композиция
(def result (preprocess-v2 code options))
```

### Haskell проект (целевая архитектура)

**Преимущества миграции на Haskell:**

1. **Типобезопасность на уровне компиляции**
   - Алгебраические типы данных (ADT)
   - Pattern matching с проверкой исчерпывающности
   - Система типов предотвращает runtime ошибки

2. **Функциональная чистота**
   - Монады для управления состоянием и ошибками
   - Ленивые вычисления для эффективности
   - Композиция функций как основной принцип

3. **Современные библиотеки**
   - **Megaparsec** для парсинга
   - **Prettyprinter** для форматирования
   - **MTL** для монадных трансформеров

## 🏗️ Архитектура портирования

### Фаза 1: Препроцессор (✅ Завершена)

**Clojure → Haskell mapping:**

| Clojure концепция | Haskell эквивалент | Преимущества |
|-------------------|-------------------|--------------|
| `defprotocol` | `class PreprocessorDirective` | Типобезопасность |
| `volatile!` состояние | `StateT` монада | Чистота функций |
| `transducers` | Функции высшего порядка | Композиция |
| `spec` валидация | Система типов | Compile-time проверки |

**Реализованная архитектура:**
```haskell
-- Типобезопасные директивы
class PreprocessorDirective a where
  matches :: a -> Text -> Bool
  process :: a -> Text -> StateT PreprocessorState (ExceptT CompilerError IO) (Maybe Text)

-- Монадный стек для обработки ошибок
type PreprocessorM = StateT PreprocessorState (ExceptT CompilerError IO)

-- Алгебраические типы для макросов
data MacroDefinition
  = SimpleMacro !Text
  | FunctionMacro ![Text] !Text
```

### Фаза 2: Лексический анализ

**Цель:** Портировать лексер с использованием Megaparsec

**Архитектурные решения:**
```haskell
-- Токены как алгебраические типы
data Token
  = TKeyword !Keyword !SourcePos
  | TIdentifier !Text !SourcePos  
  | TLiteral !Literal !SourcePos
  | TOperator !Operator !SourcePos
  | TDelimiter !Delimiter !SourcePos
  deriving (Eq, Show)

-- Лексер как парсер
type Lexer = Parsec Void Text

-- Композиция лексем
lexeme :: Lexer a -> Lexer a
token :: Lexer Token
identifier :: Lexer Text
```

**Преимущества над Clojure:**
- Автоматическое отслеживание позиций
- Композиция парсеров
- Обработка ошибок на уровне типов

### Фаза 3: Синтаксический анализ

**Цель:** AST с полной типобезопасностью

```haskell
-- Типизированное AST
data Expr
  = ELiteral !Literal !SourcePos
  | EVariable !Identifier !SourcePos
  | EBinary !BinaryOp !Expr !Expr !SourcePos
  | EUnary !UnaryOp !Expr !SourcePos
  | ECall !Identifier ![Expr] !SourcePos
  deriving (Eq, Show)

data Stmt  
  = SExpr !Expr !SourcePos
  | SDecl !Declaration !SourcePos
  | SIf !Expr !Stmt !(Maybe Stmt) !SourcePos
  | SWhile !Expr !Stmt !SourcePos
  | SReturn !(Maybe Expr) !SourcePos
  deriving (Eq, Show)

-- Парсер с восстановлением ошибок
type Parser = Parsec Void [Token]

parseExpr :: Parser Expr
parseStmt :: Parser Stmt
parseProgram :: Parser Program
```

### Фаза 4: Семантический анализ и IR

**High-level IR (HIR):**
```haskell
-- Типизированное промежуточное представление
data HIR
  = HIRProgram ![HIRFunction] ![HIRGlobal]
  deriving (Eq, Show)

data HIRFunction = HIRFunction
  { hirFuncName :: !Identifier
  , hirFuncType :: !FunctionType
  , hirFuncParams :: ![Parameter]
  , hirFuncBody :: ![HIRStmt]
  , hirFuncPos :: !SourcePos
  } deriving (Eq, Show)

-- Проверка типов как чистая функция
typeCheck :: AST -> Either [TypeError] HIR
```

**Mid-level IR (MIR):**
```haskell
-- Упрощенное представление для оптимизаций
data MIR
  = MIRProgram ![MIRFunction]
  deriving (Eq, Show)

data MIRInstr
  = MIRLoad !Register !Address
  | MIRStore !Address !Register  
  | MIRAdd !Register !Register !Register
  | MIRJump !Label
  | MIRCall !FunctionName
  deriving (Eq, Show)

-- Трансформация HIR → MIR
lowerToMIR :: HIR -> MIR
```

### Фаза 5: Оптимизации

**Функциональные трансформации:**
```haskell
-- Оптимизации как чистые функции
type Optimization = MIR -> MIR

-- Композиция оптимизаций
optimize :: [Optimization] -> MIR -> MIR
optimize opts mir = foldl' (flip ($)) mir opts

-- Конкретные оптимизации
constantFolding :: Optimization
deadCodeElimination :: Optimization
registerAllocation :: Optimization
```

### Фаза 6: Кодогенерация для AT89S4051

**Целевая архитектура:**
```haskell
-- Инструкции AT89S4051
data AT89Instruction
  = MOV !Operand !Operand
  | ADD !Operand !Operand
  | JMP !Label
  | CALL !Address
  | RET
  deriving (Eq, Show)

-- Генерация кода
codeGen :: MIR -> [AT89Instruction]

-- Форматирование в ассемблер
formatAsm :: [AT89Instruction] -> Text
```

## 🔄 Сравнение архитектур

### Обработка ошибок

**Clojure v2:**
```clojure
;; Накопление ошибок в состоянии
(defn add-error [state error-message]
  (update state :errors conj error-info))

;; Возврат результата с ошибками
{:result processed-code
 :errors collected-errors
 :success (empty? errors)}
```

**Haskell:**
```haskell
-- Монадный стек для ошибок
type CompilerM = ExceptT CompilerError (WriterT [Diagnostic] IO)

-- Композиция обработки ошибок
runCompilerM :: CompilerM a -> IO (Either CompilerError a, [Diagnostic])

-- Типобезопасная обработка
throwCompilerError :: CompilerError -> CompilerM a
addDiagnostic :: Diagnostic -> CompilerM ()
```

### Состояние компилятора

**Clojure v2:**
```clojure
;; Неизменяемые карты
(def preprocessor-state
  {:defines {}
   :include-stack []
   :conditional-stack []
   :line-number 1})
```

**Haskell:**
```haskell
-- Строго типизированные записи
data PreprocessorState = PreprocessorState
  { psDefines :: !(Map Text MacroDefinition)
  , psIncludeStack :: ![FilePath]
  , psConditionalStack :: ![ConditionalState]
  , psLineNumber :: !Int
  } deriving (Eq, Show)
```

### Директивы препроцессора

**Clojure v2:**
```clojure
;; Протоколы для полиморфизма
(defprotocol PreprocessorDirective
  (matches? [this line])
  (process [this line state]))

;; Реестр директив
(def directive-registry
  [include-directive define-directive ifdef-directive])
```

**Haskell:**
```haskell
-- Классы типов для полиморфизма
class PreprocessorDirective a where
  matches :: a -> Text -> Bool
  process :: a -> Text -> PreprocessorM (Maybe Text)

-- Экзистенциальные типы для гетерогенных списков
data SomeDirective where
  SomeDirective :: PreprocessorDirective a => a -> SomeDirective
```

## 📈 Преимущества Haskell архитектуры

### 1. Типобезопасность

**Проблемы Clojure:**
- Runtime ошибки из-за неправильных типов
- Отсутствие проверки исчерпывающности case

**Решения Haskell:**
- Compile-time проверка типов
- Pattern matching с предупреждениями о неполноте
- Невозможность null pointer exceptions

### 2. Производительность

**Clojure ограничения:**
- JVM overhead
- Динамическая диспетчеризация
- Garbage collection паузы

**Haskell преимущества:**
- Компиляция в нативный код
- Ленивые вычисления
- Оптимизирующий компилятор GHC

### 3. Композиция и переиспользование

**Clojure подход:**
```clojure
;; Transducers для композиции
(def xf (comp (map process-line)
              (filter should-include?)
              (map expand-macros)))
```

**Haskell подход:**
```haskell
-- Функциональная композиция
processFile :: FilePath -> CompilerM Text
processFile = readFile >=> preprocess >=> parse >=> typeCheck >=> optimize >=> codeGen
```

### 4. Параллелизм

**Haskell встроенная поддержка:**
```haskell
-- Параллельная обработка файлов
processFiles :: [FilePath] -> CompilerM [CompiledModule]
processFiles files = mapConcurrently processFile files

-- STM для безопасного состояния
updateGlobalState :: GlobalState -> STM ()
```

## 🎓 Учебная ценность проекта

### Изучаемые концепции компиляторостроения:

1. **Лексический анализ**
   - Регулярные выражения и конечные автоматы
   - Обработка позиций в исходном коде
   - Восстановление после ошибок

2. **Синтаксический анализ**
   - Контекстно-свободные грамматики
   - Recursive descent парсеры
   - Обработка приоритетов операторов

3. **Семантический анализ**
   - Проверка типов
   - Анализ областей видимости
   - Построение таблиц символов

4. **Промежуточные представления**
   - Трансформации AST → HIR → MIR
   - SSA форма
   - Control flow анализ

5. **Оптимизации**
   - Constant folding
   - Dead code elimination
   - Register allocation

6. **Кодогенерация**
   - Instruction selection
   - Register allocation
   - Peephole optimizations

### Изучаемые концепции Haskell:

1. **Система типов**
   - Алгебраические типы данных
   - Классы типов
   - Экзистенциальные типы

2. **Монады и трансформеры**
   - State monad для состояния
   - Except monad для ошибок
   - IO monad для побочных эффектов

3. **Парсинг**
   - Parser combinators
   - Megaparsec библиотека
   - Обработка ошибок парсинга

4. **Функциональное программирование**
   - Композиция функций
   - Ленивые вычисления
   - Неизменяемые структуры данных

## 🚀 План реализации

### Этап 1: Инфраструктура (✅ Завершен)
- [x] Базовые типы данных
- [x] Система обработки ошибок  
- [x] Препроцессор

### Этап 2: Лексический анализ (🔄 В процессе)
- [ ] Определение токенов
- [ ] Megaparsec лексер
- [ ] Обработка позиций
- [ ] Тесты лексера

### Этап 3: Синтаксический анализ
- [ ] AST типы
- [ ] Parser combinators
- [ ] Обработка ошибок
- [ ] Тесты парсера

### Этап 4: Семантический анализ
- [ ] HIR определения
- [ ] Проверка типов
- [ ] Анализ областей видимости
- [ ] Тесты семантики

### Этап 5: Оптимизации и кодогенерация
- [ ] MIR определения
- [ ] Оптимизации
- [ ] AT89S4051 кодогенерация
- [ ] Интеграционные тесты

### Этап 6: Интеграция и документация
- [ ] CLI интерфейс
- [ ] Примеры программ
- [ ] Документация API
- [ ] Учебные материалы

## 🎯 Ожидаемые результаты

1. **Полнофункциональный компилятор C → AT89S4051**
2. **Современная архитектура на Haskell**
3. **Учебные материалы по компиляторостроению**
4. **Демонстрация преимуществ функционального программирования**
5. **Готовая база для дальнейшего развития**

## 📚 Литература и ресурсы

### Компиляторостроение:
- "Compilers: Principles, Techniques, and Tools" (Dragon Book)
- "Modern Compiler Implementation in ML" 
- "Engineering a Compiler"

### Haskell:
- "Real World Haskell"
- "Haskell Programming from First Principles"
- "Parallel and Concurrent Programming in Haskell"

### AT89S4051:
- Datasheet AT89S4051
- "8051 Microcontroller Architecture"
- Assembly language reference

---

*Этот план представляет собой roadmap для создания современного, типобезопасного компилятора C для микроконтроллеров, демонстрирующего преимущества функционального программирования в области компиляторостроения.* 