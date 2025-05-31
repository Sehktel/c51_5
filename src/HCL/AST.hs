{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Модуль абстрактного синтаксического дерева для HCL компилятора
--
-- Этот модуль определяет типы данных для представления программ на C
-- с расширениями для микроконтроллера AT89S4051. AST спроектировано
-- с учетом типобезопасности и удобства трансформаций.
--
-- Основные принципы:
-- * Каждый узел содержит информацию о позиции в исходном коде
-- * Типы данных отражают семантику языка C
-- * Расширения для 8051 интегрированы естественным образом
-- * Поддержка для будущих оптимизаций и трансформаций
module HCL.AST
  ( -- * Программа и декларации верхнего уровня
    Program(..)
  , TopLevelDecl(..)
  , FunctionDef(..)
  , Declaration(..)
  
    -- * Типы данных
  , TypeSpec(..)
  , StorageClass(..)
  , TypeQualifier(..)
  , MemoryModel(..)  -- Расширение для 8051
  
    -- * Выражения
  , Expr(..)
  , ExprF(..)
  , BinaryOp(..)
  , UnaryOp(..)
  , AssignOp(..)
  
    -- * Операторы
  , Stmt(..)
  , StmtF(..)
  , CompoundStmt(..)
  
    -- * Параметры и аргументы
  , Parameter(..)
  , Argument(..)
  
    -- * Инициализаторы
  , Initializer(..)
  
    -- * Расширения для 8051
  , InterruptSpec(..)
  , BitDeclaration(..)
  , SfrDeclaration(..)
  
    -- * Утилиты
  , exprPos
  , stmtPos
  , declPos
  , prettyAST
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import HCL.Types (SourcePos(..), Identifier(..), Literal(..), C51Type(..), 
                  BinaryOp(..), UnaryOp(..), AssignOp(..))

-- ============================================================================
-- ПРОГРАММА И ДЕКЛАРАЦИИ ВЕРХНЕГО УРОВНЯ
-- ============================================================================

-- | Полная программа на C
data Program = Program
  { progDecls :: ![TopLevelDecl]  -- ^ Декларации верхнего уровня
  , progPos :: !SourcePos         -- ^ Позиция начала программы
  } deriving (Eq, Show)

-- | Декларация верхнего уровня
data TopLevelDecl
  = TLFunction !FunctionDef       -- ^ Определение функции
  | TLDeclaration !Declaration    -- ^ Декларация переменной/типа
  | TLBitDecl !BitDeclaration     -- ^ Декларация битовой переменной (8051)
  | TLSfrDecl !SfrDeclaration     -- ^ Декларация SFR (8051)
  deriving (Eq, Show)

-- | Определение функции
data FunctionDef = FunctionDef
  { funcName :: !Identifier           -- ^ Имя функции
  , funcReturnType :: !TypeSpec       -- ^ Тип возвращаемого значения
  , funcParams :: ![Parameter]        -- ^ Параметры функции
  , funcBody :: !CompoundStmt         -- ^ Тело функции
  , funcStorageClass :: !(Maybe StorageClass)  -- ^ Класс хранения
  , funcInterrupt :: !(Maybe InterruptSpec)    -- ^ Спецификация прерывания (8051)
  , funcMemoryModel :: !(Maybe MemoryModel)    -- ^ Модель памяти (8051)
  , funcPos :: !SourcePos             -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Декларация переменной или типа
data Declaration = Declaration
  { declType :: !TypeSpec             -- ^ Тип
  , declName :: !Identifier           -- ^ Имя
  , declInit :: !(Maybe Initializer)  -- ^ Инициализатор
  , declStorageClass :: !(Maybe StorageClass)  -- ^ Класс хранения
  , declQualifiers :: ![TypeQualifier]         -- ^ Квалификаторы типа
  , declMemoryModel :: !(Maybe MemoryModel)    -- ^ Модель памяти (8051)
  , declPos :: !SourcePos             -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- ============================================================================
-- ТИПЫ ДАННЫХ
-- ============================================================================

-- | Спецификация типа
data TypeSpec
  = TSVoid                            -- ^ void
  | TSChar                            -- ^ char
  | TSInt                             -- ^ int
  | TSFloat                           -- ^ float
  | TSDouble                          -- ^ double
  | TSShort                           -- ^ short
  | TSLong                            -- ^ long
  | TSSigned                          -- ^ signed
  | TSUnsigned                        -- ^ unsigned
  | TSBit                             -- ^ bit (8051)
  | TSPointer !TypeSpec               -- ^ указатель на тип
  | TSArray !TypeSpec !(Maybe Expr)   -- ^ массив типа с размером
  | TSStruct !Identifier ![Declaration] -- ^ структура
  | TSUnion !Identifier ![Declaration]  -- ^ объединение
  | TSEnum !Identifier ![Identifier]    -- ^ перечисление
  | TSTypedef !Identifier             -- ^ пользовательский тип
  deriving (Eq, Show)

-- | Класс хранения
data StorageClass
  = SCStatic      -- ^ static
  | SCExtern      -- ^ extern
  | SCRegister    -- ^ register
  | SCAuto        -- ^ auto
  deriving (Eq, Show, Enum, Bounded)

-- | Квалификатор типа
data TypeQualifier
  = TQConst       -- ^ const
  | TQVolatile    -- ^ volatile
  deriving (Eq, Show, Enum, Bounded)

-- | Модель памяти для 8051
data MemoryModel
  = MMData        -- ^ data - внутренняя память данных
  | MMIdata       -- ^ idata - косвенно адресуемая внутренняя память
  | MMXdata       -- ^ xdata - внешняя память данных
  | MMCode        -- ^ code - память программы
  deriving (Eq, Show, Enum, Bounded)

-- ============================================================================
-- ВЫРАЖЕНИЯ
-- ============================================================================

-- | Выражение с позицией
data Expr = Expr
  { exprNode :: !ExprF    -- ^ Узел выражения
  , exprPos :: !SourcePos -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Функтор для выражений (позволяет легко трансформировать AST)
data ExprF
  = ELiteral !Literal                 -- ^ Литерал
  | EVariable !Identifier             -- ^ Переменная
  | EBinary !BinaryOp !Expr !Expr     -- ^ Бинарная операция
  | EUnary !UnaryOp !Expr             -- ^ Унарная операция
  | EAssign !AssignOp !Expr !Expr     -- ^ Присваивание
  | ECall !Identifier ![Expr]         -- ^ Вызов функции
  | EArrayAccess !Expr !Expr          -- ^ Доступ к элементу массива
  | EFieldAccess !Expr !Identifier    -- ^ Доступ к полю структуры
  | EPtrFieldAccess !Expr !Identifier -- ^ Доступ к полю через указатель
  | ECast !TypeSpec !Expr             -- ^ Приведение типа
  | ESizeOf !TypeSpec                 -- ^ sizeof(type)
  | ETernary !Expr !Expr !Expr        -- ^ Тернарный оператор
  | EComma ![Expr]                    -- ^ Оператор запятая
  | EBitAccess !Expr !Expr            -- ^ Доступ к биту (8051: P1^0)
  deriving (Eq, Show)

-- ============================================================================
-- ОПЕРАТОРЫ
-- ============================================================================

-- | Оператор с позицией
data Stmt = Stmt
  { stmtNode :: !StmtF    -- ^ Узел оператора
  , stmtPos :: !SourcePos -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Функтор для операторов
data StmtF
  = SExpr !Expr                       -- ^ Выражение-оператор
  | SDecl !Declaration                -- ^ Декларация
  | SCompound !CompoundStmt           -- ^ Составной оператор
  | SIf !Expr !Stmt !(Maybe Stmt)     -- ^ if-else
  | SWhile !Expr !Stmt                -- ^ while
  | SFor !(Maybe Expr) !(Maybe Expr) !(Maybe Expr) !Stmt  -- ^ for
  | SDoWhile !Stmt !Expr              -- ^ do-while
  | SSwitch !Expr ![CaseStmt]         -- ^ switch
  | SBreak                            -- ^ break
  | SContinue                         -- ^ continue
  | SReturn !(Maybe Expr)             -- ^ return
  | SGoto !Identifier                 -- ^ goto
  | SLabel !Identifier !Stmt          -- ^ метка
  | SNop                              -- ^ пустой оператор
  deriving (Eq, Show)

-- | Составной оператор (блок)
data CompoundStmt = CompoundStmt
  { compoundStmts :: ![Stmt]    -- ^ Операторы в блоке
  , compoundPos :: !SourcePos   -- ^ Позиция блока
  } deriving (Eq, Show)

-- | Ветка case в switch
data CaseStmt
  = CaseStmt !Expr ![Stmt] !SourcePos      -- ^ case value: statements
  | DefaultStmt ![Stmt] !SourcePos         -- ^ default: statements
  deriving (Eq, Show)

-- ============================================================================
-- ПАРАМЕТРЫ И АРГУМЕНТЫ
-- ============================================================================

-- | Параметр функции
data Parameter = Parameter
  { paramType :: !TypeSpec            -- ^ Тип параметра
  , paramName :: !(Maybe Identifier)  -- ^ Имя параметра (может отсутствовать)
  , paramQualifiers :: ![TypeQualifier] -- ^ Квалификаторы
  , paramMemoryModel :: !(Maybe MemoryModel) -- ^ Модель памяти (8051)
  , paramPos :: !SourcePos            -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Аргумент вызова функции
data Argument = Argument
  { argExpr :: !Expr        -- ^ Выражение-аргумент
  , argPos :: !SourcePos    -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- ============================================================================
-- ИНИЦИАЛИЗАТОРЫ
-- ============================================================================

-- | Инициализатор переменной
data Initializer
  = InitExpr !Expr                    -- ^ Простое выражение
  | InitList ![Initializer]           -- ^ Список инициализаторов
  | InitDesignated !Designator !Initializer  -- ^ Назначенный инициализатор
  deriving (Eq, Show)

-- | Обозначение для назначенных инициализаторов
data Designator
  = DesignField !Identifier           -- ^ .field
  | DesignIndex !Expr                 -- ^ [index]
  deriving (Eq, Show)

-- ============================================================================
-- РАСШИРЕНИЯ ДЛЯ 8051
-- ============================================================================

-- | Спецификация прерывания для 8051
data InterruptSpec = InterruptSpec
  { intNumber :: !Int                 -- ^ Номер прерывания
  , intUsing :: !(Maybe Int)          -- ^ Номер банка регистров
  , intReentrant :: !Bool             -- ^ Реентерабельность
  , intPos :: !SourcePos              -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Декларация битовой переменной
data BitDeclaration = BitDeclaration
  { bitName :: !Identifier            -- ^ Имя битовой переменной
  , bitAddress :: !Expr               -- ^ Адрес бита (например, P1^0)
  , bitPos :: !SourcePos              -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Декларация Special Function Register
data SfrDeclaration = SfrDeclaration
  { sfrName :: !Identifier            -- ^ Имя SFR
  , sfrAddress :: !Expr               -- ^ Адрес SFR
  , sfrType :: !TypeSpec              -- ^ Тип SFR (обычно unsigned char)
  , sfrPos :: !SourcePos              -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- ============================================================================
-- УТИЛИТЫ
-- ============================================================================

-- | Получает позицию выражения
exprPos :: Expr -> SourcePos
exprPos (Expr _ pos) = pos

-- | Получает позицию оператора
stmtPos :: Stmt -> SourcePos
stmtPos (Stmt _ pos) = pos

-- | Получает позицию декларации
declPos :: Declaration -> SourcePos
declPos = declPos

-- | Красивое отображение AST (упрощенная версия)
prettyAST :: Program -> Text
prettyAST (Program decls _) = T.unlines $ map prettyTopLevel decls
  where
    prettyTopLevel :: TopLevelDecl -> Text
    prettyTopLevel = \case
      TLFunction func -> prettyFunction func
      TLDeclaration decl -> prettyDeclaration decl
      TLBitDecl bit -> prettyBitDecl bit
      TLSfrDecl sfr -> prettySfrDecl sfr
    
    prettyFunction :: FunctionDef -> Text
    prettyFunction FunctionDef{..} = 
      T.concat [ prettyTypeSpec funcReturnType
               , " "
               , identifierText funcName
               , "("
               , T.intercalate ", " (map prettyParameter funcParams)
               , ") "
               , prettyInterrupt funcInterrupt
               , prettyMemoryModel funcMemoryModel
               , "{ ... }"
               ]
    
    prettyDeclaration :: Declaration -> Text
    prettyDeclaration Declaration{..} =
      T.concat [ prettyTypeSpec declType
               , " "
               , identifierText declName
               , maybe "" (\init -> " = " <> prettyInitializer init) declInit
               , ";"
               ]
    
    prettyBitDecl :: BitDeclaration -> Text
    prettyBitDecl BitDeclaration{..} =
      T.concat [ "sbit "
               , identifierText bitName
               , " = "
               , prettyExpr bitAddress
               , ";"
               ]
    
    prettySfrDecl :: SfrDeclaration -> Text
    prettySfrDecl SfrDeclaration{..} =
      T.concat [ "sfr "
               , identifierText sfrName
               , " = "
               , prettyExpr sfrAddress
               , ";"
               ]
    
    prettyTypeSpec :: TypeSpec -> Text
    prettyTypeSpec = \case
      TSVoid -> "void"
      TSChar -> "char"
      TSInt -> "int"
      TSFloat -> "float"
      TSDouble -> "double"
      TSShort -> "short"
      TSLong -> "long"
      TSSigned -> "signed"
      TSUnsigned -> "unsigned"
      TSBit -> "bit"
      TSPointer t -> prettyTypeSpec t <> "*"
      TSArray t size -> prettyTypeSpec t <> "[" <> maybe "" prettyExpr size <> "]"
      TSStruct name _ -> "struct " <> identifierText name
      TSUnion name _ -> "union " <> identifierText name
      TSEnum name _ -> "enum " <> identifierText name
      TSTypedef name -> identifierText name
    
    prettyParameter :: Parameter -> Text
    prettyParameter Parameter{..} =
      T.concat [ prettyTypeSpec paramType
               , maybe "" (\name -> " " <> identifierText name) paramName
               ]
    
    prettyExpr :: Expr -> Text
    prettyExpr (Expr node _) = case node of
      ELiteral lit -> T.pack $ show lit
      EVariable name -> identifierText name
      EBinary op left right -> 
        "(" <> prettyExpr left <> " " <> T.pack (show op) <> " " <> prettyExpr right <> ")"
      EUnary op expr -> T.pack (show op) <> prettyExpr expr
      ECall name args -> 
        identifierText name <> "(" <> T.intercalate ", " (map prettyExpr args) <> ")"
      _ -> "..."  -- Упрощение для других случаев
    
    prettyInitializer :: Initializer -> Text
    prettyInitializer = \case
      InitExpr expr -> prettyExpr expr
      InitList inits -> "{" <> T.intercalate ", " (map prettyInitializer inits) <> "}"
      InitDesignated _ init -> prettyInitializer init  -- Упрощение
    
    prettyInterrupt :: Maybe InterruptSpec -> Text
    prettyInterrupt Nothing = ""
    prettyInterrupt (Just InterruptSpec{..}) =
      " interrupt " <> T.pack (show intNumber) <>
      maybe "" (\using -> " using " <> T.pack (show using)) intUsing
    
    prettyMemoryModel :: Maybe MemoryModel -> Text
    prettyMemoryModel Nothing = ""
    prettyMemoryModel (Just mm) = " " <> T.pack (show mm)

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ СОЗДАНИЯ AST
-- ============================================================================

-- | Создает простое выражение-переменную
mkVariable :: Identifier -> SourcePos -> Expr
mkVariable name pos = Expr (EVariable name) pos

-- | Создает простое выражение-литерал
mkLiteral :: Literal -> SourcePos -> Expr
mkLiteral lit pos = Expr (ELiteral lit) pos

-- | Создает бинарную операцию
mkBinary :: BinaryOp -> Expr -> Expr -> SourcePos -> Expr
mkBinary op left right pos = Expr (EBinary op left right) pos

-- | Создает вызов функции
mkCall :: Identifier -> [Expr] -> SourcePos -> Expr
mkCall name args pos = Expr (ECall name args) pos

-- | Создает простой оператор-выражение
mkExprStmt :: Expr -> Stmt
mkExprStmt expr = Stmt (SExpr expr) (exprPos expr)

-- | Создает оператор return
mkReturn :: Maybe Expr -> SourcePos -> Stmt
mkReturn mexpr pos = Stmt (SReturn mexpr) pos

-- | Создает составной оператор
mkCompound :: [Stmt] -> SourcePos -> CompoundStmt
mkCompound stmts pos = CompoundStmt stmts pos 