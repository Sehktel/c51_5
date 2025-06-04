{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE PatternSynonyms #-}

-- | Модуль базовых типов данных для HCL компилятора
-- 
-- Этот модуль определяет фундаментальные типы данных, используемые
-- во всех фазах компиляции. Включает типы для AT89S4051, позиции
-- в исходном коде, и базовые структуры данных.
module HCL.Types
  ( -- * Типы данных AT89S4051
    C51Type(..)
  , typeSize
  , isIntegralType
  , isPointerType
  , isBitType
  
    -- * Позиции в исходном коде
  , SourcePos(..)
  , SourceSpan(..)
  , HasSourcePos(..)
  
    -- * Идентификаторы
  , Identifier(..)
  , mkIdentifier
  , identifierText
  
    -- * Литералы
  , Literal(..)
  , LiteralType(..)
  
    -- * Операторы
  , BinaryOp(..)
  , UnaryOp(..)
  , AssignOp(..)
  
    -- * Утилиты
  , prettyType
  , prettyLiteral
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Data (Data, Typeable)
import Data.Word (Word8, Word16)
import Data.Int (Int8, Int16)

-- ============================================================================
-- ТИПЫ ДАННЫХ AT89S4051
-- ============================================================================

-- | Типы данных, поддерживаемые микроконтроллером AT89S4051
data C51Type
  = UInt8Type    -- ^ 8-битное беззнаковое целое (0-255)
  | Int8Type     -- ^ 8-битное знаковое целое (-128 до 127)
  | UInt16Type   -- ^ 16-битное беззнаковое целое (0-65535)
  | Int16Type    -- ^ 16-битное знаковое целое (-32768 до 32767)
  | BitType      -- ^ Битовая переменная (0 или 1)
  | PointerType C51Type  -- ^ Указатель на тип
  | ArrayType C51Type Int  -- ^ Массив элементов типа с размером
  | VoidType     -- ^ Тип void (только для функций)
  | FunctionType C51Type [C51Type]  -- ^ Тип функции (возвращаемый тип, параметры)
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Возвращает размер типа в байтах
typeSize :: C51Type -> Int
typeSize = \case
  UInt8Type -> 1
  Int8Type -> 1
  BitType -> 1  -- Биты упакованы, но минимальная адресация - байт
  UInt16Type -> 2
  Int16Type -> 2
  PointerType _ -> 2  -- Указатели всегда 16-битные на 8051
  ArrayType elemType count -> typeSize elemType * count
  VoidType -> 0
  FunctionType _ _ -> 0  -- Функции не имеют размера

-- | Проверяет, является ли тип целочисленным
isIntegralType :: C51Type -> Bool
isIntegralType = \case
  UInt8Type -> True
  Int8Type -> True
  UInt16Type -> True
  Int16Type -> True
  BitType -> True
  _ -> False

-- | Проверяет, является ли тип указателем
isPointerType :: C51Type -> Bool
isPointerType = \case
  PointerType _ -> True
  _ -> False

-- | Проверяет, является ли тип битовым
isBitType :: C51Type -> Bool
isBitType BitType = True
isBitType _ = False

-- ============================================================================
-- ПОЗИЦИИ В ИСХОДНОМ КОДЕ
-- ============================================================================

-- | Позиция в исходном файле
data SourcePos = SourcePos
  { sourceName :: !Text     -- ^ Имя файла
  , sourceLine :: !Int      -- ^ Номер строки (начиная с 1)
  , sourceColumn :: !Int    -- ^ Номер столбца (начиная с 1)
  } deriving (Eq, Ord, Show, Data, Typeable)

-- | Псевдоним для устранения конфликта с Text.Megaparsec.SourcePos
type HCLSourcePos = SourcePos

-- | Диапазон в исходном коде
data SourceSpan = SourceSpan
  { spanStart :: !SourcePos  -- ^ Начальная позиция
  , spanEnd :: !SourcePos    -- ^ Конечная позиция
  } deriving (Eq, Ord, Show, Data, Typeable)

-- | Класс для типов, имеющих позицию в исходном коде
class HasSourcePos a where
  getSourcePos :: a -> Maybe SourcePos

-- ============================================================================
-- ИДЕНТИФИКАТОРЫ
-- ============================================================================

-- | Идентификатор переменной, функции или типа
newtype Identifier = Identifier Text
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Создает идентификатор с валидацией
mkIdentifier :: Text -> Maybe Identifier
mkIdentifier txt
  | T.null txt = Nothing
  | T.all isValidIdentChar txt && isValidFirstChar (T.head txt) = Just (Identifier txt)
  | otherwise = Nothing
  where
    isValidFirstChar c = c == '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')
    isValidIdentChar c = isValidFirstChar c || (c >= '0' && c <= '9')

-- | Извлекает текст из идентификатора
identifierText :: Identifier -> Text
identifierText (Identifier txt) = txt

-- ============================================================================
-- ЛИТЕРАЛЫ
-- ============================================================================

-- | Тип литерала
data LiteralType
  = NumberLiteral    -- ^ Числовой литерал
  | StringLiteral    -- ^ Строковый литерал
  | CharLiteral      -- ^ Символьный литерал
  | BoolLiteral      -- ^ Булевый литерал (расширение)
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Литеральное значение
data Literal
  = IntLiteral !Int LiteralType      -- ^ Целочисленный литерал
  | StringLit !Text                  -- ^ Строковый литерал
  | CharLit !Char                    -- ^ Символьный литерал
  | BoolLit !Bool                    -- ^ Булевый литерал
  deriving (Eq, Ord, Show, Data, Typeable)

-- ============================================================================
-- ОПЕРАТОРЫ
-- ============================================================================

-- | Бинарные операторы
data BinaryOp
  = BinAdd | BinSub | BinMul | BinDiv | BinMod     -- Арифметические
  | BinEQ | BinNE | BinLT | BinLE | BinGT | BinGE  -- Сравнения
  | BinAnd | BinOr                                 -- Логические  
  | BinBitAnd | BinBitOr | BinBitXor               -- Битовые
  | BinShl | BinShr                                -- Сдвиги
  deriving (Eq, Show, Enum, Bounded)

-- | Унарные операторы
data UnaryOp
  = UnaryPlus | UnaryMinus | UnaryNot | UnaryBitNot  -- Арифметические/логические
  | UnaryPreInc | UnaryPostInc                       -- Инкремент
  | UnaryPreDec | UnaryPostDec                       -- Декремент
  | UnaryDeref | UnaryAddrOf                         -- Указатели
  | UnarySizeOf                                      -- sizeof
  deriving (Eq, Show, Enum, Bounded)

-- | Операторы присваивания
data AssignOp
  = Assign                                           -- =
  | AssignAdd | AssignSub | AssignMul | AssignDiv    -- +=, -=, *=, /=
  | AssignMod | AssignShl | AssignShr                -- %=, <<=, >>=
  | AssignBitAnd | AssignBitOr | AssignBitXor        -- &=, |=, ^=
  deriving (Eq, Show, Enum, Bounded)

-- ============================================================================
-- УТИЛИТЫ ДЛЯ КРАСИВОГО ВЫВОДА
-- ============================================================================

-- | Красивый вывод типа
prettyType :: C51Type -> Text
prettyType = \case
  UInt8Type -> "unsigned char"
  Int8Type -> "signed char"
  UInt16Type -> "unsigned int"
  Int16Type -> "int"
  BitType -> "bit"
  PointerType t -> prettyType t <> "*"
  ArrayType t size -> prettyType t <> "[" <> T.pack (show size) <> "]"
  VoidType -> "void"
  FunctionType retType paramTypes ->
    prettyType retType <> "(" <> T.intercalate ", " (map prettyType paramTypes) <> ")"

-- | Красивый вывод литерала
prettyLiteral :: Literal -> Text
prettyLiteral = \case
  IntLiteral n _ -> T.pack (show n)
  StringLit s -> "\"" <> s <> "\""
  CharLit c -> "'" <> T.singleton c <> "'"
  BoolLit True -> "true"
  BoolLit False -> "false" 