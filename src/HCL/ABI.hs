{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Модуль Application Binary Interface для AT89S4051
--
-- Реализует спецификацию ABI согласно документации ABI.tex:
-- * Регистровая архитектура с классификацией регистров
-- * Соглашения о вызовах функций
-- * Модели памяти и адресации
-- * Типизация данных и выравнивание
-- * Система прерываний
--
-- Архитектурные характеристики:
-- * 8-битная гарвардская архитектура
-- * 4KB ROM, 128B RAM
-- * 32 регистра общего назначения
-- * Аппаратный стек глубиной 32 уровня
module HCL.ABI
  ( -- * Регистровая архитектура
    Register(..)
  , RegisterClass(..)
  , PhysicalRegister(..)
  , VirtualRegister(..)
  
    -- * Соглашения о вызовах
  , CallingConvention(..)
  , ArgumentPassing(..)
  , ReturnConvention(..)
  
    -- * Модели памяти
  , MemorySegment(..)
  , AddressSpace(..)
  , MemoryLayout(..)
  
    -- * Типизация данных
  , ABIType(..)
  , TypeAlignment(..)
  , TypeSize(..)
  
    -- * Система прерываний
  , InterruptVector(..)
  , InterruptPriority(..)
  
    -- * ABI конфигурация
  , ABIConfig(..)
  , defaultABIConfig
  
    -- * Утилиты
  , getRegisterClass
  , isCallerSaved
  , isCalleeSaved
  , getArgumentRegisters
  , getReturnRegister
  , calculateStackOffset
  , alignType
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Word (Word8, Word16)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set

import HCL.Types (Identifier(..), SourcePos(..))

-- ============================================================================
-- РЕГИСТРОВАЯ АРХИТЕКТУРА
-- ============================================================================

-- | Физические регистры AT89S4051
data PhysicalRegister
  -- Основные вычислительные регистры
  = RegA              -- ^ Аккумулятор (примарный регистр арифметических операций)
  | RegB              -- ^ Вспомогательный регистр (умножение/деление)
  
  -- Регистры общего назначения (банк 0)
  | RegR0 | RegR1 | RegR2 | RegR3  -- ^ Сохраняемые регистры
  | RegR4 | RegR5 | RegR6 | RegR7  -- ^ Временные регистры
  
  -- Адресные регистры
  | RegDPTR           -- ^ 16-битный указатель данных
  | RegDPL            -- ^ Младший байт DPTR
  | RegDPH            -- ^ Старший байт DPTR
  | RegSP             -- ^ Указатель стека
  
  -- Статусные регистры
  | RegPSW            -- ^ Program Status Word (флаги состояния)
  
  -- Специальные регистры
  | RegPC             -- ^ Program Counter (счетчик команд)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Виртуальные регистры (неограниченное количество)
newtype VirtualRegister = VirtualRegister Int
  deriving (Eq, Ord, Show)

-- | Общий тип регистра
data Register
  = Physical !PhysicalRegister    -- ^ Физический регистр
  | Virtual !VirtualRegister      -- ^ Виртуальный регистр
  deriving (Eq, Ord, Show)

-- | Классификация регистров по функциональному назначению
data RegisterClass
  = Accumulator       -- ^ Аккумулятор (A)
  | Auxiliary         -- ^ Вспомогательный (B)
  | GeneralPurpose    -- ^ Общего назначения (R0-R7)
  | Address           -- ^ Адресные (DPTR, SP)
  | Status            -- ^ Статусные (PSW)
  | Special           -- ^ Специальные (PC)
  deriving (Eq, Show, Enum, Bounded)

-- ============================================================================
-- СОГЛАШЕНИЯ О ВЫЗОВАХ ФУНКЦИЙ
-- ============================================================================

-- | Соглашение о вызовах функций
data CallingConvention = CallingConvention
  { ccArgumentPassing :: !ArgumentPassing     -- ^ Передача аргументов
  , ccReturnConvention :: !ReturnConvention   -- ^ Возврат значений
  , ccCallerSaved :: !(Set PhysicalRegister)  -- ^ Регистры, сохраняемые вызывающим
  , ccCalleeSaved :: !(Set PhysicalRegister)  -- ^ Регистры, сохраняемые вызываемым
  , ccStackAlignment :: !Int                  -- ^ Выравнивание стека
  , ccMaxFunctionSize :: !Int                 -- ^ Максимальный размер функции (256 байт)
  } deriving (Eq, Show)

-- | Механизм передачи аргументов
data ArgumentPassing = ArgumentPassing
  { apRegisterArgs :: ![PhysicalRegister]     -- ^ Регистры для аргументов (R0-R3)
  , apMaxRegisterArgs :: !Int                 -- ^ Максимум аргументов в регистрах (4)
  , apStackGrowsDown :: !Bool                 -- ^ Направление роста стека
  , apStackAlignment :: !Int                  -- ^ Выравнивание аргументов в стеке
  } deriving (Eq, Show)

-- | Соглашение о возврате значений
data ReturnConvention = ReturnConvention
  { rcReturnRegister :: !PhysicalRegister     -- ^ Регистр возврата (A)
  , rcReturnOnStack :: !Bool                  -- ^ Возврат через стек для больших значений
  , rcMaxReturnSize :: !Int                   -- ^ Максимальный размер возвращаемого значения
  } deriving (Eq, Show)

-- ============================================================================
-- МОДЕЛИ ПАМЯТИ
-- ============================================================================

-- | Сегменты памяти AT89S4051
data MemorySegment
  = CodeSegment       -- ^ Память программы (ROM) [0x0000, 0x0FFF]
  | DataSegment       -- ^ Оперативная память (RAM) [0x0000, 0x007F]
  | SFRSegment        -- ^ Регистры специальных функций [0x80, 0xFF]
  | StackSegment      -- ^ Сегмент стека
  deriving (Eq, Show, Enum, Bounded)

-- | Адресное пространство
data AddressSpace = AddressSpace
  { asCodeStart :: !Word16        -- ^ Начало памяти программы (0x0000)
  , asCodeEnd :: !Word16          -- ^ Конец памяти программы (0x0FFF)
  , asDataStart :: !Word16        -- ^ Начало RAM (0x0000)
  , asDataEnd :: !Word16          -- ^ Конец RAM (0x007F)
  , asSFRStart :: !Word16         -- ^ Начало SFR (0x80)
  , asSFREnd :: !Word16           -- ^ Конец SFR (0xFF)
  , asStackStart :: !Word16       -- ^ Начало стека
  , asStackSize :: !Int           -- ^ Размер стека (32 уровня)
  } deriving (Eq, Show)

-- | Схема размещения в памяти
data MemoryLayout = MemoryLayout
  { mlAddressSpace :: !AddressSpace           -- ^ Адресное пространство
  , mlSegmentSizes :: !(Map MemorySegment Int) -- ^ Размеры сегментов
  , mlAlignment :: !(Map ABIType Int)         -- ^ Выравнивание типов
  } deriving (Eq, Show)

-- ============================================================================
-- ТИПИЗАЦИЯ ДАННЫХ
-- ============================================================================

-- | Типы данных в ABI
data ABIType
  = UnsignedByte      -- ^ Беззнаковый байт: 8 бит [0, 255]
  | SignedByte        -- ^ Знаковый байт: 8 бит [-128, 127]
  | UnsignedWord      -- ^ Беззнаковое слово: 16 бит [0, 65535]
  | SignedWord        -- ^ Знаковое слово: 16 бит [-32768, 32767]
  | Pointer           -- ^ Указатель: 16 бит, выравнивание 1 байт
  | BitType           -- ^ Битовый тип (специфично для 8051)
  deriving (Eq, Show, Enum, Bounded)

-- | Выравнивание типов
newtype TypeAlignment = TypeAlignment Int
  deriving (Eq, Show)

-- | Размер типов
newtype TypeSize = TypeSize Int
  deriving (Eq, Show)

-- ============================================================================
-- СИСТЕМА ПРЕРЫВАНИЙ
-- ============================================================================

-- | Векторы прерываний
data InterruptVector
  = INT0Vector        -- ^ Внешнее прерывание 0 (0x0003)
  | Timer0Vector      -- ^ Прерывание таймера 0 (0x000B)
  | INT1Vector        -- ^ Внешнее прерывание 1 (0x0013)
  | Timer1Vector      -- ^ Прерывание таймера 1 (0x001B)
  | SerialVector      -- ^ Прерывание последовательного порта (0x0023)
  deriving (Eq, Show, Enum, Bounded)

-- | Приоритеты прерываний
data InterruptPriority
  = HighPriority      -- ^ Высокий приоритет
  | LowPriority       -- ^ Низкий приоритет
  deriving (Eq, Show, Enum, Bounded)

-- ============================================================================
-- ABI КОНФИГУРАЦИЯ
-- ============================================================================

-- | Полная конфигурация ABI
data ABIConfig = ABIConfig
  { abiCallingConvention :: !CallingConvention  -- ^ Соглашения о вызовах
  , abiMemoryLayout :: !MemoryLayout            -- ^ Схема памяти
  , abiInterruptVectors :: !(Map InterruptVector Word16) -- ^ Таблица векторов
  , abiRegisterClasses :: !(Map PhysicalRegister RegisterClass) -- ^ Классы регистров
  } deriving (Eq, Show)

-- | Конфигурация ABI по умолчанию для AT89S4051
defaultABIConfig :: ABIConfig
defaultABIConfig = ABIConfig
  { abiCallingConvention = defaultCallingConvention
  , abiMemoryLayout = defaultMemoryLayout
  , abiInterruptVectors = defaultInterruptVectors
  , abiRegisterClasses = defaultRegisterClasses
  }

-- | Соглашение о вызовах по умолчанию
defaultCallingConvention :: CallingConvention
defaultCallingConvention = CallingConvention
  { ccArgumentPassing = ArgumentPassing
      { apRegisterArgs = [RegR0, RegR1, RegR2, RegR3]  -- До 4 аргументов в регистрах
      , apMaxRegisterArgs = 4
      , apStackGrowsDown = True
      , apStackAlignment = 1
      }
  , ccReturnConvention = ReturnConvention
      { rcReturnRegister = RegA                         -- Возврат через аккумулятор
      , rcReturnOnStack = False
      , rcMaxReturnSize = 1
      }
  , ccCallerSaved = Set.fromList [RegA, RegB, RegR4, RegR5, RegR6, RegR7]  -- Временные
  , ccCalleeSaved = Set.fromList [RegR0, RegR1, RegR2, RegR3]              -- Сохраняемые
  , ccStackAlignment = 1
  , ccMaxFunctionSize = 256                             -- Ограничение архитектуры
  }

-- | Схема памяти по умолчанию
defaultMemoryLayout :: MemoryLayout
defaultMemoryLayout = MemoryLayout
  { mlAddressSpace = AddressSpace
      { asCodeStart = 0x0000
      , asCodeEnd = 0x0FFF      -- 4KB ROM
      , asDataStart = 0x0000
      , asDataEnd = 0x007F      -- 128B RAM
      , asSFRStart = 0x80
      , asSFREnd = 0xFF
      , asStackStart = 0x08     -- Начало стека после регистров
      , asStackSize = 32        -- 32 уровня
      }
  , mlSegmentSizes = Map.fromList
      [ (CodeSegment, 4096)
      , (DataSegment, 128)
      , (SFRSegment, 128)
      , (StackSegment, 32)
      ]
  , mlAlignment = Map.fromList
      [ (UnsignedByte, 1)
      , (SignedByte, 1)
      , (UnsignedWord, 1)       -- 8051 не требует выравнивания
      , (SignedWord, 1)
      , (Pointer, 1)
      , (BitType, 1)
      ]
  }

-- | Векторы прерываний по умолчанию
defaultInterruptVectors :: Map InterruptVector Word16
defaultInterruptVectors = Map.fromList
  [ (INT0Vector, 0x0003)
  , (Timer0Vector, 0x000B)
  , (INT1Vector, 0x0013)
  , (Timer1Vector, 0x001B)
  , (SerialVector, 0x0023)
  ]

-- | Классы регистров по умолчанию
defaultRegisterClasses :: Map PhysicalRegister RegisterClass
defaultRegisterClasses = Map.fromList
  [ (RegA, Accumulator)
  , (RegB, Auxiliary)
  , (RegR0, GeneralPurpose), (RegR1, GeneralPurpose)
  , (RegR2, GeneralPurpose), (RegR3, GeneralPurpose)
  , (RegR4, GeneralPurpose), (RegR5, GeneralPurpose)
  , (RegR6, GeneralPurpose), (RegR7, GeneralPurpose)
  , (RegDPTR, Address), (RegDPL, Address), (RegDPH, Address)
  , (RegSP, Address)
  , (RegPSW, Status)
  , (RegPC, Special)
  ]

-- ============================================================================
-- УТИЛИТЫ
-- ============================================================================

-- | Получает класс регистра
getRegisterClass :: PhysicalRegister -> ABIConfig -> RegisterClass
getRegisterClass reg config = 
  Map.findWithDefault GeneralPurpose reg (abiRegisterClasses config)

-- | Проверяет, сохраняется ли регистр вызывающим
isCallerSaved :: PhysicalRegister -> ABIConfig -> Bool
isCallerSaved reg config = 
  Set.member reg (ccCallerSaved $ abiCallingConvention config)

-- | Проверяет, сохраняется ли регистр вызываемым
isCalleeSaved :: PhysicalRegister -> ABIConfig -> Bool
isCalleeSaved reg config = 
  Set.member reg (ccCalleeSaved $ abiCallingConvention config)

-- | Получает регистры для передачи аргументов
getArgumentRegisters :: ABIConfig -> [PhysicalRegister]
getArgumentRegisters config = 
  apRegisterArgs $ ccArgumentPassing $ abiCallingConvention config

-- | Получает регистр для возврата значения
getReturnRegister :: ABIConfig -> PhysicalRegister
getReturnRegister config = 
  rcReturnRegister $ ccReturnConvention $ abiCallingConvention config

-- | Вычисляет смещение в стеке
calculateStackOffset :: [ABIType] -> ABIConfig -> Int
calculateStackOffset types config = 
  sum $ map (getTypeSize config) types
  where
    getTypeSize _ UnsignedByte = 1
    getTypeSize _ SignedByte = 1
    getTypeSize _ UnsignedWord = 2
    getTypeSize _ SignedWord = 2
    getTypeSize _ Pointer = 2
    getTypeSize _ BitType = 1

-- | Выравнивает тип данных
alignType :: ABIType -> ABIConfig -> Int
alignType abiType config = 
  Map.findWithDefault 1 abiType (mlAlignment $ abiMemoryLayout config)

-- ============================================================================
-- ДОПОЛНИТЕЛЬНЫЕ УТИЛИТЫ ДЛЯ КОДОГЕНЕРАЦИИ
-- ============================================================================

-- | Получает размер типа в байтах
getABITypeSize :: ABIType -> Int
getABITypeSize = \case
  UnsignedByte -> 1
  SignedByte -> 1
  UnsignedWord -> 2
  SignedWord -> 2
  Pointer -> 2
  BitType -> 1

-- | Проверяет, помещается ли тип в регистр
fitsInRegister :: ABIType -> Bool
fitsInRegister = \case
  UnsignedByte -> True
  SignedByte -> True
  BitType -> True
  _ -> False

-- | Получает адрес вектора прерывания
getInterruptAddress :: InterruptVector -> ABIConfig -> Maybe Word16
getInterruptAddress vector config = 
  Map.lookup vector (abiInterruptVectors config)

-- | Проверяет валидность адреса для сегмента
isValidAddress :: Word16 -> MemorySegment -> ABIConfig -> Bool
isValidAddress addr segment config = 
  let as = mlAddressSpace $ abiMemoryLayout config
  in case segment of
    CodeSegment -> addr >= asCodeStart as && addr <= asCodeEnd as
    DataSegment -> addr >= asDataStart as && addr <= asDataEnd as
    SFRSegment -> addr >= asSFRStart as && addr <= asSFREnd as
    StackSegment -> addr >= asStackStart as && 
                   addr <= (asStackStart as + fromIntegral (asStackSize as)) 