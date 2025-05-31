{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Генератор кода для микроконтроллера AT89S4051
--
-- Этот модуль преобразует низкоуровневое промежуточное представление (LIR)
-- в ассемблерный код для микроконтроллера AT89S4051. Генератор учитывает
-- архитектурные особенности 8051 и оптимизирует код для минимального
-- размера и максимальной производительности.
--
-- Особенности генерации:
-- * Эффективное использование аккумулятора и регистров
-- * Оптимизация инструкций для минимального размера кода
-- * Поддержка всех режимов адресации AT89S4051
-- * Генерация отладочной информации
-- * Интеграция с линкером и ассемблером
-- * Поддержка прерываний и специальных функций
module HCL.CodeGen
  ( -- * Основные функции генерации
    generateCode
  , generateFunction
  , generateInstruction
  , generateExpression
  
    -- * Генератор и контекст
  , CodeGenerator
  , CodeGenContext(..)
  , CodeGenState(..)
  , CodeGenResult(..)
  
    -- * Ассемблерный код
  , AssemblyCode(..)
  , AssemblyInstruction(..)
  , AssemblyOperand(..)
  , AssemblyDirective(..)
  , AssemblyLabel(..)
  
    -- * Оптимизации кода
  , CodeOptimization(..)
  , PeepholeOptimization(..)
  , InstructionSelection(..)
  , AddressingModeOptimization(..)
  
    -- * Архитектурные особенности
  , AT89S4051Features(..)
  , InstructionEncoding(..)
  , AddressingMode(..)
  , RegisterUsage(..)
  
    -- * Отладочная информация
  , DebugInfo(..)
  , SourceMapping(..)
  , SymbolInfo(..)
  
    -- * Утилиты
  , optimizeAssembly
  , validateCode
  , estimateCodeSize
  , generateDebugInfo
  ) where

import Control.Monad.State.Strict (StateT, runStateT, get, put, modify, gets)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, catMaybes, isJust, mapMaybe)
import Data.List (foldl', find, nub, intercalate)
import Data.Word (Word8, Word16)
import Data.Bits ((.&.), (.|.), xor, shiftL, shiftR)
import qualified Data.Vector as V

import HCL.Types (SourcePos(..), Identifier(..), identifierText)
import HCL.IR.LIR (LIRProgram(..), LIRFunction(..), LIRInstruction(..), LIROperand(..), 
                   LIRBasicBlock(..), LIRRegister(..), LIRImmediate(..), LIRMemoryRef(..))
import HCL.ABI (PhysicalRegister(..), ABIConfig, defaultABIConfig)
import HCL.Error (CompilerError(..), Diagnostic(..), DiagnosticLevel(..))

-- ============================================================================
-- ОСНОВНЫЕ ТИПЫ
-- ============================================================================

-- | Монада генератора кода
type CodeGenerator = ReaderT CodeGenContext (StateT CodeGenState (WriterT [Diagnostic] (ExceptT CompilerError IO)))

-- | Контекст генерации кода
data CodeGenContext = CodeGenContext
  { cgcTargetArch :: !Text                        -- ^ Целевая архитектура
  , cgcOptimizationLevel :: !Int                  -- ^ Уровень оптимизации
  , cgcDebugInfo :: !Bool                         -- ^ Генерировать отладочную информацию
  , cgcFeatures :: !AT89S4051Features             -- ^ Особенности архитектуры
  , cgcABIConfig :: !ABIConfig                    -- ^ Конфигурация ABI
  } deriving (Eq, Show)

-- | Состояние генератора кода
data CodeGenState = CodeGenState
  { cgsInstructions :: ![AssemblyInstruction]     -- ^ Сгенерированные инструкции
  , cgsLabels :: !(Map Text Int)                  -- ^ Таблица меток
  , cgsCurrentAddress :: !Word16                  -- ^ Текущий адрес
  , cgsRegisterUsage :: !(Map PhysicalRegister Bool) -- ^ Использование регистров
  , cgsStackOffset :: !Int                        -- ^ Смещение стека
  , cgsDebugMappings :: ![SourceMapping]          -- ^ Отображения исходного кода
  , cgsSymbols :: ![SymbolInfo]                   -- ^ Информация о символах
  } deriving (Eq, Show)

-- | Результат генерации кода
data CodeGenResult = CodeGenResult
  { cgrAssemblyCode :: !AssemblyCode              -- ^ Ассемблерный код
  , cgrDebugInfo :: !(Maybe DebugInfo)            -- ^ Отладочная информация
  , cgrDiagnostics :: ![Diagnostic]               -- ^ Диагностические сообщения
  , cgrCodeSize :: !Int                           -- ^ Размер кода в байтах
  , cgrSuccess :: !Bool                           -- ^ Успешность генерации
  } deriving (Eq, Show)

-- ============================================================================
-- АССЕМБЛЕРНЫЙ КОД
-- ============================================================================

-- | Ассемблерный код
data AssemblyCode = AssemblyCode
  { acInstructions :: ![AssemblyInstruction]      -- ^ Инструкции
  , acDirectives :: ![AssemblyDirective]          -- ^ Директивы ассемблера
  , acLabels :: ![AssemblyLabel]                  -- ^ Метки
  , acComments :: !(Map Int Text)                 -- ^ Комментарии к строкам
  } deriving (Eq, Show)

-- | Ассемблерная инструкция
data AssemblyInstruction = AssemblyInstruction
  { aiMnemonic :: !Text                           -- ^ Мнемоника инструкции
  , aiOperands :: ![AssemblyOperand]              -- ^ Операнды
  , aiAddress :: !Word16                          -- ^ Адрес инструкции
  , aiSize :: !Int                                -- ^ Размер в байтах
  , aiCycles :: !Int                              -- ^ Количество циклов
  , aiComment :: !(Maybe Text)                    -- ^ Комментарий
  , aiSourcePos :: !(Maybe SourcePos)             -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Операнд ассемблерной инструкции
data AssemblyOperand
  = AsmRegister !PhysicalRegister                 -- ^ Регистр
  | AsmImmediate !Word16                          -- ^ Непосредственное значение
  | AsmAddress !Word16                            -- ^ Адрес
  | AsmLabel !Text                                -- ^ Метка
  | AsmIndirect !PhysicalRegister                 -- ^ Косвенная адресация
  | AsmIndexed !PhysicalRegister !Word8           -- ^ Индексированная адресация
  | AsmBitAddress !Word8 !Word8                   -- ^ Битовый адрес
  deriving (Eq, Show)

-- | Директива ассемблера
data AssemblyDirective
  = AsmOrg !Word16                                -- ^ Установка адреса
  | AsmEqu !Text !Word16                          -- ^ Определение константы
  | AsmDb ![Word8]                                -- ^ Данные байт
  | AsmDw ![Word16]                               -- ^ Данные слов
  | AsmDs !Int                                    -- ^ Резервирование места
  | AsmEnd                                        -- ^ Конец программы
  deriving (Eq, Show)

-- | Метка ассемблера
data AssemblyLabel = AssemblyLabel
  { alName :: !Text                               -- ^ Имя метки
  , alAddress :: !Word16                          -- ^ Адрес метки
  , alType :: !LabelType                          -- ^ Тип метки
  } deriving (Eq, Show)

-- | Тип метки
data LabelType
  = CodeLabel                                     -- ^ Метка кода
  | DataLabel                                     -- ^ Метка данных
  | FunctionLabel                                 -- ^ Метка функции
  | InterruptLabel                                -- ^ Метка прерывания
  deriving (Eq, Show, Enum, Bounded)

-- ============================================================================
-- АРХИТЕКТУРНЫЕ ОСОБЕННОСТИ AT89S4051
-- ============================================================================

-- | Особенности архитектуры AT89S4051
data AT89S4051Features = AT89S4051Features
  { af51ROMSize :: !Word16                        -- ^ Размер ROM (4KB)
  , af51RAMSize :: !Word8                         -- ^ Размер RAM (128 байт)
  , af51StackSize :: !Word8                       -- ^ Размер стека
  , af51Interrupts :: ![InterruptVector]          -- ^ Векторы прерываний
  , af51SFRs :: !(Map Text Word8)                 -- ^ Специальные регистры
  , af51BitAddresses :: !(Map Text Word8)         -- ^ Битовые адреса
  , af51TimingModel :: !TimingModel               -- ^ Модель времени выполнения
  } deriving (Eq, Show)

-- | Вектор прерывания
data InterruptVector = InterruptVector
  { ivNumber :: !Word8                            -- ^ Номер прерывания
  , ivAddress :: !Word16                          -- ^ Адрес вектора
  , ivName :: !Text                               -- ^ Название прерывания
  , ivPriority :: !Int                            -- ^ Приоритет
  } deriving (Eq, Show)

-- | Модель времени выполнения
data TimingModel = TimingModel
  { tmClockFrequency :: !Word32                   -- ^ Частота тактирования
  , tmInstructionCycles :: !(Map Text Int)        -- ^ Циклы инструкций
  , tmMemoryAccessTime :: !Int                    -- ^ Время доступа к памяти
  } deriving (Eq, Show)

-- | Кодирование инструкции
data InstructionEncoding = InstructionEncoding
  { ieOpcode :: !Word8                            -- ^ Код операции
  , ieOperands :: ![OperandEncoding]              -- ^ Кодирование операндов
  , ieSize :: !Int                                -- ^ Размер инструкции
  , ieCycles :: !Int                              -- ^ Количество циклов
  } deriving (Eq, Show)

-- | Кодирование операнда
data OperandEncoding
  = DirectByte !Word8                             -- ^ Прямой байт
  | ImmediateByte !Word8                          -- ^ Непосредственный байт
  | RelativeOffset !Int8                          -- ^ Относительное смещение
  | AbsoluteAddress !Word16                       -- ^ Абсолютный адрес
  deriving (Eq, Show)

-- | Режим адресации
data AddressingMode
  = RegisterMode !PhysicalRegister                -- ^ Регистровый режим
  | ImmediateMode !Word16                         -- ^ Непосредственный режим
  | DirectMode !Word8                             -- ^ Прямой режим
  | IndirectMode !PhysicalRegister                -- ^ Косвенный режим
  | IndexedMode !PhysicalRegister !Word8          -- ^ Индексированный режим
  | BitMode !Word8 !Word8                         -- ^ Битовый режим
  deriving (Eq, Show)

-- | Использование регистров
data RegisterUsage = RegisterUsage
  { ruAccumulator :: !Bool                        -- ^ Использование аккумулятора
  , ruBRegister :: !Bool                          -- ^ Использование регистра B
  , ruDataPointer :: !Bool                        -- ^ Использование DPTR
  , ruWorkingRegs :: !(Set Word8)                 -- ^ Используемые рабочие регистры
  , ruStackPointer :: !Bool                       -- ^ Использование SP
  } deriving (Eq, Show)

-- ============================================================================
-- ОПТИМИЗАЦИИ КОДА
-- ============================================================================

-- | Оптимизация кода
data CodeOptimization
  = PeepholeOpt !PeepholeOptimization             -- ^ Оптимизация "замочной скважины"
  | InstructionSelectionOpt !InstructionSelection -- ^ Выбор инструкций
  | AddressingModeOpt !AddressingModeOptimization -- ^ Оптимизация режимов адресации
  | BranchOpt                                     -- ^ Оптимизация ветвлений
  | ConstantFoldingOpt                            -- ^ Свёртка констант
  deriving (Eq, Show)

-- | Оптимизация "замочной скважины"
data PeepholeOptimization = PeepholeOptimization
  { poPattern :: ![AssemblyInstruction]           -- ^ Паттерн для замены
  , poReplacement :: ![AssemblyInstruction]       -- ^ Замена
  , poCondition :: !(Maybe Text)                  -- ^ Условие применения
  , poBenefit :: !Int                             -- ^ Выгода оптимизации
  } deriving (Eq, Show)

-- | Выбор инструкций
data InstructionSelection = InstructionSelection
  { isOperation :: !Text                          -- ^ Операция
  , isAlternatives :: ![AssemblyInstruction]      -- ^ Альтернативные инструкции
  , isSelector :: !Text                           -- ^ Критерий выбора
  } deriving (Eq, Show)

-- | Оптимизация режимов адресации
data AddressingModeOptimization = AddressingModeOptimization
  { amoOriginalMode :: !AddressingMode            -- ^ Исходный режим
  , amoOptimizedMode :: !AddressingMode           -- ^ Оптимизированный режим
  , amoSavings :: !Int                            -- ^ Экономия в байтах/циклах
  } deriving (Eq, Show)

-- ============================================================================
-- ОТЛАДОЧНАЯ ИНФОРМАЦИЯ
-- ============================================================================

-- | Отладочная информация
data DebugInfo = DebugInfo
  { diSourceMappings :: ![SourceMapping]          -- ^ Отображения исходного кода
  , diSymbols :: ![SymbolInfo]                    -- ^ Информация о символах
  , diLineNumbers :: !(Map Word16 Int)            -- ^ Номера строк
  , diVariables :: ![VariableInfo]                -- ^ Информация о переменных
  } deriving (Eq, Show)

-- | Отображение исходного кода
data SourceMapping = SourceMapping
  { smSourcePos :: !SourcePos                     -- ^ Позиция в исходном коде
  , smAddress :: !Word16                          -- ^ Адрес в машинном коде
  , smLength :: !Int                              -- ^ Длина в байтах
  } deriving (Eq, Show)

-- | Информация о символе
data SymbolInfo = SymbolInfo
  { siName :: !Text                               -- ^ Имя символа
  , siAddress :: !Word16                          -- ^ Адрес символа
  , siType :: !SymbolType                         -- ^ Тип символа
  , siSize :: !Int                                -- ^ Размер символа
  , siScope :: !Text                              -- ^ Область видимости
  } deriving (Eq, Show)

-- | Тип символа
data SymbolType
  = FunctionSymbol                                -- ^ Функция
  | VariableSymbol                                -- ^ Переменная
  | LabelSymbol                                   -- ^ Метка
  | ConstantSymbol                                -- ^ Константа
  deriving (Eq, Show, Enum, Bounded)

-- | Информация о переменной
data VariableInfo = VariableInfo
  { viName :: !Text                               -- ^ Имя переменной
  , viType :: !Text                               -- ^ Тип переменной
  , viLocation :: !VariableLocation               -- ^ Расположение
  , viScope :: !(Word16, Word16)                  -- ^ Область видимости (начало, конец)
  } deriving (Eq, Show)

-- | Расположение переменной
data VariableLocation
  = RegisterLocation !PhysicalRegister            -- ^ В регистре
  | MemoryLocation !Word16                        -- ^ В памяти
  | StackLocation !Int                            -- ^ В стеке
  deriving (Eq, Show)

-- ============================================================================
-- ОСНОВНЫЕ ФУНКЦИИ ГЕНЕРАЦИИ
-- ============================================================================

-- | Генерация кода из LIR программы
generateCode :: LIRProgram -> CodeGenerator CodeGenResult
generateCode (LIRProgram functions _) = do
  context <- ask
  
  -- Инициализируем состояние
  initializeCodeGenState
  
  -- Генерируем код для каждой функции
  mapM_ generateFunction functions
  
  -- Применяем оптимизации
  when (cgcOptimizationLevel context > 0) $
    optimizeGeneratedCode
  
  -- Собираем результат
  instructions <- gets cgsInstructions
  debugMappings <- gets cgsDebugMappings
  symbols <- gets cgsSymbols
  
  let assemblyCode = AssemblyCode
        { acInstructions = reverse instructions
        , acDirectives = generateDirectives
        , acLabels = generateLabels
        , acComments = Map.empty
        }
  
  let debugInfo = if cgcDebugInfo context
        then Just $ DebugInfo debugMappings symbols Map.empty []
        else Nothing
  
  let codeSize = sum $ map aiSize (acInstructions assemblyCode)
  
  return $ CodeGenResult
    { cgrAssemblyCode = assemblyCode
    , cgrDebugInfo = debugInfo
    , cgrDiagnostics = []
    , cgrCodeSize = codeSize
    , cgrSuccess = True
    }

-- | Генерация кода функции
generateFunction :: LIRFunction -> CodeGenerator ()
generateFunction LIRFunction{..} = do
  -- Генерируем метку функции
  generateFunctionLabel lirFuncName
  
  -- Генерируем пролог функции
  generateFunctionPrologue lirFuncParams
  
  -- Генерируем код базовых блоков
  mapM_ generateBasicBlock lirFuncBody
  
  -- Генерируем эпилог функции
  generateFunctionEpilogue

-- | Генерация инструкции
generateInstruction :: LIRInstruction -> CodeGenerator ()
generateInstruction instr = case instr of
  LIRMove src dst -> generateMoveInstruction src dst
  LIRAdd src1 src2 dst -> generateArithmeticInstruction "ADD" src1 src2 dst
  LIRSub src1 src2 dst -> generateArithmeticInstruction "SUBB" src1 src2 dst
  LIRMul src1 src2 dst -> generateMultiplyInstruction src1 src2 dst
  LIRDiv src1 src2 dst -> generateDivideInstruction src1 src2 dst
  LIRLoad addr dst -> generateLoadInstruction addr dst
  LIRStore src addr -> generateStoreInstruction src addr
  LIRBranch label -> generateBranchInstruction label
  LIRBranchIf cond label -> generateConditionalBranch cond label
  LIRCall func args -> generateCallInstruction func args
  LIRReturn val -> generateReturnInstruction val
  LIRLabel label -> generateLabelInstruction label
  LIRNop -> generateNopInstruction

-- | Генерация выражения
generateExpression :: LIROperand -> CodeGenerator AssemblyOperand
generateExpression operand = case operand of
  LIRRegisterOp reg -> return $ AsmRegister (convertRegister reg)
  LIRImmediateOp imm -> return $ AsmImmediate (convertImmediate imm)
  LIRMemoryOp memRef -> generateMemoryOperand memRef
  LIRLabelOp label -> return $ AsmLabel (identifierText label)

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Конфигурация по умолчанию
defaultCodeGenContext :: CodeGenContext
defaultCodeGenContext = CodeGenContext
  { cgcTargetArch = "AT89S4051"
  , cgcOptimizationLevel = 2
  , cgcDebugInfo = True
  , cgcFeatures = defaultAT89S4051Features
  , cgcABIConfig = defaultABIConfig
  }

-- | Особенности AT89S4051 по умолчанию
defaultAT89S4051Features :: AT89S4051Features
defaultAT89S4051Features = AT89S4051Features
  { af51ROMSize = 4096
  , af51RAMSize = 128
  , af51StackSize = 32
  , af51Interrupts = defaultInterruptVectors
  , af51SFRs = defaultSFRs
  , af51BitAddresses = defaultBitAddresses
  , af51TimingModel = defaultTimingModel
  }

-- | Векторы прерываний по умолчанию
defaultInterruptVectors :: [InterruptVector]
defaultInterruptVectors =
  [ InterruptVector 0 0x0003 "INT0" 1
  , InterruptVector 1 0x000B "TIMER0" 2
  , InterruptVector 2 0x0013 "INT1" 3
  , InterruptVector 3 0x001B "TIMER1" 4
  , InterruptVector 4 0x0023 "SERIAL" 5
  ]

-- | SFR по умолчанию
defaultSFRs :: Map Text Word8
defaultSFRs = Map.fromList
  [ ("ACC", 0xE0)
  , ("B", 0xF0)
  , ("PSW", 0xD0)
  , ("SP", 0x81)
  , ("DPL", 0x82)
  , ("DPH", 0x83)
  , ("P1", 0x90)
  , ("P3", 0xB0)
  , ("IE", 0xA8)
  , ("IP", 0xB8)
  , ("TMOD", 0x89)
  , ("TCON", 0x88)
  , ("TH0", 0x8C)
  , ("TL0", 0x8A)
  , ("TH1", 0x8D)
  , ("TL1", 0x8B)
  , ("SCON", 0x98)
  , ("SBUF", 0x99)
  ]

-- | Битовые адреса по умолчанию
defaultBitAddresses :: Map Text Word8
defaultBitAddresses = Map.fromList
  [ ("CY", 0xD7)
  , ("AC", 0xD6)
  , ("F0", 0xD5)
  , ("RS1", 0xD4)
  , ("RS0", 0xD3)
  , ("OV", 0xD2)
  , ("P", 0xD0)
  , ("EA", 0xAF)
  , ("ES", 0xAC)
  , ("ET1", 0xAB)
  , ("EX1", 0xAA)
  , ("ET0", 0xA9)
  , ("EX0", 0xA8)
  ]

-- | Модель времени выполнения по умолчанию
defaultTimingModel :: TimingModel
defaultTimingModel = TimingModel
  { tmClockFrequency = 12000000  -- 12 МГц
  , tmInstructionCycles = Map.fromList
      [ ("MOV", 1)
      , ("ADD", 1)
      , ("SUBB", 1)
      , ("MUL", 4)
      , ("DIV", 4)
      , ("LJMP", 2)
      , ("LCALL", 2)
      , ("RET", 2)
      , ("NOP", 1)
      ]
  , tmMemoryAccessTime = 1
  }

-- Заглушки для функций, которые будут реализованы далее
initializeCodeGenState :: CodeGenerator ()
initializeCodeGenState = return ()

optimizeGeneratedCode :: CodeGenerator ()
optimizeGeneratedCode = return ()

generateDirectives :: [AssemblyDirective]
generateDirectives = []

generateLabels :: [AssemblyLabel]
generateLabels = []

generateFunctionLabel :: Identifier -> CodeGenerator ()
generateFunctionLabel = undefined

generateFunctionPrologue :: [LIRRegister] -> CodeGenerator ()
generateFunctionPrologue = undefined

generateBasicBlock :: LIRBasicBlock -> CodeGenerator ()
generateBasicBlock = undefined

generateFunctionEpilogue :: CodeGenerator ()
generateFunctionEpilogue = undefined

generateMoveInstruction :: LIROperand -> LIROperand -> CodeGenerator ()
generateMoveInstruction = undefined

generateArithmeticInstruction :: Text -> LIROperand -> LIROperand -> LIROperand -> CodeGenerator ()
generateArithmeticInstruction = undefined

generateMultiplyInstruction :: LIROperand -> LIROperand -> LIROperand -> CodeGenerator ()
generateMultiplyInstruction = undefined

generateDivideInstruction :: LIROperand -> LIROperand -> LIROperand -> CodeGenerator ()
generateDivideInstruction = undefined

generateLoadInstruction :: LIROperand -> LIROperand -> CodeGenerator ()
generateLoadInstruction = undefined

generateStoreInstruction :: LIROperand -> LIROperand -> CodeGenerator ()
generateStoreInstruction = undefined

generateBranchInstruction :: Identifier -> CodeGenerator ()
generateBranchInstruction = undefined

generateConditionalBranch :: LIROperand -> Identifier -> CodeGenerator ()
generateConditionalBranch = undefined

generateCallInstruction :: Identifier -> [LIROperand] -> CodeGenerator ()
generateCallInstruction = undefined

generateReturnInstruction :: Maybe LIROperand -> CodeGenerator ()
generateReturnInstruction = undefined

generateLabelInstruction :: Identifier -> CodeGenerator ()
generateLabelInstruction = undefined

generateNopInstruction :: CodeGenerator ()
generateNopInstruction = undefined

generateMemoryOperand :: LIRMemoryRef -> CodeGenerator AssemblyOperand
generateMemoryOperand = undefined

convertRegister :: LIRRegister -> PhysicalRegister
convertRegister = undefined

convertImmediate :: LIRImmediate -> Word16
convertImmediate = undefined

optimizeAssembly :: AssemblyCode -> CodeGenerator AssemblyCode
optimizeAssembly = undefined

validateCode :: AssemblyCode -> CodeGenerator Bool
validateCode = undefined

estimateCodeSize :: AssemblyCode -> CodeGenerator Int
estimateCodeSize = undefined

generateDebugInfo :: AssemblyCode -> CodeGenerator DebugInfo
generateDebugInfo = undefined 