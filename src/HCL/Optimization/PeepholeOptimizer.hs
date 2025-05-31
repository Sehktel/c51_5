{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Автоматизированная система peephole оптимизаций для AT89S4051
--
-- Этот модуль реализует интеллектуальную систему оптимизации "замочной скважины",
-- которая автоматически обнаруживает и применяет паттерны оптимизации в
-- ассемблерном коде для микроконтроллера AT89S4051.
--
-- Ключевые особенности:
-- * Автоматическое обнаружение паттернов оптимизации
-- * Типобезопасные правила трансформации
-- * Метрики эффективности оптимизаций
-- * Валидация корректности трансформаций
-- * Адаптивное обучение на основе результатов
-- * Специализированные оптимизации для 8051
module HCL.Optimization.PeepholeOptimizer
  ( -- * Основные функции
    optimizeAssemblyCode
  , applyPeepholeOptimizations
  , discoverOptimizationPatterns
  , validateOptimizations
  
    -- * Система правил
  , OptimizationRule(..)
  , RulePattern(..)
  , RuleAction(..)
  , RuleCondition(..)
  , RuleMetrics(..)
  
    -- * Автоматическое обнаружение
  , PatternDiscovery(..)
  , PatternMatcher(..)
  , PatternExtractor(..)
  , FrequencyAnalyzer(..)
  
    -- * Оптимизатор
  , PeepholeOptimizer
  , OptimizerConfig(..)
  , OptimizerState(..)
  , OptimizationResult(..)
  
    -- * Специализированные оптимизации AT89S4051
  , AT89S4051Optimizations(..)
  , AccumulatorOptimizations(..)
  , RegisterOptimizations(..)
  , MemoryOptimizations(..)
  , BranchOptimizations(..)
  
    -- * Метрики и анализ
  , OptimizationMetrics(..)
  , PerformanceAnalysis(..)
  , CodeSizeAnalysis(..)
  , CycleCountAnalysis(..)
  
    -- * Утилиты
  , buildOptimizationDatabase
  , exportOptimizationRules
  , importOptimizationRules
  , generateOptimizationReport
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
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Maybe (fromMaybe, catMaybes, isJust, mapMaybe)
import Data.List (foldl', sortBy, nub, partition, minimumBy, maximumBy, groupBy, tails, inits)
import Data.Ord (comparing)
import Data.Function (on)
import Data.Word (Word8, Word16)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import HCL.Types (SourcePos(..), Identifier(..), identifierText)
import HCL.CodeGen (AssemblyCode(..), AssemblyInstruction(..), AssemblyOperand(..), 
                    AssemblyDirective(..), AssemblyLabel(..), LabelType(..))
import HCL.ABI (PhysicalRegister(..), ABIConfig, defaultABIConfig)
import HCL.Error (CompilerError(..), Diagnostic(..), DiagnosticLevel(..))

-- ============================================================================
-- ОСНОВНЫЕ ТИПЫ
-- ============================================================================

-- | Монада peephole оптимизатора
type PeepholeOptimizer = ReaderT OptimizerConfig (StateT OptimizerState (WriterT [Diagnostic] (ExceptT CompilerError IO)))

-- | Конфигурация оптимизатора
data OptimizerConfig = OptimizerConfig
  { ocOptimizationLevel :: !Int                   -- ^ Уровень оптимизации (0-3)
  , ocTargetArch :: !Text                         -- ^ Целевая архитектура
  , ocMaxPatternLength :: !Int                    -- ^ Максимальная длина паттерна
  , ocMinFrequency :: !Int                        -- ^ Минимальная частота для обнаружения
  , ocEnableAdaptiveLearning :: !Bool             -- ^ Включить адаптивное обучение
  , ocSafetyChecks :: !Bool                       -- ^ Включить проверки безопасности
  , ocVerboseLogging :: !Bool                     -- ^ Подробное логирование
  , ocOptimizationDatabase :: !Text               -- ^ Путь к базе оптимизаций
  } deriving (Eq, Show)

-- | Состояние оптимизатора
data OptimizerState = OptimizerState
  { osRules :: ![OptimizationRule]                -- ^ Активные правила оптимизации
  , osPatternFrequency :: !(Map PatternSignature Int) -- ^ Частота паттернов
  , osAppliedOptimizations :: ![AppliedOptimization] -- ^ Применённые оптимизации
  , osMetrics :: !OptimizationMetrics             -- ^ Метрики оптимизации
  , osIterationCount :: !Int                      -- ^ Счётчик итераций
  , osConverged :: !Bool                          -- ^ Сходимость оптимизации
  } deriving (Eq, Show)

-- | Результат оптимизации
data OptimizationResult = OptimizationResult
  { orOriginalCode :: !AssemblyCode               -- ^ Исходный код
  , orOptimizedCode :: !AssemblyCode              -- ^ Оптимизированный код
  , orAppliedRules :: ![AppliedOptimization]      -- ^ Применённые правила
  , orMetrics :: !OptimizationMetrics             -- ^ Метрики оптимизации
  , orDiagnostics :: ![Diagnostic]                -- ^ Диагностические сообщения
  , orSuccess :: !Bool                            -- ^ Успешность оптимизации
  } deriving (Eq, Show)

-- ============================================================================
-- СИСТЕМА ПРАВИЛ ОПТИМИЗАЦИИ
-- ============================================================================

-- | Правило оптимизации
data OptimizationRule = OptimizationRule
  { orId :: !Text                                 -- ^ Уникальный идентификатор
  , orName :: !Text                               -- ^ Название правила
  , orDescription :: !Text                        -- ^ Описание
  , orPattern :: !RulePattern                     -- ^ Паттерн для сопоставления
  , orAction :: !RuleAction                       -- ^ Действие трансформации
  , orConditions :: ![RuleCondition]              -- ^ Условия применения
  , orMetrics :: !RuleMetrics                     -- ^ Метрики правила
  , orPriority :: !Int                            -- ^ Приоритет применения
  , orEnabled :: !Bool                            -- ^ Включено ли правило
  } deriving (Eq, Show)

-- | Паттерн правила
data RulePattern = RulePattern
  { rpInstructions :: ![InstructionPattern]       -- ^ Паттерны инструкций
  , rpLength :: !Int                              -- ^ Длина паттерна
  , rpVariables :: !(Map Text PatternVariable)    -- ^ Переменные паттерна
  , rpConstraints :: ![PatternConstraint]         -- ^ Ограничения паттерна
  } deriving (Eq, Show)

-- | Паттерн инструкции
data InstructionPattern
  = ExactInstruction !Text ![OperandPattern]      -- ^ Точная инструкция
  | WildcardInstruction                           -- ^ Любая инструкция
  | InstructionClass !InstructionClass            -- ^ Класс инструкций
  | ConditionalPattern !Text ![OperandPattern] ![PatternCondition] -- ^ Условный паттерн
  deriving (Eq, Show)

-- | Паттерн операнда
data OperandPattern
  = ExactOperand !AssemblyOperand                 -- ^ Точный операнд
  | VariableOperand !Text                         -- ^ Переменный операнд
  | RegisterClass !RegisterClass                  -- ^ Класс регистров
  | ImmediateRange !Word16 !Word16                -- ^ Диапазон значений
  | AnyOperand                                    -- ^ Любой операнд
  deriving (Eq, Show)

-- | Класс инструкций
data InstructionClass
  = ArithmeticClass                               -- ^ Арифметические инструкции
  | LogicalClass                                  -- ^ Логические инструкции
  | MoveClass                                     -- ^ Инструкции пересылки
  | BranchClass                                   -- ^ Инструкции ветвления
  | MemoryClass                                   -- ^ Инструкции работы с памятью
  deriving (Eq, Show, Enum, Bounded)

-- | Класс регистров
data RegisterClass
  = AccumulatorClass                              -- ^ Аккумулятор
  | WorkingRegisterClass                          -- ^ Рабочие регистры
  , PointerRegisterClass                          -- ^ Указательные регистры
  | SpecialRegisterClass                          -- ^ Специальные регистры
  deriving (Eq, Show, Enum, Bounded)

-- | Переменная паттерна
data PatternVariable = PatternVariable
  { pvName :: !Text                               -- ^ Имя переменной
  , pvType :: !VariableType                       -- ^ Тип переменной
  , pvConstraints :: ![VariableConstraint]        -- ^ Ограничения переменной
  } deriving (Eq, Show)

-- | Тип переменной паттерна
data VariableType
  = RegisterVariable                              -- ^ Регистр
  | ImmediateVariable                             -- ^ Непосредственное значение
  | AddressVariable                               -- ^ Адрес
  | LabelVariable                                 -- ^ Метка
  deriving (Eq, Show, Enum, Bounded)

-- | Ограничение переменной
data VariableConstraint
  = SameAs !Text                                  -- ^ Совпадает с другой переменной
  | DifferentFrom !Text                           -- ^ Отличается от другой переменной
  , InRange !Word16 !Word16                       -- ^ В диапазоне значений
  | NotUsedAfter !Int                             -- ^ Не используется после позиции
  deriving (Eq, Show)

-- | Ограничение паттерна
data PatternConstraint
  = NoSideEffects                                 -- ^ Нет побочных эффектов
  | NoRegisterConflicts                           -- ^ Нет конфликтов регистров
  | PreservesFlags                                -- ^ Сохраняет флаги
  | WithinBasicBlock                              -- ^ В пределах базового блока
  deriving (Eq, Show, Enum, Bounded)

-- | Условие паттерна
data PatternCondition
  = RegisterNotUsed !Text !Int                    -- ^ Регистр не используется
  | FlagNotAffected !Text                         -- ^ Флаг не затрагивается
  | NoJumpTargets !Int !Int                       -- ^ Нет целей переходов
  | CustomCondition !Text                         -- ^ Пользовательское условие
  deriving (Eq, Show)

-- | Действие правила
data RuleAction = RuleAction
  { raType :: !ActionType                         -- ^ Тип действия
  , raReplacement :: ![AssemblyInstruction]       -- ^ Замещающие инструкции
  , raTransformation :: !(Maybe TransformationFunction) -- ^ Функция трансформации
  , raParameters :: !(Map Text Text)              -- ^ Параметры действия
  } deriving (Eq, Show)

-- | Тип действия
data ActionType
  = ReplaceInstructions                           -- ^ Замена инструкций
  | DeleteInstructions                            -- ^ Удаление инструкций
  | ReorderInstructions                           -- ^ Переупорядочивание инструкций
  | TransformInstructions                         -- ^ Трансформация инструкций
  deriving (Eq, Show, Enum, Bounded)

-- | Функция трансформации
newtype TransformationFunction = TransformationFunction
  { runTransformation :: [AssemblyInstruction] -> Map Text AssemblyOperand -> [AssemblyInstruction]
  }

instance Eq TransformationFunction where
  _ == _ = True  -- Упрощение для сравнения

instance Show TransformationFunction where
  show _ = "<TransformationFunction>"

-- | Условие правила
data RuleCondition
  = ArchitectureCondition !Text                   -- ^ Условие архитектуры
  | OptimizationLevelCondition !Int               -- ^ Условие уровня оптимизации
  | FrequencyCondition !Int                       -- ^ Условие частоты
  | SafetyCondition !Text                         -- ^ Условие безопасности
  deriving (Eq, Show)

-- | Метрики правила
data RuleMetrics = RuleMetrics
  { rmCycleSavings :: !Int                        -- ^ Экономия циклов
  , rmSizeSavings :: !Int                         -- ^ Экономия размера
  , rmApplicationCount :: !Int                    -- ^ Количество применений
  , rmSuccessRate :: !Double                      -- ^ Процент успешных применений
  , rmAverageImpact :: !Double                    -- ^ Средний эффект
  } deriving (Eq, Show)

-- ============================================================================
-- АВТОМАТИЧЕСКОЕ ОБНАРУЖЕНИЕ ПАТТЕРНОВ
-- ============================================================================

-- | Система обнаружения паттернов
data PatternDiscovery = PatternDiscovery
  { pdMatcher :: !PatternMatcher                  -- ^ Сопоставитель паттернов
  , pdExtractor :: !PatternExtractor              -- ^ Извлекатель паттернов
  , pdAnalyzer :: !FrequencyAnalyzer              -- ^ Анализатор частоты
  , pdThreshold :: !Double                        -- ^ Порог обнаружения
  } deriving (Eq, Show)

-- | Сопоставитель паттернов
data PatternMatcher = PatternMatcher
  { pmAlgorithm :: !MatchingAlgorithm             -- ^ Алгоритм сопоставления
  , pmMaxDepth :: !Int                            -- ^ Максимальная глубина
  , pmCacheSize :: !Int                           -- ^ Размер кэша
  } deriving (Eq, Show)

-- | Алгоритм сопоставления
data MatchingAlgorithm
  = BruteForceMatching                            -- ^ Полный перебор
  | KMPMatching                                   -- ^ Алгоритм Кнута-Морриса-Пратта
  | SuffixTreeMatching                            -- ^ Суффиксное дерево
  | HashBasedMatching                             -- ^ Хэш-основанное сопоставление
  deriving (Eq, Show, Enum, Bounded)

-- | Извлекатель паттернов
data PatternExtractor = PatternExtractor
  { peMinLength :: !Int                           -- ^ Минимальная длина паттерна
  , peMaxLength :: !Int                           -- ^ Максимальная длина паттерна
  , peOverlapAllowed :: !Bool                     -- ^ Разрешены перекрытия
  , peGeneralizationLevel :: !Int                 -- ^ Уровень обобщения
  } deriving (Eq, Show)

-- | Анализатор частоты
data FrequencyAnalyzer = FrequencyAnalyzer
  { faWindowSize :: !Int                          -- ^ Размер окна анализа
  , faDecayFactor :: !Double                      -- ^ Фактор затухания
  , faUpdateInterval :: !Int                      -- ^ Интервал обновления
  } deriving (Eq, Show)

-- | Сигнатура паттерна
data PatternSignature = PatternSignature
  { psInstructions :: ![Text]                     -- ^ Мнемоники инструкций
  , psOperandTypes :: ![[OperandType]]            -- ^ Типы операндов
  , psLength :: !Int                              -- ^ Длина паттерна
  , psHash :: !Word32                             -- ^ Хэш паттерна
  } deriving (Eq, Show, Ord)

-- | Тип операнда
data OperandType
  = RegisterType                                  -- ^ Регистр
  | ImmediateType                                 -- ^ Непосредственное значение
  | AddressType                                   -- ^ Адрес
  | LabelType                                     -- ^ Метка
  | IndirectType                                  -- ^ Косвенная адресация
  deriving (Eq, Show, Enum, Bounded)

-- ============================================================================
-- СПЕЦИАЛИЗИРОВАННЫЕ ОПТИМИЗАЦИИ AT89S4051
-- ============================================================================

-- | Оптимизации для AT89S4051
data AT89S4051Optimizations = AT89S4051Optimizations
  { aoAccumulator :: !AccumulatorOptimizations    -- ^ Оптимизации аккумулятора
  , aoRegister :: !RegisterOptimizations          -- ^ Оптимизации регистров
  , aoMemory :: !MemoryOptimizations              -- ^ Оптимизации памяти
  , aoBranch :: !BranchOptimizations              -- ^ Оптимизации ветвлений
  } deriving (Eq, Show)

-- | Оптимизации аккумулятора
data AccumulatorOptimizations = AccumulatorOptimizations
  { accRedundantMoves :: !Bool                    -- ^ Удаление избыточных пересылок
  , accChainOptimization :: !Bool                 -- ^ Оптимизация цепочек операций
  , accFlagPreservation :: !Bool                  -- ^ Сохранение флагов
  } deriving (Eq, Show)

-- | Оптимизации регистров
data RegisterOptimizations = RegisterOptimizations
  { regCoalescing :: !Bool                        -- ^ Объединение регистров
  , regReallocation :: !Bool                      -- ^ Перераспределение регистров
  , regSpillReduction :: !Bool                    -- ^ Снижение spill'ов
  } deriving (Eq, Show)

-- | Оптимизации памяти
data MemoryOptimizations = MemoryOptimizations
  { memAccessCoalescing :: !Bool                  -- ^ Объединение доступов к памяти
  , memAddressingModes :: !Bool                   -- ^ Оптимизация режимов адресации
  , memConstantFolding :: !Bool                   -- ^ Свёртка констант
  } deriving (Eq, Show)

-- | Оптимизации ветвлений
data BranchOptimizations = BranchOptimizations
  { branchElimination :: !Bool                    -- ^ Удаление ветвлений
  , branchPrediction :: !Bool                     -- ^ Предсказание ветвлений
  , branchReordering :: !Bool                     -- ^ Переупорядочивание ветвлений
  } deriving (Eq, Show)

-- ============================================================================
-- МЕТРИКИ И АНАЛИЗ
-- ============================================================================

-- | Метрики оптимизации
data OptimizationMetrics = OptimizationMetrics
  { omOriginalSize :: !Int                        -- ^ Исходный размер кода
  , omOptimizedSize :: !Int                       -- ^ Оптимизированный размер
  , omOriginalCycles :: !Int                      -- ^ Исходное количество циклов
  , omOptimizedCycles :: !Int                     -- ^ Оптимизированное количество циклов
  , omRulesApplied :: !Int                        -- ^ Количество применённых правил
  , omOptimizationTime :: !Double                 -- ^ Время оптимизации (мс)
  , omIterations :: !Int                          -- ^ Количество итераций
  } deriving (Eq, Show)

-- | Анализ производительности
data PerformanceAnalysis = PerformanceAnalysis
  { paCycleReduction :: !Double                   -- ^ Снижение циклов (%)
  , paFrequencyImpact :: !(Map Text Double)       -- ^ Влияние на частоту
  , paCriticalPath :: ![AssemblyInstruction]      -- ^ Критический путь
  , paBottlenecks :: ![Text]                      -- ^ Узкие места
  } deriving (Eq, Show)

-- | Анализ размера кода
data CodeSizeAnalysis = CodeSizeAnalysis
  { csaSizeReduction :: !Double                   -- ^ Снижение размера (%)
  , csaInstructionCounts :: !(Map Text Int)       -- ^ Счётчики инструкций
  , csaOptimizationImpact :: !(Map Text Int)      -- ^ Влияние оптимизаций
  } deriving (Eq, Show)

-- | Анализ количества циклов
data CycleCountAnalysis = CycleCountAnalysis
  { ccaOriginalCycles :: !Int                     -- ^ Исходные циклы
  , ccaOptimizedCycles :: !Int                    -- ^ Оптимизированные циклы
  , ccaCycleDistribution :: !(Map Text Int)       -- ^ Распределение циклов
  , ccaWorstCase :: !Int                          -- ^ Худший случай
  , ccaBestCase :: !Int                           -- ^ Лучший случай
  } deriving (Eq, Show)

-- | Применённая оптимизация
data AppliedOptimization = AppliedOptimization
  { aoRule :: !OptimizationRule                   -- ^ Применённое правило
  , aoPosition :: !Int                            -- ^ Позиция применения
  , aoOriginalInstructions :: ![AssemblyInstruction] -- ^ Исходные инструкции
  , aoOptimizedInstructions :: ![AssemblyInstruction] -- ^ Оптимизированные инструкции
  , aoSavings :: !OptimizationSavings             -- ^ Экономия
  } deriving (Eq, Show)

-- | Экономия от оптимизации
data OptimizationSavings = OptimizationSavings
  { osCycles :: !Int                              -- ^ Экономия циклов
  , osBytes :: !Int                               -- ^ Экономия байт
  , osRegisters :: !Int                           -- ^ Освобождённые регистры
  } deriving (Eq, Show)

-- ============================================================================
-- ОСНОВНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Оптимизация ассемблерного кода
optimizeAssemblyCode :: OptimizerConfig -> AssemblyCode -> PeepholeOptimizer OptimizationResult
optimizeAssemblyCode config originalCode = do
  -- Инициализируем состояние оптимизатора
  initializeOptimizer config
  
  -- Загружаем базу правил оптимизации
  loadOptimizationRules
  
  -- Обнаруживаем новые паттерны если включено адаптивное обучение
  when (ocEnableAdaptiveLearning config) $
    discoverNewPatterns originalCode
  
  -- Применяем оптимизации итеративно
  optimizedCode <- iterativeOptimization originalCode
  
  -- Валидируем результат
  validationResult <- validateOptimizations originalCode optimizedCode
  
  -- Собираем метрики
  metrics <- calculateOptimizationMetrics originalCode optimizedCode
  appliedRules <- gets osAppliedOptimizations
  
  return $ OptimizationResult
    { orOriginalCode = originalCode
    , orOptimizedCode = optimizedCode
    , orAppliedRules = appliedRules
    , orMetrics = metrics
    , orDiagnostics = []
    , orSuccess = validationResult
    }

-- | Применение peephole оптимизаций
applyPeepholeOptimizations :: [OptimizationRule] -> AssemblyCode -> PeepholeOptimizer AssemblyCode
applyPeepholeOptimizations rules code = do
  let instructions = acInstructions code
  optimizedInstructions <- applyRulesToInstructions rules instructions
  
  return $ code { acInstructions = optimizedInstructions }

-- | Обнаружение паттернов оптимизации
discoverOptimizationPatterns :: AssemblyCode -> PeepholeOptimizer [OptimizationRule]
discoverOptimizationPatterns code = do
  config <- ask
  
  -- Извлекаем паттерны из кода
  patterns <- extractPatterns (acInstructions code)
  
  -- Анализируем частоту паттернов
  frequencies <- analyzePatternFrequencies patterns
  
  -- Генерируем правила оптимизации
  rules <- generateOptimizationRules frequencies
  
  -- Фильтруем по минимальной частоте
  let filteredRules = filter (\r -> rmApplicationCount (orMetrics r) >= ocMinFrequency config) rules
  
  return filteredRules

-- | Валидация оптимизаций
validateOptimizations :: AssemblyCode -> AssemblyCode -> PeepholeOptimizer Bool
validateOptimizations original optimized = do
  config <- ask
  
  if ocSafetyChecks config
    then do
      -- Проверяем семантическую эквивалентность
      semanticCheck <- checkSemanticEquivalence original optimized
      
      -- Проверяем корректность регистров
      registerCheck <- checkRegisterUsage original optimized
      
      -- Проверяем сохранение флагов
      flagCheck <- checkFlagPreservation original optimized
      
      return $ semanticCheck && registerCheck && flagCheck
    else return True

-- ============================================================================
-- ВСТРОЕННЫЕ ПРАВИЛА ОПТИМИЗАЦИИ AT89S4051
-- ============================================================================

-- | Встроенные правила оптимизации для AT89S4051
builtinAT89S4051Rules :: [OptimizationRule]
builtinAT89S4051Rules =
  [ redundantMoveElimination
  , accumulatorChainOptimization
  , immediateValueOptimization
  , branchOptimization
  , memoryAccessOptimization
  , registerCoalescingRule
  , deadCodeElimination
  , constantFoldingRule
  ]

-- | Удаление избыточных пересылок
redundantMoveElimination :: OptimizationRule
redundantMoveElimination = OptimizationRule
  { orId = "redundant_move_elimination"
  , orName = "Удаление избыточных пересылок"
  , orDescription = "Удаляет последовательные пересылки MOV A,Rn; MOV Rn,A"
  , orPattern = RulePattern
      { rpInstructions = 
          [ ExactInstruction "MOV" [VariableOperand "dst", VariableOperand "src"]
          , ExactInstruction "MOV" [VariableOperand "src", VariableOperand "dst"]
          ]
      , rpLength = 2
      , rpVariables = Map.fromList
          [ ("dst", PatternVariable "dst" RegisterVariable [])
          , ("src", PatternVariable "src" RegisterVariable [DifferentFrom "dst"])
          ]
      , rpConstraints = [NoSideEffects, WithinBasicBlock]
      }
  , orAction = RuleAction
      { raType = DeleteInstructions
      , raReplacement = []
      , raTransformation = Nothing
      , raParameters = Map.empty
      }
  , orConditions = [OptimizationLevelCondition 1]
  , orMetrics = RuleMetrics 2 2 0 0.0 0.0
  , orPriority = 10
  , orEnabled = True
  }

-- | Оптимизация цепочек аккумулятора
accumulatorChainOptimization :: OptimizationRule
accumulatorChainOptimization = OptimizationRule
  { orId = "accumulator_chain_optimization"
  , orName = "Оптимизация цепочек аккумулятора"
  , orDescription = "Оптимизирует последовательности операций с аккумулятором"
  , orPattern = RulePattern
      { rpInstructions =
          [ ExactInstruction "MOV" [ExactOperand (AsmRegister RegA), VariableOperand "src"]
          , InstructionClass ArithmeticClass
          , ExactInstruction "MOV" [VariableOperand "dst", ExactOperand (AsmRegister RegA)]
          ]
      , rpLength = 3
      , rpVariables = Map.fromList
          [ ("src", PatternVariable "src" RegisterVariable [])
          , ("dst", PatternVariable "dst" RegisterVariable [])
          ]
      , rpConstraints = [NoRegisterConflicts]
      }
  , orAction = RuleAction
      { raType = TransformInstructions
      , raReplacement = []
      , raTransformation = Just $ TransformationFunction optimizeAccumulatorChain
      , raParameters = Map.empty
      }
  , orConditions = [OptimizationLevelCondition 2]
  , orMetrics = RuleMetrics 1 0 0 0.0 0.0
  , orPriority = 8
  , orEnabled = True
  }

-- | Оптимизация непосредственных значений
immediateValueOptimization :: OptimizationRule
immediateValueOptimization = OptimizationRule
  { orId = "immediate_value_optimization"
  , orName = "Оптимизация непосредственных значений"
  , orDescription = "Оптимизирует загрузку констант"
  , orPattern = RulePattern
      { rpInstructions =
          [ ExactInstruction "MOV" [VariableOperand "reg", VariableOperand "imm1"]
          , ExactInstruction "MOV" [VariableOperand "reg", VariableOperand "imm2"]
          ]
      , rpLength = 2
      , rpVariables = Map.fromList
          [ ("reg", PatternVariable "reg" RegisterVariable [])
          , ("imm1", PatternVariable "imm1" ImmediateVariable [])
          , ("imm2", PatternVariable "imm2" ImmediateVariable [])
          ]
      , rpConstraints = [WithinBasicBlock]
      }
  , orAction = RuleAction
      { raType = ReplaceInstructions
      , raReplacement = [] -- Будет заполнено динамически
      , raTransformation = Just $ TransformationFunction optimizeImmediateValues
      , raParameters = Map.empty
      }
  , orConditions = [OptimizationLevelCondition 1]
  , orMetrics = RuleMetrics 1 1 0 0.0 0.0
  , orPriority = 7
  , orEnabled = True
  }

-- | Оптимизация ветвлений
branchOptimization :: OptimizationRule
branchOptimization = OptimizationRule
  { orId = "branch_optimization"
  , orName = "Оптимизация ветвлений"
  , orDescription = "Оптимизирует условные и безусловные переходы"
  , orPattern = RulePattern
      { rpInstructions =
          [ InstructionClass BranchClass
          , ExactInstruction "SJMP" [VariableOperand "label"]
          ]
      , rpLength = 2
      , rpVariables = Map.fromList
          [ ("label", PatternVariable "label" LabelVariable [])
          ]
      , rpConstraints = []
      }
  , orAction = RuleAction
      { raType = TransformInstructions
      , raReplacement = []
      , raTransformation = Just $ TransformationFunction optimizeBranches
      , raParameters = Map.empty
      }
  , orConditions = [OptimizationLevelCondition 2]
  , orMetrics = RuleMetrics 2 1 0 0.0 0.0
  , orPriority = 6
  , orEnabled = True
  }

-- | Оптимизация доступа к памяти
memoryAccessOptimization :: OptimizationRule
memoryAccessOptimization = OptimizationRule
  { orId = "memory_access_optimization"
  , orName = "Оптимизация доступа к памяти"
  , orDescription = "Оптимизирует последовательные обращения к памяти"
  , orPattern = RulePattern
      { rpInstructions =
          [ InstructionClass MemoryClass
          , InstructionClass MemoryClass
          ]
      , rpLength = 2
      , rpVariables = Map.empty
      , rpConstraints = [NoSideEffects]
      }
  , orAction = RuleAction
      { raType = TransformInstructions
      , raReplacement = []
      , raTransformation = Just $ TransformationFunction optimizeMemoryAccess
      , raParameters = Map.empty
      }
  , orConditions = [OptimizationLevelCondition 2]
  , orMetrics = RuleMetrics 1 0 0 0.0 0.0
  , orPriority = 5
  , orEnabled = True
  }

-- | Объединение регистров
registerCoalescingRule :: OptimizationRule
registerCoalescingRule = OptimizationRule
  { orId = "register_coalescing"
  , orName = "Объединение регистров"
  , orDescription = "Объединяет операции с одинаковыми регистрами"
  , orPattern = RulePattern
      { rpInstructions =
          [ ExactInstruction "MOV" [VariableOperand "reg1", VariableOperand "reg2"]
          , WildcardInstruction
          , ExactInstruction "MOV" [VariableOperand "reg2", VariableOperand "reg1"]
          ]
      , rpLength = 3
      , rpVariables = Map.fromList
          [ ("reg1", PatternVariable "reg1" RegisterVariable [])
          , ("reg2", PatternVariable "reg2" RegisterVariable [DifferentFrom "reg1"])
          ]
      , rpConstraints = [NoRegisterConflicts]
      }
  , orAction = RuleAction
      { raType = TransformInstructions
      , raReplacement = []
      , raTransformation = Just $ TransformationFunction coalesceRegisters
      , raParameters = Map.empty
      }
  , orConditions = [OptimizationLevelCondition 3]
  , orMetrics = RuleMetrics 2 2 0 0.0 0.0
  , orPriority = 4
  , orEnabled = True
  }

-- | Удаление мёртвого кода
deadCodeElimination :: OptimizationRule
deadCodeElimination = OptimizationRule
  { orId = "dead_code_elimination"
  , orName = "Удаление мёртвого кода"
  , orDescription = "Удаляет неиспользуемые инструкции"
  , orPattern = RulePattern
      { rpInstructions = [WildcardInstruction]
      , rpLength = 1
      , rpVariables = Map.empty
      , rpConstraints = []
      }
  , orAction = RuleAction
      { raType = DeleteInstructions
      , raReplacement = []
      , raTransformation = Just $ TransformationFunction eliminateDeadCode
      , raParameters = Map.empty
      }
  , orConditions = [OptimizationLevelCondition 1]
  , orMetrics = RuleMetrics 1 1 0 0.0 0.0
  , orPriority = 3
  , orEnabled = True
  }

-- | Свёртка констант
constantFoldingRule :: OptimizationRule
constantFoldingRule = OptimizationRule
  { orId = "constant_folding"
  , orName = "Свёртка констант"
  , orDescription = "Вычисляет константные выражения во время компиляции"
  , orPattern = RulePattern
      { rpInstructions =
          [ ExactInstruction "MOV" [ExactOperand (AsmRegister RegA), VariableOperand "imm1"]
          , InstructionClass ArithmeticClass
          ]
      , rpLength = 2
      , rpVariables = Map.fromList
          [ ("imm1", PatternVariable "imm1" ImmediateVariable [])
          ]
      , rpConstraints = []
      }
  , orAction = RuleAction
      { raType = TransformInstructions
      , raReplacement = []
      , raTransformation = Just $ TransformationFunction foldConstants
      , raParameters = Map.empty
      }
  , orConditions = [OptimizationLevelCondition 1]
  , orMetrics = RuleMetrics 1 1 0 0.0 0.0
  , orPriority = 9
  , orEnabled = True
  }

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Конфигурация по умолчанию
defaultOptimizerConfig :: OptimizerConfig
defaultOptimizerConfig = OptimizerConfig
  { ocOptimizationLevel = 2
  , ocTargetArch = "AT89S4051"
  , ocMaxPatternLength = 5
  , ocMinFrequency = 3
  , ocEnableAdaptiveLearning = True
  , ocSafetyChecks = True
  , ocVerboseLogging = False
  , ocOptimizationDatabase = "optimizations.db"
  }

-- Заглушки для функций, которые будут реализованы далее
initializeOptimizer :: OptimizerConfig -> PeepholeOptimizer ()
initializeOptimizer = undefined

loadOptimizationRules :: PeepholeOptimizer ()
loadOptimizationRules = undefined

discoverNewPatterns :: AssemblyCode -> PeepholeOptimizer ()
discoverNewPatterns = undefined

iterativeOptimization :: AssemblyCode -> PeepholeOptimizer AssemblyCode
iterativeOptimization = undefined

calculateOptimizationMetrics :: AssemblyCode -> AssemblyCode -> PeepholeOptimizer OptimizationMetrics
calculateOptimizationMetrics = undefined

applyRulesToInstructions :: [OptimizationRule] -> [AssemblyInstruction] -> PeepholeOptimizer [AssemblyInstruction]
applyRulesToInstructions = undefined

extractPatterns :: [AssemblyInstruction] -> PeepholeOptimizer [PatternSignature]
extractPatterns = undefined

analyzePatternFrequencies :: [PatternSignature] -> PeepholeOptimizer (Map PatternSignature Int)
analyzePatternFrequencies = undefined

generateOptimizationRules :: Map PatternSignature Int -> PeepholeOptimizer [OptimizationRule]
generateOptimizationRules = undefined

checkSemanticEquivalence :: AssemblyCode -> AssemblyCode -> PeepholeOptimizer Bool
checkSemanticEquivalence = undefined

checkRegisterUsage :: AssemblyCode -> AssemblyCode -> PeepholeOptimizer Bool
checkRegisterUsage = undefined

checkFlagPreservation :: AssemblyCode -> AssemblyCode -> PeepholeOptimizer Bool
checkFlagPreservation = undefined

-- Функции трансформации
optimizeAccumulatorChain :: [AssemblyInstruction] -> Map Text AssemblyOperand -> [AssemblyInstruction]
optimizeAccumulatorChain = undefined

optimizeImmediateValues :: [AssemblyInstruction] -> Map Text AssemblyOperand -> [AssemblyInstruction]
optimizeImmediateValues = undefined

optimizeBranches :: [AssemblyInstruction] -> Map Text AssemblyOperand -> [AssemblyInstruction]
optimizeBranches = undefined

optimizeMemoryAccess :: [AssemblyInstruction] -> Map Text AssemblyOperand -> [AssemblyInstruction]
optimizeMemoryAccess = undefined

coalesceRegisters :: [AssemblyInstruction] -> Map Text AssemblyOperand -> [AssemblyInstruction]
coalesceRegisters = undefined

eliminateDeadCode :: [AssemblyInstruction] -> Map Text AssemblyOperand -> [AssemblyInstruction]
eliminateDeadCode = undefined

foldConstants :: [AssemblyInstruction] -> Map Text AssemblyOperand -> [AssemblyInstruction]
foldConstants = undefined

buildOptimizationDatabase :: [AssemblyCode] -> PeepholeOptimizer ()
buildOptimizationDatabase = undefined

exportOptimizationRules :: [OptimizationRule] -> FilePath -> PeepholeOptimizer ()
exportOptimizationRules = undefined

importOptimizationRules :: FilePath -> PeepholeOptimizer [OptimizationRule]
importOptimizationRules = undefined

generateOptimizationReport :: OptimizationResult -> PeepholeOptimizer Text
generateOptimizationReport = undefined 