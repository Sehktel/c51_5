{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

-- | Продвинутое распределение регистров для AT89S4051
--
-- Этот модуль реализует архитектурно-специфичные алгоритмы распределения
-- регистров, учитывающие особенности микроконтроллера AT89S4051:
--
-- * Ограниченный набор регистров (A, B, R0-R7, DPTR)
-- * Специализированные роли регистров (A для арифметики, R0/R1 для указателей)
-- * Асимметричные операции (некоторые работают только с аккумулятором)
-- * Минимизация обращений к памяти из-за ограниченного RAM (128 байт)
--
-- Алгоритмы:
-- * Итеративная раскраска графа с приоритизацией
-- * Архитектурно-осведомлённая эвристика выбора регистров
-- * Интеллектуальное объединение move-инструкций
-- * Оптимизация spill кода с учётом стоимости доступа к памяти
-- * Анализ жизненных циклов переменных с учётом архитектурных ограничений
module HCL.IR.RegisterAllocation
  ( -- * Основные типы
    AdvancedRegisterAllocator
  , AllocationStrategy(..)
  , ArchitecturalConstraints(..)
  , RegisterPriority(..)
  , SpillStrategy(..)
  
    -- * Конфигурация алгоритма
  , AllocatorConfig(..)
  , defaultAllocatorConfig
  , at89s4051Config
  
    -- * Основные функции
  , allocateRegistersAdvanced
  , buildConstrainedInterferenceGraph
  , prioritizedGraphColoring
  , architecturalCoalescing
  , intelligentSpillSelection
  
    -- * Архитектурные эвристики
  , ArchitecturalHeuristics(..)
  , AccumulatorUsagePattern(..)
  , PointerUsagePattern(..)
  , analyzeUsagePatterns
  , selectOptimalRegister
  
    -- * Оптимизации
  , OptimizationPass(..)
  , applyArchitecturalOptimizations
  , optimizeAccumulatorUsage
  , optimizePointerOperations
  , minimizeRegisterPressure
  
    -- * Анализ и статистика
  , AllocationQuality(..)
  , analyzeAllocationQuality
  , estimatePerformanceImpact
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
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Graph (Graph, Vertex, buildG, vertices, edges)
import Data.Array (Array, (!), bounds, listArray, elems)
import Data.Maybe (fromMaybe, catMaybes, isJust, mapMaybe)
import Data.List (foldl', sortBy, nub, partition, minimumBy, maximumBy)
import Data.Ord (comparing)
import Data.Function (on)
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import HCL.Types (SourcePos(..), Identifier(..), Literal(..), identifierText)
import HCL.ABI (Register(..), PhysicalRegister(..), VirtualRegister(..), ABIType(..), 
                ABIConfig, defaultABIConfig, getArgumentRegisters, getReturnRegister,
                isCallerSaved, isCalleeSaved, RegisterClass(..), getRegisterClass)
import HCL.IR.MIR (MIRProgram(..), MIRFunction(..), BasicBlock(..), BlockId(..),
                   TACInstruction(..), TACOperand(..), TACOperation(..), 
                   SSAVariable(..), SSAVersion(..), LivenessInfo(..), CFG(..))

-- ============================================================================
-- ОСНОВНЫЕ ТИПЫ И КОНФИГУРАЦИЯ
-- ============================================================================

-- | Продвинутый распределитель регистров
type AdvancedRegisterAllocator = ReaderT AllocatorConfig (StateT AllocatorState (ExceptT Text IO))

-- | Стратегия распределения регистров
data AllocationStrategy
  = GreedyColoring              -- ^ Жадная раскраска (быстро, но не оптимально)
  | IterativeColoring           -- ^ Итеративная раскраска с приоритизацией
  | ArchitecturalAware          -- ^ Архитектурно-осведомлённая раскраска
  | HybridStrategy              -- ^ Гибридная стратегия (рекомендуется)
  deriving (Eq, Show, Enum, Bounded)

-- | Архитектурные ограничения AT89S4051
data ArchitecturalConstraints = ArchitecturalConstraints
  { acAccumulatorOperations :: !(Set TACOperation)    -- ^ Операции, требующие аккумулятор
  , acPointerOperations :: !(Set TACOperation)        -- ^ Операции с указателями
  , acAsymmetricOperations :: !(Map TACOperation (Set PhysicalRegister)) -- ^ Асимметричные операции
  , acRegisterAliases :: !(Map PhysicalRegister (Set PhysicalRegister))  -- ^ Алиасы регистров
  , acMemoryAccessCost :: !(Map PhysicalRegister Double) -- ^ Стоимость доступа к памяти
  , acRegisterPressureLimit :: !Int                   -- ^ Лимит давления регистров
  } deriving (Eq, Show)

-- | Приоритет регистра для конкретной переменной
data RegisterPriority = RegisterPriority
  { rpRegister :: !PhysicalRegister     -- ^ Регистр
  , rpPriority :: !Double               -- ^ Приоритет (выше = лучше)
  , rpReason :: !Text                   -- ^ Причина приоритета
  , rpConstraints :: ![Text]            -- ^ Ограничения
  } deriving (Eq, Show)

-- | Стратегия вытеснения переменных
data SpillStrategy
  = SpillLeastUsed                      -- ^ Вытеснить наименее используемые
  | SpillFarthestNextUse                -- ^ Вытеснить с самым дальним следующим использованием
  | SpillLowestPriority                 -- ^ Вытеснить с наименьшим приоритетом
  | SpillArchitecturalOptimal           -- ^ Архитектурно-оптимальное вытеснение
  deriving (Eq, Show, Enum, Bounded)

-- | Конфигурация распределителя
data AllocatorConfig = AllocatorConfig
  { acStrategy :: !AllocationStrategy           -- ^ Стратегия распределения
  , acSpillStrategy :: !SpillStrategy           -- ^ Стратегия вытеснения
  , acConstraints :: !ArchitecturalConstraints  -- ^ Архитектурные ограничения
  , acOptimizationPasses :: ![OptimizationPass] -- ^ Проходы оптимизации
  , acMaxIterations :: !Int                     -- ^ Максимум итераций
  , acSpillCostThreshold :: !Double             -- ^ Порог стоимости вытеснения
  , acCoalescingEnabled :: !Bool                -- ^ Включить объединение move
  , acVerboseLogging :: !Bool                   -- ^ Подробное логирование
  } deriving (Eq, Show)

-- | Состояние распределителя
data AllocatorState = AllocatorState
  { asInterferenceGraph :: !EnhancedInterferenceGraph  -- ^ Расширенный граф интерференции
  , asLivenessInfo :: !(Map SSAVariable LivenessInfo)  -- ^ Информация о жизненных циклах
  , asUsagePatterns :: !(Map SSAVariable UsagePattern) -- ^ Паттерны использования
  , asRegisterAssignment :: !(Map SSAVariable PhysicalRegister) -- ^ Назначения регистров
  , asSpilledVariables :: !(Set SSAVariable)           -- ^ Вытесненные переменные
  , asSpillSlots :: !(Map SSAVariable Int)             -- ^ Назначение spill слотов
  , asCoalescedMoves :: !(Map SSAVariable SSAVariable) -- ^ Объединённые move
  , asIterationCount :: !Int                           -- ^ Счётчик итераций
  , asOptimizationLog :: ![Text]                       -- ^ Лог оптимизаций
  } deriving (Eq, Show)

-- ============================================================================
-- РАСШИРЕННЫЙ ГРАФ ИНТЕРФЕРЕНЦИИ
-- ============================================================================

-- | Расширенный граф интерференции с архитектурной информацией
data EnhancedInterferenceGraph = EnhancedInterferenceGraph
  { eigNodes :: !(Map SSAVariable Int)                    -- ^ Отображение переменных в узлы
  , eigGraph :: !Graph                                    -- ^ Граф интерференции
  , eigDegrees :: !(Array Int Int)                        -- ^ Степени узлов
  , eigMoveEdges :: !(Set (Int, Int))                     -- ^ Рёбра move-инструкций
  , eigPrecolored :: !(Map Int PhysicalRegister)          -- ^ Предокрашенные узлы
  , eigArchConstraints :: !(Map Int (Set PhysicalRegister)) -- ^ Архитектурные ограничения
  , eigSpillCosts :: !(Array Int Double)                  -- ^ Стоимости вытеснения
  , eigPriorities :: !(Array Int Double)                  -- ^ Приоритеты узлов
  } deriving (Eq, Show)

-- | Паттерн использования переменной
data UsagePattern = UsagePattern
  { upVariable :: !SSAVariable                  -- ^ Переменная
  , upAccumulatorOps :: !Int                    -- ^ Количество операций с аккумулятором
  , upPointerOps :: !Int                        -- ^ Количество операций с указателями
  , upArithmeticOps :: !Int                     -- ^ Количество арифметических операций
  , upMemoryAccesses :: !Int                    -- ^ Количество обращений к памяти
  , upLoopNesting :: !Int                       -- ^ Уровень вложенности в циклы
  , upFrequency :: !Double                      -- ^ Частота использования
  , upPreferredRegisters :: ![PhysicalRegister] -- ^ Предпочтительные регистры
  } deriving (Eq, Show)

-- ============================================================================
-- АРХИТЕКТУРНЫЕ ЭВРИСТИКИ
-- ============================================================================

-- | Архитектурные эвристики для AT89S4051
data ArchitecturalHeuristics = ArchitecturalHeuristics
  { ahAccumulatorPatterns :: ![AccumulatorUsagePattern]  -- ^ Паттерны использования аккумулятора
  , ahPointerPatterns :: ![PointerUsagePattern]          -- ^ Паттерны использования указателей
  , ahRegisterAffinities :: !(Map TACOperation (Map PhysicalRegister Double)) -- ^ Сродство операций к регистрам
  , ahSpillCostModel :: !(PhysicalRegister -> SSAVariable -> Double) -- ^ Модель стоимости вытеснения
  } deriving Show

-- | Паттерн использования аккумулятора
data AccumulatorUsagePattern
  = ArithmeticChain ![TACOperation]     -- ^ Цепочка арифметических операций
  | ComparisonSequence ![TACOperation]  -- ^ Последовательность сравнений
  | IOOperation !TACOperation          -- ^ Операция ввода-вывода
  | FunctionCall !Identifier           -- ^ Вызов функции
  deriving (Eq, Show)

-- | Паттерн использования указателей
data PointerUsagePattern
  = ArrayAccess !SSAVariable ![SSAVariable]    -- ^ Доступ к массиву
  | StructureAccess !SSAVariable !Int          -- ^ Доступ к структуре
  | IndirectCall !SSAVariable                  -- ^ Косвенный вызов
  | MemoryCopy !SSAVariable !SSAVariable !Int  -- ^ Копирование памяти
  deriving (Eq, Show)

-- ============================================================================
-- ПРОХОДЫ ОПТИМИЗАЦИИ
-- ============================================================================

-- | Проход оптимизации
data OptimizationPass
  = AccumulatorOptimization     -- ^ Оптимизация использования аккумулятора
  | PointerOptimization         -- ^ Оптимизация операций с указателями
  | MoveCoalescing              -- ^ Объединение move-инструкций
  | SpillCodeOptimization       -- ^ Оптимизация spill кода
  | RegisterPressureReduction   -- ^ Снижение давления регистров
  | DeadCodeElimination         -- ^ Удаление мёртвого кода
  deriving (Eq, Show, Enum, Bounded)

-- ============================================================================
-- АНАЛИЗ КАЧЕСТВА РАСПРЕДЕЛЕНИЯ
-- ============================================================================

-- | Качество распределения регистров
data AllocationQuality = AllocationQuality
  { aqRegisterUtilization :: !Double            -- ^ Утилизация регистров (0-1)
  , aqSpillRatio :: !Double                     -- ^ Доля вытесненных переменных
  , aqMoveInstructions :: !Int                  -- ^ Количество move-инструкций
  , aqMemoryAccesses :: !Int                    -- ^ Количество обращений к памяти
  , aqCodeSize :: !Int                          -- ^ Размер кода в байтах
  , aqEstimatedCycles :: !Int                   -- ^ Оценка количества циклов
  , aqArchitecturalEfficiency :: !Double        -- ^ Архитектурная эффективность
  } deriving (Eq, Show)

-- ============================================================================
-- КОНФИГУРАЦИИ ПО УМОЛЧАНИЮ
-- ============================================================================

-- | Конфигурация по умолчанию
defaultAllocatorConfig :: AllocatorConfig
defaultAllocatorConfig = AllocatorConfig
  { acStrategy = HybridStrategy
  , acSpillStrategy = SpillArchitecturalOptimal
  , acConstraints = defaultArchitecturalConstraints
  , acOptimizationPasses = [AccumulatorOptimization, PointerOptimization, MoveCoalescing]
  , acMaxIterations = 10
  , acSpillCostThreshold = 1.5
  , acCoalescingEnabled = True
  , acVerboseLogging = False
  }

-- | Конфигурация для AT89S4051
at89s4051Config :: AllocatorConfig
at89s4051Config = defaultAllocatorConfig
  { acConstraints = at89s4051Constraints
  , acOptimizationPasses = 
      [ AccumulatorOptimization
      , PointerOptimization
      , MoveCoalescing
      , SpillCodeOptimization
      , RegisterPressureReduction
      ]
  , acSpillCostThreshold = 2.0  -- Более агрессивное избегание spill'ов
  }

-- | Архитектурные ограничения по умолчанию
defaultArchitecturalConstraints :: ArchitecturalConstraints
defaultArchitecturalConstraints = ArchitecturalConstraints
  { acAccumulatorOperations = Set.empty
  , acPointerOperations = Set.empty
  , acAsymmetricOperations = Map.empty
  , acRegisterAliases = Map.empty
  , acMemoryAccessCost = Map.empty
  , acRegisterPressureLimit = 8
  }

-- | Архитектурные ограничения AT89S4051
at89s4051Constraints :: ArchitecturalConstraints
at89s4051Constraints = ArchitecturalConstraints
  { acAccumulatorOperations = Set.fromList
      [ TACAdd, TACSubtract, TACMultiply, TACDivide, TACModulo
      , TACBitwiseAnd, TACBitwiseOr, TACBitwiseXor, TACBitwiseNot
      , TACShiftLeft, TACShiftRight
      , TACCompareEQ, TACCompareLT, TACCompareLE
      ]
  , acPointerOperations = Set.fromList
      [ TACLoad, TACStore, TACGetElementPtr
      ]
  , acAsymmetricOperations = Map.fromList
      [ (TACMultiply, Set.fromList [RegA, RegB])  -- Умножение требует A и B
      , (TACDivide, Set.fromList [RegA, RegB])    -- Деление требует A и B
      , (TACLoad, Set.fromList [RegR0, RegR1, RegDPTR]) -- Загрузка через указатели
      , (TACStore, Set.fromList [RegR0, RegR1, RegDPTR]) -- Сохранение через указатели
      ]
  , acRegisterAliases = Map.fromList
      [ (RegDPTR, Set.fromList [RegDPL, RegDPH])  -- DPTR состоит из DPL и DPH
      ]
  , acMemoryAccessCost = Map.fromList
      [ (RegA, 1.0)      -- Быстрый доступ к аккумулятору
      , (RegB, 1.2)      -- Немного медленнее
      , (RegR0, 1.5)     -- Регистры общего назначения
      , (RegR1, 1.5)
      , (RegR2, 1.8)
      , (RegR3, 1.8)
      , (RegR4, 2.0)
      , (RegR5, 2.0)
      , (RegR6, 2.2)
      , (RegR7, 2.2)
      , (RegDPTR, 2.5)   -- Более дорогой доступ к указателю данных
      ]
  , acRegisterPressureLimit = 6  -- Консервативный лимит для AT89S4051
  }

-- ============================================================================
-- ОСНОВНЫЕ ФУНКЦИИ РАСПРЕДЕЛЕНИЯ
-- ============================================================================

-- | Продвинутое распределение регистров
allocateRegistersAdvanced :: MIRFunction -> AdvancedRegisterAllocator (Either Text LIRFunction)
allocateRegistersAdvanced mirFunc = do
  config <- ask
  
  -- Логируем начало процесса
  when (acVerboseLogging config) $
    tell ["Начинаем продвинутое распределение регистров для функции: " <> T.pack (show $ mfName mirFunc)]
  
  -- Строим расширенный граф интерференции
  eig <- buildConstrainedInterferenceGraph mirFunc
  modify $ \s -> s { asInterferenceGraph = eig }
  
  -- Анализируем паттерны использования
  patterns <- analyzeUsagePatterns mirFunc
  modify $ \s -> s { asUsagePatterns = patterns }
  
  -- Выполняем итеративную раскраску с приоритизацией
  result <- prioritizedGraphColoring
  
  case result of
    Left err -> return $ Left err
    Right assignment -> do
      -- Применяем архитектурные оптимизации
      optimizedAssignment <- applyArchitecturalOptimizations assignment
      
      -- Генерируем LIR функцию
      lirFunc <- generateLIRFunction mirFunc optimizedAssignment
      
      -- Анализируем качество распределения
      quality <- analyzeAllocationQuality lirFunc
      
      when (acVerboseLogging config) $ do
        report <- generateOptimizationReport quality
        tell [report]
      
      return $ Right lirFunc

-- | Построение ограниченного графа интерференции
buildConstrainedInterferenceGraph :: MIRFunction -> AdvancedRegisterAllocator EnhancedInterferenceGraph
buildConstrainedInterferenceGraph mirFunc = do
  config <- ask
  
  -- Извлекаем все SSA переменные
  let variables = extractSSAVariables mirFunc
      nodeMap = Map.fromList $ zip variables [0..]
      nodeCount = length variables
  
  -- Строим базовый граф интерференции
  let interferenceEdges = computeInterferenceEdges mirFunc variables
      graph = buildG (0, nodeCount - 1) interferenceEdges
  
  -- Вычисляем степени узлов
  let degrees = listArray (0, nodeCount - 1) 
                [length [() | (u, v) <- interferenceEdges, u == i || v == i] | i <- [0..nodeCount-1]]
  
  -- Находим move-рёбра
  let moveEdges = computeMoveEdges mirFunc nodeMap
  
  -- Применяем архитектурные ограничения
  archConstraints <- computeArchitecturalConstraints mirFunc nodeMap (acConstraints config)
  
  -- Вычисляем стоимости вытеснения
  spillCosts <- computeSpillCosts mirFunc variables (acConstraints config)
  
  -- Вычисляем приоритеты узлов
  priorities <- computeNodePriorities mirFunc variables (acConstraints config)
  
  return $ EnhancedInterferenceGraph
    { eigNodes = nodeMap
    , eigGraph = graph
    , eigDegrees = degrees
    , eigMoveEdges = moveEdges
    , eigPrecolored = Map.empty  -- Пока без предокрашенных узлов
    , eigArchConstraints = archConstraints
    , eigSpillCosts = listArray (0, nodeCount - 1) spillCosts
    , eigPriorities = listArray (0, nodeCount - 1) priorities
    }

-- | Приоритизированная раскраска графа
prioritizedGraphColoring :: AdvancedRegisterAllocator (Either Text (Map SSAVariable PhysicalRegister))
prioritizedGraphColoring = do
  config <- ask
  state <- get
  
  let eig = asInterferenceGraph state
      maxIterations = acMaxIterations config
  
  -- Итеративный процесс раскраски
  result <- iterativeColoring 0 maxIterations eig
  
  case result of
    Left err -> return $ Left err
    Right assignment -> do
      -- Применяем объединение move-инструкций если включено
      finalAssignment <- if acCoalescingEnabled config
        then architecturalCoalescing assignment
        else return assignment
      
      modify $ \s -> s { asRegisterAssignment = finalAssignment }
      return $ Right finalAssignment

-- | Итеративная раскраска
iterativeColoring :: Int -> Int -> EnhancedInterferenceGraph 
                  -> AdvancedRegisterAllocator (Either Text (Map SSAVariable PhysicalRegister))
iterativeColoring iteration maxIter eig
  | iteration >= maxIter = return $ Left "Превышено максимальное количество итераций раскраски"
  | otherwise = do
      config <- ask
      
      when (acVerboseLogging config) $
        tell ["Итерация раскраски " <> T.pack (show iteration)]
      
      -- Пытаемся раскрасить граф
      coloringResult <- attemptColoring eig
      
      case coloringResult of
        ColoringSuccess assignment -> return $ Right assignment
        ColoringSpill spillNodes -> do
          -- Выбираем переменные для вытеснения
          spillVars <- selectSpillVariables spillNodes eig
          
          -- Обновляем граф после вытеснения
          newEig <- updateGraphAfterSpill spillVars eig
          
          -- Продолжаем итерации
          iterativeColoring (iteration + 1) maxIter newEig

-- Заглушки для функций, которые будут реализованы далее
extractSSAVariables :: MIRFunction -> [SSAVariable]
extractSSAVariables = undefined

computeInterferenceEdges :: MIRFunction -> [SSAVariable] -> [(Int, Int)]
computeInterferenceEdges = undefined

computeMoveEdges :: MIRFunction -> Map SSAVariable Int -> Set (Int, Int)
computeMoveEdges = undefined

computeArchitecturalConstraints :: MIRFunction -> Map SSAVariable Int -> ArchitecturalConstraints 
                                -> AdvancedRegisterAllocator (Map Int (Set PhysicalRegister))
computeArchitecturalConstraints = undefined

computeSpillCosts :: MIRFunction -> [SSAVariable] -> ArchitecturalConstraints -> AdvancedRegisterAllocator [Double]
computeSpillCosts = undefined

computeNodePriorities :: MIRFunction -> [SSAVariable] -> ArchitecturalConstraints -> AdvancedRegisterAllocator [Double]
computeNodePriorities = undefined

attemptColoring :: EnhancedInterferenceGraph -> AdvancedRegisterAllocator ColoringResult
attemptColoring = undefined

selectSpillVariables :: [Int] -> EnhancedInterferenceGraph -> AdvancedRegisterAllocator [SSAVariable]
selectSpillVariables = undefined

updateGraphAfterSpill :: [SSAVariable] -> EnhancedInterferenceGraph -> AdvancedRegisterAllocator EnhancedInterferenceGraph
updateGraphAfterSpill = undefined

architecturalCoalescing :: Map SSAVariable PhysicalRegister -> AdvancedRegisterAllocator (Map SSAVariable PhysicalRegister)
architecturalCoalescing = undefined

applyArchitecturalOptimizations :: Map SSAVariable PhysicalRegister -> AdvancedRegisterAllocator (Map SSAVariable PhysicalRegister)
applyArchitecturalOptimizations = undefined

generateLIRFunction :: MIRFunction -> Map SSAVariable PhysicalRegister -> AdvancedRegisterAllocator LIRFunction
generateLIRFunction = undefined

analyzeUsagePatterns :: MIRFunction -> AdvancedRegisterAllocator (Map SSAVariable UsagePattern)
analyzeUsagePatterns = undefined

analyzeAllocationQuality :: LIRFunction -> AdvancedRegisterAllocator AllocationQuality
analyzeAllocationQuality = undefined

generateOptimizationReport :: AllocationQuality -> AdvancedRegisterAllocator Text
generateOptimizationReport = undefined

data ColoringResult
  = ColoringSuccess !(Map SSAVariable PhysicalRegister)
  | ColoringSpill ![Int]
  deriving (Eq, Show)

-- Временные импорты для компиляции
data LIRFunction = LIRFunction deriving (Eq, Show) 