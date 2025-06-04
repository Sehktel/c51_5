{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Модуль низкоуровневого промежуточного представления (LIR)
--
-- LIR представляет код после распределения регистров и подготовки к генерации
-- машинного кода. Основные возможности:
--
-- * Раскраска графа регистров (Graph Coloring Register Allocation)
-- * Распределение виртуальных регистров на физические
-- * Вставка spill кода для переполнения регистров
-- * Линеаризация базовых блоков
-- * Peephole оптимизации
-- * Подготовка к генерации ассемблерного кода
--
-- Архитектурные принципы:
-- * Учёт ограничений целевой архитектуры AT89S4051
-- * Эффективное использование ограниченного набора регистров
-- * Минимизация обращений к памяти
-- * Оптимизация для размера кода
module HCL.IR.LIR
  ( -- * Основные типы LIR
    LIRProgram(..)
  , LIRFunction(..)
  , LIRInstruction(..)
  , LIROperand(..)
  
    -- * Раскраска графа регистров
  , InterferenceGraph(..)
  , RegisterAllocation(..)
  , SpillInfo(..)
  , ColoringResult(..)
  
    -- * Распределение регистров
  , RegisterAllocator
  , AllocatorState(..)
  , allocateRegisters
  , buildInterferenceGraph
  , colorGraph
  , insertSpillCode
  
    -- * Peephole оптимизации
  , PeepholePattern(..)
  , PeepholeRule(..)
  , applyPeepholeOptimizations
  , standardPeepholeRules
  
    -- * Трансформации
  , mirToLIR
  , linearizeBlocks
  , optimizeLIR
  
    -- * Утилиты
  , prettyLIR
  , validateLIR
  , lirStatistics
  , estimateCodeSize
  ) where

import Control.Monad.State.Strict (StateT, runStateT, get, put, modify, gets)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.Writer (WriterT, runWriterT, tell)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IntMap
import Data.Graph (Graph, Vertex, buildG, vertices, edges)
import Data.Array (Array, (!), bounds, listArray)
import Data.Maybe (fromMaybe, catMaybes, isJust)
import Data.List (foldl', sortBy, nub, partition)
import Data.Ord (comparing)

import HCL.Types (SourcePos(..), Identifier(..), Literal(..), identifierText)
import HCL.ABI (Register(..), PhysicalRegister(..), VirtualRegister(..), ABIType(..), 
                ABIConfig, defaultABIConfig, getArgumentRegisters, getReturnRegister,
                isCallerSaved, isCalleeSaved)
import HCL.IR.MIR (MIRProgram(..), MIRFunction(..), BasicBlock(..), BlockId(..),
                   TACInstruction(..), TACOperand(..), TACOperation(..), 
                   SSAVariable(..), SSAVersion(..), LivenessInfo(..))

-- ============================================================================
-- ОСНОВНЫЕ ТИПЫ LIR
-- ============================================================================

-- | Программа в LIR представлении
data LIRProgram = LIRProgram
  { lirFunctions :: ![LIRFunction]      -- ^ Функции программы
  , lirGlobals :: ![LIRGlobal]          -- ^ Глобальные переменные
  , lirConstants :: !(Map Identifier Literal) -- ^ Константы
  , lirRegisterAllocation :: !RegisterAllocation -- ^ Результат распределения регистров
  , lirPos :: !SourcePos                -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Глобальная переменная в LIR
data LIRGlobal = LIRGlobal
  { lgName :: !Identifier               -- ^ Имя переменной
  , lgType :: !ABIType                  -- ^ Тип переменной
  , lgLocation :: !MemoryLocation       -- ^ Расположение в памяти
  , lgInitializer :: !(Maybe Literal)   -- ^ Инициализатор
  , lgPos :: !SourcePos                 -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Расположение в памяти
data MemoryLocation
  = MemoryAddress !Int                  -- ^ Абсолютный адрес
  | StackOffset !Int                    -- ^ Смещение от указателя стека
  | RegisterLocation !PhysicalRegister  -- ^ В регистре
  deriving (Eq, Show)

-- | Функция в LIR представлении
data LIRFunction = LIRFunction
  { lfName :: !Identifier               -- ^ Имя функции
  , lfParameters :: ![LIROperand]       -- ^ Параметры с назначенными регистрами
  , lfReturnType :: !ABIType            -- ^ Тип возвращаемого значения
  , lfInstructions :: ![LIRInstruction] -- ^ Линеаризованные инструкции
  , lfStackSize :: !Int                 -- ^ Размер стекового фрейма
  , lfUsedRegisters :: !(Set PhysicalRegister) -- ^ Используемые регистры
  , lfSpillSlots :: !Int                -- ^ Количество spill слотов
  , lfPos :: !SourcePos                 -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Инструкция LIR с назначенными регистрами
data LIRInstruction = LIRInstruction
  { liOperation :: !TACOperation        -- ^ Операция
  , liResult :: !(Maybe LIROperand)     -- ^ Результат
  , liOperands :: ![LIROperand]         -- ^ Операнды
  , liType :: !ABIType                  -- ^ Тип операции
  , liPos :: !SourcePos                 -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Операнд LIR с назначенными регистрами или памятью
data LIROperand
  = LIRRegister !PhysicalRegister       -- ^ Физический регистр
  | LIRConstant !Literal                -- ^ Константа
  | LIRMemory !MemoryLocation           -- ^ Ячейка памяти
  | LIRLabel !Text                      -- ^ Метка
  deriving (Eq, Show)

-- ============================================================================
-- РАСКРАСКА ГРАФА РЕГИСТРОВ
-- ============================================================================

-- | Граф интерференции переменных
data InterferenceGraph = InterferenceGraph
  { igNodes :: !(Map SSAVariable Int)   -- ^ Отображение переменных в номера узлов
  , igGraph :: !Graph                   -- ^ Граф интерференции
  , igDegrees :: !(Array Int Int)       -- ^ Степени узлов
  , igMoveRelated :: !(Set (Int, Int))  -- ^ Рёбра move-инструкций
  , igPrecolored :: !(Map Int PhysicalRegister) -- ^ Предокрашенные узлы
  } deriving (Eq, Show)

-- | Результат распределения регистров
data RegisterAllocation = RegisterAllocation
  { raAssignment :: !(Map SSAVariable PhysicalRegister) -- ^ Назначение регистров
  , raSpilled :: !(Set SSAVariable)     -- ^ Переменные, вытесненные в память
  , raSpillSlots :: !(Map SSAVariable Int) -- ^ Назначение spill слотов
  , raCoalesced :: !(Map SSAVariable SSAVariable) -- ^ Объединённые переменные
  , raStatistics :: !AllocationStatistics -- ^ Статистика распределения
  } deriving (Eq, Show)

-- | Статистика распределения регистров
data AllocationStatistics = AllocationStatistics
  { asVariablesTotal :: !Int            -- ^ Общее количество переменных
  , asVariablesInRegisters :: !Int      -- ^ Переменных в регистрах
  , asVariablesSpilled :: !Int          -- ^ Переменных вытеснено
  , asSpillInstructions :: !Int         -- ^ Добавлено spill инструкций
  , asCoalescedMoves :: !Int            -- ^ Объединено move инструкций
  } deriving (Eq, Show)

-- | Информация о вытеснении переменной
data SpillInfo = SpillInfo
  { siVariable :: !SSAVariable          -- ^ Вытесняемая переменная
  , siSpillSlot :: !Int                 -- ^ Номер spill слота
  , siSpillCost :: !Double              -- ^ Стоимость вытеснения
  , siLoadInstructions :: ![LIRInstruction] -- ^ Инструкции загрузки
  , siStoreInstructions :: ![LIRInstruction] -- ^ Инструкции сохранения
  } deriving (Eq, Show)

-- | Результат раскраски графа
data ColoringResult
  = ColoringSuccess !(Map Int PhysicalRegister) -- ^ Успешная раскраска
  | ColoringSpill ![Int]                -- ^ Требуется вытеснение узлов
  deriving (Eq, Show)

-- ============================================================================
-- СОСТОЯНИЕ РАСПРЕДЕЛИТЕЛЯ РЕГИСТРОВ
-- ============================================================================

-- | Состояние распределителя регистров
data AllocatorState = AllocatorState
  { asAvailableRegisters :: ![PhysicalRegister] -- ^ Доступные регистры
  , asInterferenceGraph :: !InterferenceGraph    -- ^ Граф интерференции
  , asLivenessInfo :: !(Map BlockId LivenessInfo) -- ^ Информация о живости
  , asCurrentAssignment :: !(Map SSAVariable PhysicalRegister) -- ^ Текущее назначение
  , asSpilledVariables :: !(Set SSAVariable)     -- ^ Вытесненные переменные
  , asSpillSlotCounter :: !Int                   -- ^ Счётчик spill слотов
  , asStatistics :: !AllocationStatistics       -- ^ Статистика
  } deriving (Eq, Show)

-- | Монада распределителя регистров
type RegisterAllocator = StateT AllocatorState IO

-- ============================================================================
-- PEEPHOLE ОПТИМИЗАЦИИ
-- ============================================================================

-- | Шаблон для peephole оптимизации
data PeepholePattern = PeepholePattern
  { ppInstructions :: ![LIRInstruction] -- ^ Шаблон инструкций
  , ppCondition :: ![LIRInstruction] -> Bool -- ^ Дополнительное условие
  , ppReplacement :: ![LIRInstruction] -> [LIRInstruction] -- ^ Замена
  , ppDescription :: !Text              -- ^ Описание оптимизации
  } deriving (Show)

-- | Правило peephole оптимизации
data PeepholeRule = PeepholeRule
  { prName :: !Text                     -- ^ Название правила
  , prPattern :: !PeepholePattern       -- ^ Шаблон
  , prPriority :: !Int                  -- ^ Приоритет применения
  , prEnabled :: !Bool                  -- ^ Включено ли правило
  } deriving (Show)

-- ============================================================================
-- ТРАНСФОРМАЦИЯ MIR → LIR
-- ============================================================================

-- | Преобразует MIR в LIR с распределением регистров
mirToLIR :: ABIConfig -> MIRProgram -> IO LIRProgram
mirToLIR abiConfig mirProgram = do
  -- Распределяем регистры для каждой функции
  (lirFunctions, globalAllocation) <- foldM (allocateFunction abiConfig) ([], emptyAllocation) (mirFunctions mirProgram)
  
  return $ LIRProgram
    { lirFunctions = reverse lirFunctions
    , lirGlobals = []  -- TODO: Трансформация глобальных переменных
    , lirConstants = mirConstants mirProgram
    , lirRegisterAllocation = globalAllocation
    , lirPos = mirPos mirProgram
    }
  where
    emptyAllocation = RegisterAllocation Map.empty Set.empty Map.empty Map.empty emptyStats
    emptyStats = AllocationStatistics 0 0 0 0 0

-- | Распределяет регистры для одной функции
allocateFunction :: ABIConfig -> ([LIRFunction], RegisterAllocation) -> MIRFunction -> IO ([LIRFunction], RegisterAllocation)
allocateFunction abiConfig (accFunctions, globalAlloc) mirFunc = do
  -- Строим граф интерференции
  let interferenceGraph = buildInterferenceGraph mirFunc
  
  -- Выполняем распределение регистров
  (allocation, spillInfo) <- runStateT (allocateRegisters abiConfig mirFunc) (initialAllocatorState abiConfig interferenceGraph)
  
  -- Линеаризуем блоки и вставляем spill код
  linearInstructions <- linearizeBlocks mirFunc allocation spillInfo
  
  -- Создаём LIR функцию
  let lirFunc = LIRFunction
        { lfName = mfName mirFunc
        , lfParameters = []  -- TODO: Трансформация параметров
        , lfReturnType = mfReturnType mirFunc
        , lfInstructions = linearInstructions
        , lfStackSize = calculateStackSize allocation spillInfo
        , lfUsedRegisters = Set.fromList $ Map.elems $ asCurrentAssignment allocation
        , lfSpillSlots = asSpillSlotCounter allocation
        , lfPos = mfPos mirFunc
        }
  
  -- Обновляем глобальное распределение
  let updatedAlloc = mergeAllocations globalAlloc (createAllocationFromState allocation)
  
  return (lirFunc : accFunctions, updatedAlloc)

-- | Начальное состояние распределителя
initialAllocatorState :: ABIConfig -> InterferenceGraph -> AllocatorState
initialAllocatorState abiConfig interferenceGraph = AllocatorState
  { asAvailableRegisters = getAvailableRegisters abiConfig
  , asInterferenceGraph = interferenceGraph
  , asLivenessInfo = Map.empty  -- Будет заполнено позже
  , asCurrentAssignment = Map.empty
  , asSpilledVariables = Set.empty
  , asSpillSlotCounter = 0
  , asStatistics = AllocationStatistics 0 0 0 0 0
  }

-- | Получает доступные регистры для распределения
getAvailableRegisters :: ABIConfig -> [PhysicalRegister]
getAvailableRegisters _abiConfig = 
  -- Для AT89S4051 доступны регистры общего назначения
  [RegR0, RegR1, RegR2, RegR3, RegR4, RegR5, RegR6, RegR7, RegA, RegB]

-- ============================================================================
-- ПОСТРОЕНИЕ ГРАФА ИНТЕРФЕРЕНЦИИ
-- ============================================================================

-- | Строит граф интерференции для функции
buildInterferenceGraph :: MIRFunction -> InterferenceGraph
buildInterferenceGraph mirFunc = 
  let blocks = IntMap.elems $ mfBlocks mirFunc
      variables = collectVariables blocks
      nodeMap = Map.fromList $ zip variables [0..]
      numNodes = length variables
      
      -- Строим рёбра интерференции на основе анализа живости
      interferenceEdges = concatMap (buildBlockInterferences nodeMap) blocks
      
      -- Строим рёбра move-инструкций
      moveEdges = concatMap (buildBlockMoves nodeMap) blocks
      
      graph = buildG (0, numNodes - 1) interferenceEdges
      degrees = listArray (0, numNodes - 1) [length [() | (u, v) <- interferenceEdges, u == i || v == i] | i <- [0..numNodes-1]]
      
  in InterferenceGraph
    { igNodes = nodeMap
    , igGraph = graph
    , igDegrees = degrees
    , igMoveRelated = Set.fromList moveEdges
    , igPrecolored = Map.empty  -- TODO: Предокрашенные узлы для параметров
    }

-- | Собирает все переменные из блоков
collectVariables :: [BasicBlock] -> [SSAVariable]
collectVariables blocks = nub $ concatMap collectFromBlock blocks
  where
    collectFromBlock block = 
      concatMap collectFromInstruction (bbInstructions block) ++
      collectFromInstruction (bbTerminator block)
    
    collectFromInstruction instr = 
      maybe [] (:[]) (tacResult instr) ++
      concatMap collectFromOperand (tacOperands instr)
    
    collectFromOperand = \case
      OpVariable var -> [var]
      _ -> []

-- | Строит рёбра интерференции для блока
buildBlockInterferences :: Map SSAVariable Int -> BasicBlock -> [(Int, Int)]
buildBlockInterferences nodeMap block = 
  let liveVars = Set.toList $ liLiveOut $ bbLiveness block
      nodeIds = catMaybes [Map.lookup var nodeMap | var <- liveVars]
  in [(i, j) | i <- nodeIds, j <- nodeIds, i < j]

-- | Строит рёбра move-инструкций для блока
buildBlockMoves :: Map SSAVariable Int -> BasicBlock -> [(Int, Int)]
buildBlockMoves nodeMap block = 
  catMaybes $ map extractMoveEdge (bbInstructions block)
  where
    extractMoveEdge instr
      | tacOperation instr == TACMove
      , Just result <- tacResult instr
      , [OpVariable source] <- tacOperands instr
      , Just resultId <- Map.lookup result nodeMap
      , Just sourceId <- Map.lookup source nodeMap
      = Just (min resultId sourceId, max resultId sourceId)
      | otherwise = Nothing

-- ============================================================================
-- АЛГОРИТМ РАСПРЕДЕЛЕНИЯ РЕГИСТРОВ
-- ============================================================================

-- | Выполняет распределение регистров
allocateRegisters :: ABIConfig -> MIRFunction -> RegisterAllocator AllocatorState
allocateRegisters abiConfig mirFunc = do
  -- Инициализируем информацию о живости
  let livenessInfo = computeFunctionLiveness mirFunc
  modify $ \s -> s { asLivenessInfo = livenessInfo }
  
  -- Выполняем итеративное распределение
  iterativeAllocation abiConfig
  
  -- Возвращаем финальное состояние
  get

-- | Итеративный алгоритм распределения регистров
iterativeAllocation :: ABIConfig -> RegisterAllocator ()
iterativeAllocation abiConfig = do
  interferenceGraph <- gets asInterferenceGraph
  availableRegs <- gets asAvailableRegisters
  
  -- Пытаемся раскрасить граф
  let coloringResult = colorGraph interferenceGraph availableRegs
  
  case coloringResult of
    ColoringSuccess assignment -> do
      -- Успешная раскраска - обновляем назначение
      updateAssignmentFromColoring assignment
      
    ColoringSpill spillNodes -> do
      -- Требуется вытеснение - выбираем переменные для spill
      spillVariables <- selectSpillVariables spillNodes
      
      -- Обновляем граф и повторяем
      updateGraphAfterSpill spillVariables
      iterativeAllocation abiConfig

-- | Раскрашивает граф интерференции
colorGraph :: InterferenceGraph -> [PhysicalRegister] -> ColoringResult
colorGraph interferenceGraph availableRegs = 
  let numColors = length availableRegs
      graph = igGraph interferenceGraph
      nodeCount = snd (bounds graph) + 1
      
      -- Простой жадный алгоритм раскраски
      coloring = greedyColoring graph numColors
      
  in if all (< numColors) coloring
     then ColoringSuccess $ Map.fromList $ zip [0..] (map (availableRegs !!) coloring)
     else ColoringSpill [i | (i, color) <- zip [0..] coloring, color >= numColors]

-- | Жадный алгоритм раскраски графа
greedyColoring :: Graph -> Int -> [Int]
greedyColoring graph numColors = 
  let nodeCount = snd (bounds graph) + 1
      nodes = [0..nodeCount-1]
      
      -- Сортируем узлы по степени (эвристика largest-first)
      sortedNodes = sortBy (comparing (negate . degree)) nodes
      
      colorNode node usedColors = 
        head $ filter (`notElem` usedColors) [0..numColors-1] ++ [numColors]
      
      assignColor assignment node = 
        let neighbors = [v | (u, v) <- edges graph, u == node] ++ [u | (u, v) <- edges graph, v == node]
            neighborColors = [assignment ! neighbor | neighbor <- neighbors, neighbor < node]
        in colorNode node neighborColors
      
  in [assignColor (listArray (0, nodeCount-1) (repeat 0)) node | node <- sortedNodes]
  where
    degree node = length [() | (u, v) <- edges graph, u == node || v == node]

-- | Выбирает переменные для вытеснения
selectSpillVariables :: [Int] -> RegisterAllocator [SSAVariable]
selectSpillVariables spillNodes = do
  interferenceGraph <- gets asInterferenceGraph
  let nodeMap = igNodes interferenceGraph
      reverseMap = Map.fromList [(v, k) | (k, v) <- Map.toList nodeMap]
      spillVars = catMaybes [Map.lookup node reverseMap | node <- spillNodes]
  
  -- Простая эвристика - выбираем все переменные для spill
  return spillVars

-- | Обновляет граф после вытеснения
updateGraphAfterSpill :: [SSAVariable] -> RegisterAllocator ()
updateGraphAfterSpill spillVars = do
  modify $ \s -> s { asSpilledVariables = Set.union (asSpilledVariables s) (Set.fromList spillVars) }
  
  -- TODO: Перестроить граф интерференции без вытесненных переменных

-- | Обновляет назначение из результата раскраски
updateAssignmentFromColoring :: Map Int PhysicalRegister -> RegisterAllocator ()
updateAssignmentFromColoring coloring = do
  interferenceGraph <- gets asInterferenceGraph
  let nodeMap = igNodes interferenceGraph
      reverseMap = Map.fromList [(v, k) | (k, v) <- Map.toList nodeMap]
      assignment = Map.fromList [(var, reg) | (node, reg) <- Map.toList coloring,
                                              Just var <- [Map.lookup node reverseMap]]
  
  modify $ \s -> s { asCurrentAssignment = assignment }

-- ============================================================================
-- ВСТАВКА SPILL КОДА
-- ============================================================================

-- | Вставляет spill код для вытесненных переменных
insertSpillCode :: [LIRInstruction] -> Set SSAVariable -> RegisterAllocator [LIRInstruction]
insertSpillCode instructions spilledVars = do
  if Set.null spilledVars
    then return instructions
    else do
      -- Назначаем spill слоты
      spillSlots <- assignSpillSlots (Set.toList spilledVars)
      
      -- Вставляем load/store инструкции
      expandedInstructions <- mapM (expandInstruction spillSlots) instructions
      
      return $ concat expandedInstructions

-- | Назначает spill слоты переменным
assignSpillSlots :: [SSAVariable] -> RegisterAllocator (Map SSAVariable Int)
assignSpillSlots spillVars = do
  currentSlot <- gets asSpillSlotCounter
  let slots = Map.fromList $ zip spillVars [currentSlot..]
  modify $ \s -> s { asSpillSlotCounter = currentSlot + length spillVars }
  return slots

-- | Расширяет инструкцию с учётом spill переменных
expandInstruction :: Map SSAVariable Int -> LIRInstruction -> RegisterAllocator [LIRInstruction]
expandInstruction spillSlots instr = do
  -- TODO: Реализовать вставку load/store инструкций
  return [instr]

-- ============================================================================
-- ЛИНЕАРИЗАЦИЯ БЛОКОВ
-- ============================================================================

-- | Линеаризует базовые блоки в последовательность инструкций
linearizeBlocks :: MIRFunction -> AllocatorState -> AllocatorState -> IO [LIRInstruction]
linearizeBlocks mirFunc allocation spillInfo = do
  let blocks = IntMap.elems $ mfBlocks mirFunc
      
  -- Сортируем блоки в порядке выполнения
  let sortedBlocks = sortBlocksByExecution blocks
  
  -- Преобразуем каждый блок в LIR инструкции
  linearInstructions <- mapM (blockToLIR allocation) sortedBlocks
  
  return $ concat linearInstructions

-- | Сортирует блоки в порядке выполнения
sortBlocksByExecution :: [BasicBlock] -> [BasicBlock]
sortBlocksByExecution blocks = 
  -- Простая сортировка по ID блока
  sortBy (comparing bbId) blocks

-- | Преобразует блок в LIR инструкции
blockToLIR :: AllocatorState -> BasicBlock -> IO [LIRInstruction]
blockToLIR allocation block = do
  let tacInstructions = bbInstructions block ++ [bbTerminator block]
  
  -- Преобразуем каждую TAC инструкцию в LIR
  lirInstructions <- mapM (tacToLIR allocation) tacInstructions
  
  return lirInstructions

-- | Преобразует TAC инструкцию в LIR
tacToLIR :: AllocatorState -> TACInstruction -> IO LIRInstruction
tacToLIR allocation tacInstr = do
  let assignment = asCurrentAssignment allocation
      spilledVars = asSpilledVariables allocation
  
  -- Преобразуем операнды
  lirOperands <- mapM (tacOperandToLIR assignment spilledVars) (tacOperands tacInstr)
  lirResult <- mapM (tacResultToLIR assignment spilledVars) (tacResult tacInstr)
  
  return $ LIRInstruction
    { liOperation = tacOperation tacInstr
    , liResult = lirResult
    , liOperands = lirOperands
    , liType = tacType tacInstr
    , liPos = tacPos tacInstr
    }

-- | Преобразует TAC операнд в LIR
tacOperandToLIR :: Map SSAVariable PhysicalRegister -> Set SSAVariable -> TACOperand -> IO LIROperand
tacOperandToLIR assignment spilledVars = \case
  OpVariable var
    | Just reg <- Map.lookup var assignment -> return $ LIRRegister reg
    | Set.member var spilledVars -> return $ LIRMemory (StackOffset 0)  -- TODO: Правильное смещение
    | otherwise -> error $ "Variable not allocated: " ++ show var
  
  OpConstant lit -> return $ LIRConstant lit
  
  OpRegister (Physical reg) -> return $ LIRRegister reg
  OpRegister (Virtual _) -> error "Virtual register in TAC operand"
  
  OpMemory memLoc -> return $ LIRMemory (StackOffset 0)  -- TODO: Преобразование памяти
  
  OpLabel blockId -> return $ LIRLabel (T.pack $ show blockId)

-- | Преобразует TAC результат в LIR
tacResultToLIR :: Map SSAVariable PhysicalRegister -> Set SSAVariable -> SSAVariable -> IO LIROperand
tacResultToLIR assignment spilledVars var
  | Just reg <- Map.lookup var assignment = return $ LIRRegister reg
  | Set.member var spilledVars = return $ LIRMemory (StackOffset 0)  -- TODO: Правильное смещение
  | otherwise = error $ "Variable not allocated: " ++ show var

-- ============================================================================
-- PEEPHOLE ОПТИМИЗАЦИИ
-- ============================================================================

-- | Применяет peephole оптимизации к LIR программе
applyPeepholeOptimizations :: [PeepholeRule] -> LIRProgram -> IO LIRProgram
applyPeepholeOptimizations rules program = do
  optimizedFunctions <- mapM (optimizeFunction rules) (lirFunctions program)
  return $ program { lirFunctions = optimizedFunctions }

-- | Оптимизирует функцию с помощью peephole правил
optimizeFunction :: [PeepholeRule] -> LIRFunction -> IO LIRFunction
optimizeFunction rules func = do
  optimizedInstructions <- applyRulesToInstructions rules (lfInstructions func)
  return $ func { lfInstructions = optimizedInstructions }

-- | Применяет правила к последовательности инструкций
applyRulesToInstructions :: [PeepholeRule] -> [LIRInstruction] -> IO [LIRInstruction]
applyRulesToInstructions rules instructions = do
  -- Применяем правила итеративно до достижения неподвижной точки
  let optimized = iterateOptimizations rules instructions
  return optimized

-- | Итеративно применяет оптимизации
iterateOptimizations :: [PeepholeRule] -> [LIRInstruction] -> [LIRInstruction]
iterateOptimizations rules instructions = 
  let optimized = applyRulesOnce rules instructions
  in if optimized == instructions
     then instructions
     else iterateOptimizations rules optimized

-- | Применяет правила один раз
applyRulesOnce :: [PeepholeRule] -> [LIRInstruction] -> [LIRInstruction]
applyRulesOnce rules instructions = 
  foldl' (applyRule) instructions (sortBy (comparing prPriority) rules)

-- | Применяет одно правило
applyRule :: [LIRInstruction] -> PeepholeRule -> [LIRInstruction]
applyRule instructions rule
  | not (prEnabled rule) = instructions
  | otherwise = applyPatternToInstructions (prPattern rule) instructions

-- | Применяет шаблон к инструкциям
applyPatternToInstructions :: PeepholePattern -> [LIRInstruction] -> [LIRInstruction]
applyPatternToInstructions pattern instructions = 
  case instructions of
    [] -> []
    (instr:rest) -> 
      let patternLength = length (ppInstructions pattern)
          window = take patternLength instructions
      in if length window == patternLength && 
            matchesPattern pattern window && 
            ppCondition pattern window
         then ppReplacement pattern window ++ applyPatternToInstructions pattern (drop patternLength instructions)
         else instr : applyPatternToInstructions pattern rest

-- | Проверяет соответствие шаблону
matchesPattern :: PeepholePattern -> [LIRInstruction] -> Bool
matchesPattern pattern instructions = 
  length instructions == length (ppInstructions pattern) &&
  all matchInstruction (zip (ppInstructions pattern) instructions)
  where
    matchInstruction (patternInstr, actualInstr) = 
      liOperation patternInstr == liOperation actualInstr
      -- TODO: Более сложное сопоставление операндов

-- | Стандартные peephole правила для AT89S4051
standardPeepholeRules :: [PeepholeRule]
standardPeepholeRules = 
  [ -- Удаление избыточных move инструкций
    PeepholeRule
      { prName = "redundant-move"
      , prPattern = PeepholePattern
          { ppInstructions = [movePattern, movePattern]
          , ppCondition = \instrs -> case instrs of
              [instr1, instr2] -> 
                liResult instr1 == head (liOperands instr2) &&
                head (liOperands instr1) == liResult instr2
              _ -> False
          , ppReplacement = const []
          , ppDescription = "Remove redundant move instructions"
          }
      , prPriority = 1
      , prEnabled = True
      }
  
  -- Объединение арифметических операций
  , PeepholeRule
      { prName = "combine-arithmetic"
      , prPattern = PeepholePattern
          { ppInstructions = [addPattern, addPattern]
          , ppCondition = \instrs -> case instrs of
              [instr1, instr2] -> 
                liResult instr1 == head (liOperands instr2) &&
                isConstantOperand (head $ tail $ liOperands instr1) &&
                isConstantOperand (head $ tail $ liOperands instr2)
              _ -> False
          , ppReplacement = combineAdditions
          , ppDescription = "Combine consecutive additions with constants"
          }
      , prPriority = 2
      , prEnabled = True
      }
  ]
  where
    movePattern = LIRInstruction TACMove Nothing [] UnsignedByte (SourcePos "" 0 0)
    addPattern = LIRInstruction TACAdd Nothing [] UnsignedByte (SourcePos "" 0 0)
    
    isConstantOperand = \case
      LIRConstant _ -> True
      _ -> False
    
    combineAdditions instrs = instrs  -- TODO: Реализовать объединение

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Вычисляет информацию о живости для функции
computeFunctionLiveness :: MIRFunction -> Map BlockId LivenessInfo
computeFunctionLiveness mirFunc = 
  -- TODO: Реализовать анализ живости
  Map.empty

-- | Вычисляет размер стекового фрейма
calculateStackSize :: AllocatorState -> AllocatorState -> Int
calculateStackSize allocation spillInfo = 
  let spillSlots = asSpillSlotCounter allocation
      localVariables = 0  -- TODO: Подсчёт локальных переменных
  in spillSlots * 2 + localVariables  -- 2 байта на слот для 16-битных значений

-- | Создаёт RegisterAllocation из состояния распределителя
createAllocationFromState :: AllocatorState -> RegisterAllocation
createAllocationFromState state = RegisterAllocation
  { raAssignment = asCurrentAssignment state
  , raSpilled = asSpilledVariables state
  , raSpillSlots = Map.empty  -- TODO: Заполнить spill слоты
  , raCoalesced = Map.empty   -- TODO: Заполнить объединённые переменные
  , raStatistics = asStatistics state
  }

-- | Объединяет два результата распределения
mergeAllocations :: RegisterAllocation -> RegisterAllocation -> RegisterAllocation
mergeAllocations alloc1 alloc2 = RegisterAllocation
  { raAssignment = Map.union (raAssignment alloc1) (raAssignment alloc2)
  , raSpilled = Set.union (raSpilled alloc1) (raSpilled alloc2)
  , raSpillSlots = Map.union (raSpillSlots alloc1) (raSpillSlots alloc2)
  , raCoalesced = Map.union (raCoalesced alloc1) (raCoalesced alloc2)
  , raStatistics = mergeStatistics (raStatistics alloc1) (raStatistics alloc2)
  }

-- | Объединяет статистику распределения
mergeStatistics :: AllocationStatistics -> AllocationStatistics -> AllocationStatistics
mergeStatistics stats1 stats2 = AllocationStatistics
  { asVariablesTotal = asVariablesTotal stats1 + asVariablesTotal stats2
  , asVariablesInRegisters = asVariablesInRegisters stats1 + asVariablesInRegisters stats2
  , asVariablesSpilled = asVariablesSpilled stats1 + asVariablesSpilled stats2
  , asSpillInstructions = asSpillInstructions stats1 + asSpillInstructions stats2
  , asCoalescedMoves = asCoalescedMoves stats1 + asCoalescedMoves stats2
  }

-- ============================================================================
-- ОПТИМИЗАЦИЯ LIR
-- ============================================================================

-- | Оптимизирует LIR программу
optimizeLIR :: [PeepholeRule] -> LIRProgram -> IO LIRProgram
optimizeLIR peepholeRules program = do
  -- Применяем peephole оптимизации
  optimizedProgram <- applyPeepholeOptimizations peepholeRules program
  
  -- TODO: Другие оптимизации (удаление мёртвого кода, и т.д.)
  
  return optimizedProgram

-- ============================================================================
-- УТИЛИТЫ
-- ============================================================================

-- | Красивое отображение LIR
prettyLIR :: LIRProgram -> Text
prettyLIR program = T.unlines $
  map prettyLIRFunction (lirFunctions program)
  where
    prettyLIRFunction func = T.unlines
      [ "function " <> identifierText (lfName func) <> ":"
      , "  stack size: " <> T.pack (show $ lfStackSize func)
      , "  used registers: " <> T.pack (show $ Set.toList $ lfUsedRegisters func)
      , "  spill slots: " <> T.pack (show $ lfSpillSlots func)
      , T.unlines [prettyLIRInstruction instr | instr <- lfInstructions func]
      ]
    
    prettyLIRInstruction instr = 
      "    " <> maybe "" (\r -> prettyLIROperand r <> " = ") (liResult instr) <>
      T.pack (show $ liOperation instr) <> " " <>
      T.intercalate ", " (map prettyLIROperand $ liOperands instr)
    
    prettyLIROperand = \case
      LIRRegister reg -> T.pack (show reg)
      LIRConstant lit -> T.pack (show lit)
      LIRMemory (MemoryAddress addr) -> "[" <> T.pack (show addr) <> "]"
      LIRMemory (StackOffset offset) -> "[SP+" <> T.pack (show offset) <> "]"
      LIRMemory (RegisterLocation reg) -> T.pack (show reg)
      LIRLabel label -> label

-- | Валидирует LIR программу
validateLIR :: LIRProgram -> [Text]
validateLIR _program = []  -- TODO: Реализовать валидацию

-- | Статистика LIR программы
lirStatistics :: LIRProgram -> Text
lirStatistics program = T.unlines
  [ "LIR Statistics:"
  , "Functions: " <> T.pack (show $ length $ lirFunctions program)
  , "Total instructions: " <> T.pack (show $ sum [length (lfInstructions f) | f <- lirFunctions program])
  , "Total stack size: " <> T.pack (show $ sum [lfStackSize f | f <- lirFunctions program])
  , "Total spill slots: " <> T.pack (show $ sum [lfSpillSlots f | f <- lirFunctions program])
  ]

-- | Оценивает размер генерируемого кода
estimateCodeSize :: LIRProgram -> Int
estimateCodeSize program = 
  sum [estimateFunctionSize func | func <- lirFunctions program]
  where
    estimateFunctionSize func = 
      sum [estimateInstructionSize instr | instr <- lfInstructions func]
    
    estimateInstructionSize instr = 
      case liOperation instr of
        TACMove -> 2      -- MOV instruction
        TACAdd -> 1       -- ADD instruction
        TACSubtract -> 1  -- SUB instruction
        TACJump -> 3      -- JMP instruction
        TACCall -> 3      -- CALL instruction
        TACReturn -> 1    -- RET instruction
        _ -> 2            -- Average instruction size

-- Импорты для недостающих типов
import HCL.IR.MIR (SSAVariable(..)) 