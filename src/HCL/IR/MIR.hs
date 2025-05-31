{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Модуль среднеуровневого промежуточного представления (MIR)
--
-- MIR реализует Three Address Code (TAC) в Static Single Assignment (SSA) форме
-- с Control Flow Graph (CFG) для эффективных оптимизаций:
--
-- * TAC: Каждая инструкция имеет максимум 3 операнда
-- * SSA: Каждая переменная присваивается только один раз
-- * CFG: Граф потока управления с базовыми блоками
-- * Phi-функции: Для слияния значений в точках соединения
-- * Оптимизации: Распространение констант, удаление мертвого кода
--
-- Архитектурные принципы:
-- * Функциональная неизменяемость структур данных
-- * Типобезопасные трансформации
-- * Эффективные алгоритмы анализа потоков данных
module HCL.IR.MIR
  ( -- * Основные типы MIR
    MIRProgram(..)
  , MIRFunction(..)
  , BasicBlock(..)
  , BlockId(..)
  
    -- * Three Address Code
  , TACInstruction(..)
  , TACOperand(..)
  , TACOperation(..)
  
    -- * SSA форма
  , SSAVariable(..)
  , SSAVersion(..)
  , PhiFunction(..)
  
    -- * Control Flow Graph
  , CFG(..)
  , CFGNode(..)
  , CFGEdge(..)
  , DominatorTree(..)
  
    -- * Оптимизации
  , OptimizationPass(..)
  , DataFlowAnalysis(..)
  , LivenessInfo(..)
  , ReachingDefinitions(..)
  
    -- * Трансформации
  , hirToMIR
  , optimizeMIR
  , constructCFG
  , buildDominatorTree
  , insertPhiFunctions
  
    -- * Анализ потоков данных
  , computeLiveness
  , computeReachingDefinitions
  , constantPropagation
  , deadCodeElimination
  
    -- * Утилиты
  , prettyMIR
  , validateMIR
  , mirStatistics
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
import Data.Graph (Graph, Vertex, buildG, topSort, reachable)
import Data.Tree (Tree, Forest)
import Data.Maybe (fromMaybe, catMaybes)
import Data.List (foldl', nub)

import HCL.Types (SourcePos(..), Identifier(..), Literal(..), identifierText)
import HCL.ABI (Register(..), PhysicalRegister(..), VirtualRegister(..), ABIType(..), ABIConfig)
import HCL.IR.HIR (HIRProgram(..), HIRFunction(..), HIRExpr(..), HIRStmt(..), HIRType(..))

-- ============================================================================
-- ОСНОВНЫЕ ТИПЫ MIR
-- ============================================================================

-- | Программа в MIR представлении
data MIRProgram = MIRProgram
  { mirFunctions :: ![MIRFunction]      -- ^ Функции программы
  , mirGlobals :: ![MIRGlobal]          -- ^ Глобальные переменные
  , mirConstants :: !(Map Identifier Literal) -- ^ Константы
  , mirPos :: !SourcePos                -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Глобальная переменная в MIR
data MIRGlobal = MIRGlobal
  { mgName :: !Identifier               -- ^ Имя переменной
  , mgType :: !ABIType                  -- ^ Тип переменной
  , mgInitializer :: !(Maybe Literal)   -- ^ Инициализатор
  , mgPos :: !SourcePos                 -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Функция в MIR представлении
data MIRFunction = MIRFunction
  { mfName :: !Identifier               -- ^ Имя функции
  , mfParameters :: ![SSAVariable]      -- ^ Параметры в SSA форме
  , mfReturnType :: !ABIType            -- ^ Тип возвращаемого значения
  , mfBlocks :: !(IntMap BasicBlock)    -- ^ Базовые блоки
  , mfCFG :: !CFG                       -- ^ Граф потока управления
  , mfDominatorTree :: !DominatorTree   -- ^ Дерево доминаторов
  , mfEntry :: !BlockId                 -- ^ Входной блок
  , mfExit :: !BlockId                  -- ^ Выходной блок
  , mfPos :: !SourcePos                 -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Идентификатор базового блока
newtype BlockId = BlockId Int
  deriving (Eq, Ord, Show, Enum)

-- | Базовый блок в SSA форме
data BasicBlock = BasicBlock
  { bbId :: !BlockId                    -- ^ Идентификатор блока
  , bbLabel :: !(Maybe Identifier)      -- ^ Метка блока
  , bbPhiFunctions :: ![PhiFunction]    -- ^ Phi-функции в начале блока
  , bbInstructions :: ![TACInstruction] -- ^ TAC инструкции
  , bbTerminator :: !TACInstruction     -- ^ Терминатор блока
  , bbPredecessors :: ![BlockId]        -- ^ Предшественники
  , bbSuccessors :: ![BlockId]          -- ^ Преемники
  , bbLiveness :: !LivenessInfo         -- ^ Информация о живости переменных
  , bbPos :: !SourcePos                 -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- ============================================================================
-- THREE ADDRESS CODE
-- ============================================================================

-- | Инструкция Three Address Code
data TACInstruction = TACInstruction
  { tacOperation :: !TACOperation       -- ^ Операция
  , tacResult :: !(Maybe SSAVariable)   -- ^ Результат (может отсутствовать)
  , tacOperands :: ![TACOperand]        -- ^ Операнды (максимум 3)
  , tacType :: !ABIType                 -- ^ Тип операции
  , tacPos :: !SourcePos                -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Операнд TAC инструкции
data TACOperand
  = OpVariable !SSAVariable             -- ^ Переменная в SSA форме
  | OpConstant !Literal                 -- ^ Константа
  | OpRegister !Register                -- ^ Регистр (физический или виртуальный)
  | OpMemory !MemoryLocation            -- ^ Ячейка памяти
  | OpLabel !BlockId                    -- ^ Метка блока
  deriving (Eq, Show)

-- | Расположение в памяти
data MemoryLocation = MemoryLocation
  { mlBase :: !TACOperand               -- ^ Базовый адрес
  , mlOffset :: !Int                    -- ^ Смещение
  , mlType :: !ABIType                  -- ^ Тип данных
  } deriving (Eq, Show)

-- | Операции Three Address Code
data TACOperation
  -- Арифметические операции
  = TACAdd | TACSubtract | TACMultiply | TACDivide | TACModulo
  
  -- Битовые операции
  | TACBitwiseAnd | TACBitwiseOr | TACBitwiseXor | TACBitwiseNot
  | TACShiftLeft | TACShiftRight
  
  -- Логические операции
  | TACLogicalAnd | TACLogicalOr | TACLogicalNot
  
  -- Операции сравнения
  | TACEqual | TACNotEqual | TACLessThan | TACLessEqual
  | TACGreaterThan | TACGreaterEqual
  
  -- Операции присваивания и перемещения
  | TACMove | TACLoad | TACStore
  
  -- Операции с памятью
  | TACAlloca | TACMemCopy | TACMemSet
  
  -- Операции управления потоком
  | TACJump | TACBranch | TACCall | TACReturn
  
  -- Операции приведения типов
  | TACCast | TACTruncate | TACExtend
  
  -- Специальные операции для 8051
  | TACBitSet | TACBitClear | TACBitTest
  | TACInterruptEnable | TACInterruptDisable
  
  deriving (Eq, Show, Enum, Bounded)

-- ============================================================================
-- SSA ФОРМА
-- ============================================================================

-- | Переменная в SSA форме
data SSAVariable = SSAVariable
  { ssaName :: !Identifier              -- ^ Базовое имя переменной
  , ssaVersion :: !SSAVersion           -- ^ Версия в SSA форме
  , ssaType :: !ABIType                 -- ^ Тип переменной
  } deriving (Eq, Ord, Show)

-- | Версия переменной в SSA
newtype SSAVersion = SSAVersion Int
  deriving (Eq, Ord, Show, Enum)

-- | Phi-функция для слияния значений
data PhiFunction = PhiFunction
  { phiResult :: !SSAVariable           -- ^ Результирующая переменная
  , phiInputs :: ![(SSAVariable, BlockId)] -- ^ Входные переменные и блоки
  , phiPos :: !SourcePos                -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- ============================================================================
-- CONTROL FLOW GRAPH
-- ============================================================================

-- | Граф потока управления
data CFG = CFG
  { cfgNodes :: !(IntMap CFGNode)       -- ^ Узлы графа
  , cfgEdges :: ![CFGEdge]              -- ^ Рёбра графа
  , cfgEntry :: !BlockId                -- ^ Входной узел
  , cfgExit :: !BlockId                 -- ^ Выходной узел
  } deriving (Eq, Show)

-- | Узел CFG
data CFGNode = CFGNode
  { cfgNodeId :: !BlockId               -- ^ Идентификатор узла
  , cfgNodeBlock :: !BasicBlock         -- ^ Соответствующий базовый блок
  , cfgNodePredecessors :: ![BlockId]   -- ^ Предшественники
  , cfgNodeSuccessors :: ![BlockId]     -- ^ Преемники
  } deriving (Eq, Show)

-- | Рёбро CFG
data CFGEdge = CFGEdge
  { cfgEdgeFrom :: !BlockId             -- ^ Исходный блок
  , cfgEdgeTo :: !BlockId               -- ^ Целевой блок
  , cfgEdgeCondition :: !(Maybe TACOperand) -- ^ Условие перехода
  } deriving (Eq, Show)

-- | Дерево доминаторов
data DominatorTree = DominatorTree
  { dtRoot :: !BlockId                  -- ^ Корень дерева
  , dtChildren :: !(Map BlockId [BlockId]) -- ^ Дети каждого узла
  , dtParent :: !(Map BlockId BlockId)  -- ^ Родитель каждого узла
  , dtDominators :: !(Map BlockId (Set BlockId)) -- ^ Доминаторы каждого узла
  } deriving (Eq, Show)

-- ============================================================================
-- АНАЛИЗ ПОТОКОВ ДАННЫХ
-- ============================================================================

-- | Информация о живости переменных
data LivenessInfo = LivenessInfo
  { liLiveIn :: !(Set SSAVariable)      -- ^ Живые на входе в блок
  , liLiveOut :: !(Set SSAVariable)     -- ^ Живые на выходе из блока
  , liDefined :: !(Set SSAVariable)     -- ^ Определённые в блоке
  , liUsed :: !(Set SSAVariable)        -- ^ Используемые в блоке
  } deriving (Eq, Show)

-- | Достигающие определения
data ReachingDefinitions = ReachingDefinitions
  { rdGenerated :: !(Set (SSAVariable, BlockId)) -- ^ Генерируемые определения
  , rdKilled :: !(Set (SSAVariable, BlockId))    -- ^ Убиваемые определения
  , rdReachingIn :: !(Set (SSAVariable, BlockId)) -- ^ Достигающие на входе
  , rdReachingOut :: !(Set (SSAVariable, BlockId)) -- ^ Достигающие на выходе
  } deriving (Eq, Show)

-- | Анализ потоков данных
data DataFlowAnalysis = DataFlowAnalysis
  { dfaLiveness :: !(Map BlockId LivenessInfo)           -- ^ Анализ живости
  , dfaReachingDefs :: !(Map BlockId ReachingDefinitions) -- ^ Достигающие определения
  , dfaConstants :: !(Map SSAVariable Literal)           -- ^ Константные значения
  , dfaDeadCode :: !(Set BlockId)                        -- ^ Мёртвые блоки
  } deriving (Eq, Show)

-- ============================================================================
-- ОПТИМИЗАЦИИ
-- ============================================================================

-- | Проход оптимизации
data OptimizationPass
  = ConstantPropagation     -- ^ Распространение констант
  | DeadCodeElimination     -- ^ Удаление мёртвого кода
  | CommonSubexpressionElimination -- ^ Удаление общих подвыражений
  | LoopInvariantMotion     -- ^ Вынос инвариантов циклов
  | RegisterPromotion       -- ^ Продвижение в регистры
  | PeepholeOptimization    -- ^ Peephole оптимизации
  deriving (Eq, Show, Enum, Bounded)

-- ============================================================================
-- СОСТОЯНИЕ ТРАНСФОРМАЦИИ
-- ============================================================================

-- | Состояние трансформации HIR → MIR
data MIRState = MIRState
  { msNextBlockId :: !Int               -- ^ Следующий ID блока
  , msNextSSAVersion :: !(Map Identifier Int) -- ^ Следующие версии SSA
  , msCurrentBlock :: !BlockId          -- ^ Текущий блок
  , msBlocks :: !(IntMap BasicBlock)    -- ^ Построенные блоки
  , msVariableMap :: !(Map Identifier SSAVariable) -- ^ Отображение переменных
  } deriving (Eq, Show)

-- | Монада трансформации MIR
type MIRTransform = StateT MIRState IO

-- ============================================================================
-- ТРАНСФОРМАЦИЯ HIR → MIR
-- ============================================================================

-- | Преобразует HIR в MIR
hirToMIR :: HIRProgram -> IO MIRProgram
hirToMIR hirProgram = do
  (mirFunctions, _) <- runStateT (mapM transformFunction (hirFunctions hirProgram)) initialMIRState
  return $ MIRProgram
    { mirFunctions = mirFunctions
    , mirGlobals = []  -- TODO: Трансформация глобальных переменных
    , mirConstants = Map.empty
    , mirPos = hirPos hirProgram
    }
  where
    initialMIRState = MIRState
      { msNextBlockId = 0
      , msNextSSAVersion = Map.empty
      , msCurrentBlock = BlockId 0
      , msBlocks = IntMap.empty
      , msVariableMap = Map.empty
      }

-- | Трансформирует функцию HIR в MIR
transformFunction :: HIRFunction -> MIRTransform MIRFunction
transformFunction hirFunc = do
  -- Создаём входной и выходной блоки
  entryId <- newBlockId
  exitId <- newBlockId
  
  -- Трансформируем параметры в SSA переменные
  ssaParams <- mapM transformParameter (hirFuncParams hirFunc)
  
  -- Трансформируем тело функции
  modify $ \s -> s { msCurrentBlock = entryId }
  mapM_ transformStatement (hirFuncBody hirFunc)
  
  -- Получаем построенные блоки
  blocks <- gets msBlocks
  
  -- Строим CFG и дерево доминаторов
  let cfg = constructCFG blocks entryId exitId
      domTree = buildDominatorTree cfg
  
  return $ MIRFunction
    { mfName = hirFuncName hirFunc
    , mfParameters = ssaParams
    , mfReturnType = abiTypeFromHIR (hirFuncType hirFunc)
    , mfBlocks = blocks
    , mfCFG = cfg
    , mfDominatorTree = domTree
    , mfEntry = entryId
    , mfExit = exitId
    , mfPos = hirFuncPos hirFunc
    }

-- | Трансформирует параметр функции
transformParameter :: HIRDeclaration -> MIRTransform SSAVariable
transformParameter hirDecl = do
  let name = hirDeclName hirDecl
      abiType = abiTypeFromHIR (hirDeclType hirDecl)
  newSSAVariable name abiType

-- | Трансформирует оператор HIR в TAC инструкции
transformStatement :: HIRStmt -> MIRTransform ()
transformStatement (HIRStmt stmtNode pos) = case stmtNode of
  HIRExprStmt expr -> do
    _ <- transformExpression expr
    return ()
  
  HIRReturn mexpr -> do
    case mexpr of
      Just expr -> do
        resultVar <- transformExpression expr
        emitInstruction $ TACInstruction TACReturn Nothing [OpVariable resultVar] (ssaType resultVar) pos
      Nothing -> 
        emitInstruction $ TACInstruction TACReturn Nothing [] UnsignedByte pos
  
  _ -> error "TODO: Implement other statement transformations"

-- | Трансформирует выражение HIR в SSA переменную
transformExpression :: HIRExpr -> MIRTransform SSAVariable
transformExpression (HIRExpr exprNode exprType pos) = case exprNode of
  HIRLiteral lit -> do
    tempVar <- newTempVariable (abiTypeFromHIR exprType)
    emitInstruction $ TACInstruction TACMove (Just tempVar) [OpConstant lit] (abiTypeFromHIR exprType) pos
    return tempVar
  
  HIRVariable symbol -> do
    -- Получаем SSA переменную для символа
    let name = symName symbol
    maybeVar <- gets (Map.lookup name . msVariableMap)
    case maybeVar of
      Just var -> return var
      Nothing -> newSSAVariable name (abiTypeFromHIR exprType)
  
  HIRBinary op left right -> do
    leftVar <- transformExpression left
    rightVar <- transformExpression right
    resultVar <- newTempVariable (abiTypeFromHIR exprType)
    let tacOp = binaryOpToTAC op
    emitInstruction $ TACInstruction tacOp (Just resultVar) 
      [OpVariable leftVar, OpVariable rightVar] (abiTypeFromHIR exprType) pos
    return resultVar
  
  _ -> error "TODO: Implement other expression transformations"

-- ============================================================================
-- ПОСТРОЕНИЕ CFG И ДЕРЕВА ДОМИНАТОРОВ
-- ============================================================================

-- | Строит граф потока управления
constructCFG :: IntMap BasicBlock -> BlockId -> BlockId -> CFG
constructCFG blocks entryId exitId = 
  let nodes = IntMap.map blockToCFGNode blocks
      edges = concatMap blockToEdges (IntMap.elems blocks)
  in CFG nodes edges entryId exitId
  where
    blockToCFGNode block = CFGNode
      { cfgNodeId = bbId block
      , cfgNodeBlock = block
      , cfgNodePredecessors = bbPredecessors block
      , cfgNodeSuccessors = bbSuccessors block
      }
    
    blockToEdges block = 
      map (\succ -> CFGEdge (bbId block) succ Nothing) (bbSuccessors block)

-- | Строит дерево доминаторов
buildDominatorTree :: CFG -> DominatorTree
buildDominatorTree cfg = 
  let dominators = computeDominators cfg
      (children, parents) = buildTreeRelations dominators
  in DominatorTree
    { dtRoot = cfgEntry cfg
    , dtChildren = children
    , dtParent = parents
    , dtDominators = dominators
    }

-- | Вычисляет доминаторы для всех узлов
computeDominators :: CFG -> Map BlockId (Set BlockId)
computeDominators cfg = 
  let allNodes = Map.keys (cfgNodes cfg)
      initialDoms = Map.fromList $ 
        (cfgEntry cfg, Set.singleton (cfgEntry cfg)) : 
        [(node, Set.fromList allNodes) | node <- allNodes, node /= cfgEntry cfg]
  in iterateUntilFixed (updateDominators cfg) initialDoms

-- | Обновляет доминаторы за одну итерацию
updateDominators :: CFG -> Map BlockId (Set BlockId) -> Map BlockId (Set BlockId)
updateDominators cfg dominators = 
  Map.mapWithKey updateNode dominators
  where
    updateNode nodeId oldDoms
      | nodeId == cfgEntry cfg = Set.singleton nodeId
      | otherwise = 
          let preds = maybe [] cfgNodePredecessors (IntMap.lookup (getBlockIdInt nodeId) (cfgNodes cfg))
              predDoms = map (\p -> Map.findWithDefault Set.empty p dominators) preds
              newDoms = if null predDoms 
                       then Set.empty 
                       else foldl1 Set.intersection predDoms
          in Set.insert nodeId newDoms

-- ============================================================================
-- ОПТИМИЗАЦИИ
-- ============================================================================

-- | Применяет оптимизации к MIR программе
optimizeMIR :: [OptimizationPass] -> MIRProgram -> IO MIRProgram
optimizeMIR passes program = 
  foldl' (\prog pass -> prog >>= applyOptimization pass) (return program) passes

-- | Применяет конкретную оптимизацию
applyOptimization :: OptimizationPass -> MIRProgram -> IO MIRProgram
applyOptimization pass program = case pass of
  ConstantPropagation -> constantPropagation program
  DeadCodeElimination -> deadCodeElimination program
  _ -> return program  -- TODO: Реализовать другие оптимизации

-- | Распространение констант
constantPropagation :: MIRProgram -> IO MIRProgram
constantPropagation program = do
  optimizedFunctions <- mapM propagateInFunction (mirFunctions program)
  return $ program { mirFunctions = optimizedFunctions }
  where
    propagateInFunction func = do
      -- Анализируем константные значения
      let constants = analyzeConstants func
      -- Заменяем использования констант
      optimizedBlocks <- mapM (propagateInBlock constants) (IntMap.elems $ mfBlocks func)
      return $ func { mfBlocks = IntMap.fromList [(bbId block, block) | block <- optimizedBlocks] }

-- | Удаление мёртвого кода
deadCodeElimination :: MIRProgram -> IO MIRProgram
deadCodeElimination program = do
  optimizedFunctions <- mapM eliminateInFunction (mirFunctions program)
  return $ program { mirFunctions = optimizedFunctions }
  where
    eliminateInFunction func = do
      -- Вычисляем живость переменных
      let liveness = computeLiveness func
      -- Удаляем мёртвые инструкции
      optimizedBlocks <- mapM (eliminateInBlock liveness) (IntMap.elems $ mfBlocks func)
      return $ func { mfBlocks = IntMap.fromList [(bbId block, block) | block <- optimizedBlocks] }

-- | Вычисляет информацию о живости переменных
computeLiveness :: MIRFunction -> Map BlockId LivenessInfo
computeLiveness func = 
  let blocks = mfBlocks func
      cfg = mfCFG func
  in iterateUntilFixed (updateLiveness cfg blocks) (initializeLiveness blocks)

-- | Вычисляет достигающие определения
computeReachingDefinitions :: MIRFunction -> Map BlockId ReachingDefinitions
computeReachingDefinitions func = 
  let blocks = mfBlocks func
      cfg = mfCFG func
  in iterateUntilFixed (updateReachingDefs cfg blocks) (initializeReachingDefs blocks)

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Создаёт новый ID блока
newBlockId :: MIRTransform BlockId
newBlockId = do
  currentId <- gets msNextBlockId
  modify $ \s -> s { msNextBlockId = currentId + 1 }
  return $ BlockId currentId

-- | Создаёт новую SSA переменную
newSSAVariable :: Identifier -> ABIType -> MIRTransform SSAVariable
newSSAVariable name abiType = do
  version <- gets (Map.findWithDefault 0 name . msNextSSAVersion)
  modify $ \s -> s { msNextSSAVersion = Map.insert name (version + 1) (msNextSSAVersion s) }
  let ssaVar = SSAVariable name (SSAVersion version) abiType
  modify $ \s -> s { msVariableMap = Map.insert name ssaVar (msVariableMap s) }
  return ssaVar

-- | Создаёт временную переменную
newTempVariable :: ABIType -> MIRTransform SSAVariable
newTempVariable abiType = do
  tempId <- gets msNextBlockId  -- Используем счётчик блоков для уникальности
  modify $ \s -> s { msNextBlockId = tempId + 1 }
  let tempName = Identifier $ "temp" <> T.pack (show tempId)
  newSSAVariable tempName abiType

-- | Добавляет инструкцию в текущий блок
emitInstruction :: TACInstruction -> MIRTransform ()
emitInstruction instr = do
  currentBlockId <- gets msCurrentBlock
  modify $ \s -> s { msBlocks = IntMap.adjust (addInstruction instr) (getBlockIdInt currentBlockId) (msBlocks s) }
  where
    addInstruction instr block = block { bbInstructions = bbInstructions block ++ [instr] }

-- | Преобразует HIR тип в ABI тип
abiTypeFromHIR :: HIRType -> ABIType
abiTypeFromHIR = \case
  HIRChar True -> SignedByte
  HIRChar False -> UnsignedByte
  HIRInt True _ -> SignedWord
  HIRInt False _ -> UnsignedWord
  HIRBit -> BitType
  HIRPointer _ _ -> Pointer
  _ -> UnsignedByte  -- По умолчанию

-- | Преобразует бинарную операцию HIR в TAC
binaryOpToTAC :: BinaryOp -> TACOperation
binaryOpToTAC = \case
  Add -> TACAdd
  Sub -> TACSubtract
  Mul -> TACMultiply
  Div -> TACDivide
  Mod -> TACModulo
  BitwiseAnd -> TACBitwiseAnd
  BitwiseOr -> TACBitwiseOr
  BitwiseXor -> TACBitwiseXor
  LogicalAnd -> TACLogicalAnd
  LogicalOr -> TACLogicalOr
  Eq -> TACEqual
  Ne -> TACNotEqual
  Lt -> TACLessThan
  Le -> TACLessEqual
  Gt -> TACGreaterThan
  Ge -> TACGreaterEqual
  _ -> TACMove  -- По умолчанию

-- | Получает целочисленное значение BlockId
getBlockIdInt :: BlockId -> Int
getBlockIdInt (BlockId i) = i

-- | Итерирует функцию до достижения неподвижной точки
iterateUntilFixed :: Eq a => (a -> a) -> a -> a
iterateUntilFixed f x = 
  let x' = f x
  in if x == x' then x else iterateUntilFixed f x'

-- | Инициализирует анализ живости
initializeLiveness :: IntMap BasicBlock -> Map BlockId LivenessInfo
initializeLiveness blocks = 
  Map.fromList [(bbId block, emptyLiveness) | block <- IntMap.elems blocks]
  where
    emptyLiveness = LivenessInfo Set.empty Set.empty Set.empty Set.empty

-- | Инициализирует анализ достигающих определений
initializeReachingDefs :: IntMap BasicBlock -> Map BlockId ReachingDefinitions
initializeReachingDefs blocks = 
  Map.fromList [(bbId block, emptyReachingDefs) | block <- IntMap.elems blocks]
  where
    emptyReachingDefs = ReachingDefinitions Set.empty Set.empty Set.empty Set.empty

-- | Обновляет живость за одну итерацию
updateLiveness :: CFG -> IntMap BasicBlock -> Map BlockId LivenessInfo -> Map BlockId LivenessInfo
updateLiveness cfg blocks liveness = 
  Map.mapWithKey (updateBlockLiveness cfg blocks liveness) liveness

-- | Обновляет живость для одного блока
updateBlockLiveness :: CFG -> IntMap BasicBlock -> Map BlockId LivenessInfo -> BlockId -> LivenessInfo -> LivenessInfo
updateBlockLiveness cfg blocks liveness blockId oldInfo = 
  let block = IntMap.findWithDefault (error "Block not found") (getBlockIdInt blockId) blocks
      successors = bbSuccessors block
      liveOut = Set.unions [liLiveIn info | succId <- successors, 
                           Just info <- [Map.lookup succId liveness]]
      liveIn = Set.union (liUsed oldInfo) (Set.difference liveOut (liDefined oldInfo))
  in oldInfo { liLiveIn = liveIn, liLiveOut = liveOut }

-- | Обновляет достигающие определения за одну итерацию
updateReachingDefs :: CFG -> IntMap BasicBlock -> Map BlockId ReachingDefinitions -> Map BlockId ReachingDefinitions
updateReachingDefs cfg blocks reachingDefs = 
  Map.mapWithKey (updateBlockReachingDefs cfg blocks reachingDefs) reachingDefs

-- | Обновляет достигающие определения для одного блока
updateBlockReachingDefs :: CFG -> IntMap BasicBlock -> Map BlockId ReachingDefinitions -> BlockId -> ReachingDefinitions -> ReachingDefinitions
updateBlockReachingDefs cfg blocks reachingDefs blockId oldDefs = 
  let block = IntMap.findWithDefault (error "Block not found") (getBlockIdInt blockId) blocks
      predecessors = bbPredecessors block
      reachingIn = Set.unions [rdReachingOut defs | predId <- predecessors,
                              Just defs <- [Map.lookup predId reachingDefs]]
      reachingOut = Set.union (rdGenerated oldDefs) 
                             (Set.difference reachingIn (rdKilled oldDefs))
  in oldDefs { rdReachingIn = reachingIn, rdReachingOut = reachingOut }

-- | Анализирует константные значения в функции
analyzeConstants :: MIRFunction -> Map SSAVariable Literal
analyzeConstants func = Map.empty  -- TODO: Реализовать анализ констант

-- | Применяет распространение констант к блоку
propagateInBlock :: Map SSAVariable Literal -> BasicBlock -> IO BasicBlock
propagateInBlock constants block = return block  -- TODO: Реализовать

-- | Применяет удаление мёртвого кода к блоку
eliminateInBlock :: Map BlockId LivenessInfo -> BasicBlock -> IO BasicBlock
eliminateInBlock liveness block = return block  -- TODO: Реализовать

-- | Строит отношения родитель-ребёнок для дерева доминаторов
buildTreeRelations :: Map BlockId (Set BlockId) -> (Map BlockId [BlockId], Map BlockId BlockId)
buildTreeRelations dominators = (Map.empty, Map.empty)  -- TODO: Реализовать

-- ============================================================================
-- УТИЛИТЫ
-- ============================================================================

-- | Красивое отображение MIR
prettyMIR :: MIRProgram -> Text
prettyMIR program = T.unlines $
  map prettyMIRFunction (mirFunctions program)
  where
    prettyMIRFunction func = T.unlines
      [ "function " <> identifierText (mfName func) <> ":"
      , T.unlines [prettyBasicBlock block | block <- IntMap.elems (mfBlocks func)]
      ]
    
    prettyBasicBlock block = T.unlines
      [ "  block " <> T.pack (show $ bbId block) <> ":"
      , T.unlines [prettyPhiFunction phi | phi <- bbPhiFunctions block]
      , T.unlines [prettyTACInstruction instr | instr <- bbInstructions block]
      , prettyTACInstruction (bbTerminator block)
      ]
    
    prettyPhiFunction phi = 
      "    " <> prettySSAVariable (phiResult phi) <> " = phi " <>
      T.intercalate ", " [prettySSAVariable var <> "@" <> T.pack (show block) | (var, block) <- phiInputs phi]
    
    prettyTACInstruction instr = 
      "    " <> maybe "" (\r -> prettySSAVariable r <> " = ") (tacResult instr) <>
      T.pack (show $ tacOperation instr) <> " " <>
      T.intercalate ", " (map prettyTACOperand $ tacOperands instr)
    
    prettySSAVariable var = 
      identifierText (ssaName var) <> "." <> T.pack (show $ ssaVersion var)
    
    prettyTACOperand = \case
      OpVariable var -> prettySSAVariable var
      OpConstant lit -> T.pack (show lit)
      OpRegister reg -> T.pack (show reg)
      OpMemory mem -> "[" <> prettyTACOperand (mlBase mem) <> "+" <> T.pack (show $ mlOffset mem) <> "]"
      OpLabel blockId -> "L" <> T.pack (show blockId)

-- | Валидирует MIR программу
validateMIR :: MIRProgram -> [Text]
validateMIR _program = []  -- TODO: Реализовать валидацию

-- | Статистика MIR программы
mirStatistics :: MIRProgram -> Text
mirStatistics program = T.unlines
  [ "MIR Statistics:"
  , "Functions: " <> T.pack (show $ length $ mirFunctions program)
  , "Total blocks: " <> T.pack (show $ sum [IntMap.size (mfBlocks f) | f <- mirFunctions program])
  , "Total instructions: " <> T.pack (show $ sum [sum [length (bbInstructions b) | b <- IntMap.elems (mfBlocks f)] | f <- mirFunctions program])
  ]

-- Импорты для недостающих типов
import HCL.IR.HIR (HIRDeclaration(..), Symbol(..), symName)
import HCL.Types (BinaryOp(..)) 