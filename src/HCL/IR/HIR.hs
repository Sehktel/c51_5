{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}

-- | Модуль высокоуровневого промежуточного представления (HIR)
--
-- HIR представляет собой типизированную версию AST после семантического
-- анализа. На этом уровне:
-- * Все типы проверены и аннотированы
-- * Разрешены все идентификаторы
-- * Выполнены семантические проверки
-- * Подготовлена основа для оптимизаций
--
-- HIR служит мостом между синтаксическим анализом и генерацией кода,
-- обеспечивая типобезопасность и семантическую корректность.
module HCL.IR.HIR
  ( -- * Основные типы HIR
    HIRProgram(..)
  , HIRFunction(..)
  , HIRDeclaration(..)
  
    -- * Выражения HIR
  , HIRExpr(..)
  , HIRExprF(..)
  , HIRType(..)
  
    -- * Операторы HIR
  , HIRStmt(..)
  , HIRStmtF(..)
  
    -- * Символы и области видимости
  , Symbol(..)
  , SymbolTable
  , Scope(..)
  , ScopeId
  
    -- * Семантический анализ
  , SemanticAnalyzer
  , SemanticError(..)
  , runSemanticAnalysis
  , astToHIR
  
    -- * Утилиты
  , hirExprType
  , prettyHIR
  , validateHIR
  ) where

import Control.Monad.State.Strict (StateT, runStateT, get, put, modify)
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Control.Monad.Reader (ReaderT, runReaderT, ask, local)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (fromMaybe, isJust)
import Data.List (foldl')
import Control.Monad.Writer (Writer, runWriter)

import HCL.Types (SourcePos(..), Identifier(..), Literal(..), C51Type(..), 
                  BinaryOp(..), UnaryOp(..), AssignOp(..), identifierText)
import HCL.AST (Program(..), TopLevelDecl(..), FunctionDef(..), Declaration(..), 
                Expr(..), ExprF(..), Stmt(..), StmtF(..), TypeSpec(..), 
                StorageClass(..), MemoryModel(..), InterruptSpec(..))
import HCL.Error (CompilerError(..), Diagnostic(..), DiagnosticLevel(..), mkError)

-- ============================================================================
-- ТИПЫ HIR
-- ============================================================================

-- | Программа в HIR представлении
data HIRProgram = HIRProgram
  { hirFunctions :: ![HIRFunction]      -- ^ Функции программы
  , hirGlobals :: ![HIRDeclaration]     -- ^ Глобальные переменные
  , hirSymbolTable :: !SymbolTable      -- ^ Таблица символов
  , hirPos :: !SourcePos                -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Функция в HIR
data HIRFunction = HIRFunction
  { hirFuncName :: !Identifier          -- ^ Имя функции
  , hirFuncType :: !HIRType             -- ^ Тип функции
  , hirFuncParams :: ![HIRDeclaration]  -- ^ Параметры
  , hirFuncBody :: ![HIRStmt]           -- ^ Тело функции
  , hirFuncScope :: !ScopeId            -- ^ Область видимости
  , hirFuncInterrupt :: !(Maybe InterruptSpec)  -- ^ Спецификация прерывания
  , hirFuncPos :: !SourcePos            -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Декларация в HIR
data HIRDeclaration = HIRDeclaration
  { hirDeclName :: !Identifier          -- ^ Имя переменной
  , hirDeclType :: !HIRType             -- ^ Тип переменной
  , hirDeclInit :: !(Maybe HIRExpr)     -- ^ Инициализатор
  , hirDeclStorage :: !(Maybe StorageClass)  -- ^ Класс хранения
  , hirDeclMemory :: !(Maybe MemoryModel)    -- ^ Модель памяти
  , hirDeclScope :: !ScopeId            -- ^ Область видимости
  , hirDeclPos :: !SourcePos            -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- ============================================================================
-- ТИПЫ В HIR
-- ============================================================================

-- | Типизированная система HIR
data HIRType
  = HIRVoid                             -- ^ void
  | HIRChar !Bool                       -- ^ char (signed/unsigned)
  | HIRInt !Bool !Int                   -- ^ int (signed/unsigned, size in bits)
  | HIRFloat !Int                       -- ^ float (size in bits)
  | HIRBit                              -- ^ bit (8051)
  | HIRPointer !HIRType !MemoryModel    -- ^ указатель с моделью памяти
  | HIRArray !HIRType !Int              -- ^ массив с известным размером
  | HIRFunction !HIRType ![HIRType]     -- ^ тип функции
  | HIRStruct !Identifier ![HIRDeclaration]  -- ^ структура
  | HIRUnion !Identifier ![HIRDeclaration]   -- ^ объединение
  | HIREnum !Identifier ![Identifier]       -- ^ перечисление
  deriving (Eq, Show)

-- ============================================================================
-- ВЫРАЖЕНИЯ HIR
-- ============================================================================

-- | Типизированное выражение HIR
data HIRExpr = HIRExpr
  { hirExprNode :: !HIRExprF            -- ^ Узел выражения
  , hirExprType :: !HIRType             -- ^ Тип выражения
  , hirExprPos :: !SourcePos            -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Функтор для выражений HIR
data HIRExprF
  = HIRLiteral !Literal                 -- ^ Литерал
  | HIRVariable !Symbol                 -- ^ Переменная (разрешенная)
  | HIRBinary !BinaryOp !HIRExpr !HIRExpr  -- ^ Бинарная операция
  | HIRUnary !UnaryOp !HIRExpr          -- ^ Унарная операция
  | HIRAssign !AssignOp !HIRExpr !HIRExpr   -- ^ Присваивание
  | HIRCall !Symbol ![HIRExpr]          -- ^ Вызов функции
  | HIRArrayAccess !HIRExpr !HIRExpr    -- ^ Доступ к массиву
  | HIRFieldAccess !HIRExpr !Identifier -- ^ Доступ к полю
  | HIRCast !HIRType !HIRExpr           -- ^ Приведение типа
  | HIRSizeOf !HIRType                  -- ^ sizeof
  | HIRTernary !HIRExpr !HIRExpr !HIRExpr  -- ^ Тернарный оператор
  | HIRBitAccess !HIRExpr !HIRExpr      -- ^ Доступ к биту (8051)
  deriving (Eq, Show)

-- ============================================================================
-- ОПЕРАТОРЫ HIR
-- ============================================================================

-- | Оператор HIR
data HIRStmt = HIRStmt
  { hirStmtNode :: !HIRStmtF            -- ^ Узел оператора
  , hirStmtPos :: !SourcePos            -- ^ Позиция в исходном коде
  } deriving (Eq, Show)

-- | Функтор для операторов HIR
data HIRStmtF
  = HIRExprStmt !HIRExpr                -- ^ Выражение-оператор
  , HIRDecl !HIRDeclaration             -- ^ Декларация
  | HIRCompound ![HIRStmt] !ScopeId     -- ^ Составной оператор с областью видимости
  | HIRIf !HIRExpr !HIRStmt !(Maybe HIRStmt)  -- ^ if-else
  | HIRWhile !HIRExpr !HIRStmt          -- ^ while
  | HIRFor !(Maybe HIRExpr) !(Maybe HIRExpr) !(Maybe HIRExpr) !HIRStmt  -- ^ for
  | HIRDoWhile !HIRStmt !HIRExpr        -- ^ do-while
  | HIRSwitch !HIRExpr ![HIRCaseStmt]   -- ^ switch
  | HIRBreak                            -- ^ break
  | HIRContinue                         -- ^ continue
  | HIRReturn !(Maybe HIRExpr)          -- ^ return
  | HIRGoto !Identifier                 -- ^ goto
  | HIRLabel !Identifier !HIRStmt       -- ^ метка
  deriving (Eq, Show)

-- | Ветка case в HIR
data HIRCaseStmt
  = HIRCase !HIRExpr ![HIRStmt] !SourcePos     -- ^ case
  | HIRDefault ![HIRStmt] !SourcePos           -- ^ default
  deriving (Eq, Show)

-- ============================================================================
-- СИМВОЛЫ И ОБЛАСТИ ВИДИМОСТИ
-- ============================================================================

-- | Символ в таблице символов
data Symbol = Symbol
  { symName :: !Identifier              -- ^ Имя символа
  , symType :: !HIRType                 -- ^ Тип символа
  , symScope :: !ScopeId                -- ^ Область видимости
  , symStorage :: !(Maybe StorageClass) -- ^ Класс хранения
  , symMemory :: !(Maybe MemoryModel)   -- ^ Модель памяти
  , symPos :: !SourcePos                -- ^ Позиция определения
  } deriving (Eq, Show)

-- | Таблица символов
type SymbolTable = Map Identifier Symbol

-- | Идентификатор области видимости
type ScopeId = Int

-- | Область видимости
data Scope = Scope
  { scopeId :: !ScopeId                 -- ^ Идентификатор области
  , scopeParent :: !(Maybe ScopeId)     -- ^ Родительская область
  , scopeSymbols :: !SymbolTable        -- ^ Символы в области
  , scopeLevel :: !Int                  -- ^ Уровень вложенности
  } deriving (Eq, Show)

-- ============================================================================
-- СЕМАНТИЧЕСКИЙ АНАЛИЗАТОР
-- ============================================================================

-- | Состояние семантического анализатора
data SemanticState = SemanticState
  { ssSymbolTable :: !SymbolTable       -- ^ Глобальная таблица символов
  , ssScopes :: !(Map ScopeId Scope)    -- ^ Области видимости
  , ssCurrentScope :: !ScopeId          -- ^ Текущая область видимости
  , ssNextScopeId :: !ScopeId           -- ^ Следующий ID области
  , ssDiagnostics :: ![Diagnostic]      -- ^ Диагностические сообщения
  } deriving (Eq, Show)

-- | Ошибки семантического анализа
data SemanticError
  = UndefinedSymbol !Identifier !SourcePos
  | RedefinedSymbol !Identifier !SourcePos !SourcePos
  | TypeMismatch !HIRType !HIRType !SourcePos
  | InvalidOperation !Text !SourcePos
  | InvalidCast !HIRType !HIRType !SourcePos
  | InvalidAssignment !HIRType !HIRType !SourcePos
  | UndefinedFunction !Identifier !SourcePos
  | ArgumentCountMismatch !Int !Int !SourcePos
  | ArgumentTypeMismatch !HIRType !HIRType !Int !SourcePos
  | ReturnTypeMismatch !HIRType !HIRType !SourcePos
  | VoidValueUsed !SourcePos
  | InvalidArraySize !SourcePos
  | InvalidBitAccess !SourcePos
  deriving (Eq, Show)

-- | Монада семантического анализатора
type SemanticAnalyzer = StateT SemanticState (ExceptT SemanticError IO)

-- | Начальное состояние анализатора
initialSemanticState :: SemanticState
initialSemanticState = SemanticState
  { ssSymbolTable = Map.empty
  , ssScopes = Map.singleton 0 (Scope 0 Nothing Map.empty 0)
  , ssCurrentScope = 0
  , ssNextScopeId = 1
  , ssDiagnostics = []
  }

-- ============================================================================
-- ОСНОВНЫЕ ФУНКЦИИ СЕМАНТИЧЕСКОГО АНАЛИЗА
-- ============================================================================

-- | Запускает семантический анализ
runSemanticAnalysis :: Program -> IO (Either SemanticError HIRProgram, [Diagnostic])
runSemanticAnalysis program = do
  result <- runExceptT $ runStateT (astToHIR program) initialSemanticState
  case result of
    Left err -> return (Left err, [])
    Right (hirProgram, finalState) -> 
      return (Right hirProgram, reverse $ ssDiagnostics finalState)

-- | Преобразует AST в HIR
astToHIR :: Program -> SemanticAnalyzer HIRProgram
astToHIR (Program decls pos) = do
  -- Первый проход: собираем все декларации функций и глобальных переменных
  mapM_ collectTopLevelDecl decls
  
  -- Второй проход: анализируем тела функций
  hirFunctions <- mapM analyzeFunction =<< getFunctions decls
  hirGlobals <- mapM analyzeGlobalDecl =<< getGlobalDecls decls
  
  symbolTable <- gets ssSymbolTable
  return $ HIRProgram hirFunctions hirGlobals symbolTable pos
  where
    getFunctions = return . [f | TLFunction f <- decls]
    getGlobalDecls = return . [d | TLDeclaration d <- decls]

-- | Собирает декларации верхнего уровня
collectTopLevelDecl :: TopLevelDecl -> SemanticAnalyzer ()
collectTopLevelDecl = \case
  TLFunction func -> collectFunctionDecl func
  TLDeclaration decl -> collectGlobalDecl decl
  TLBitDecl _ -> return ()  -- TODO: Обработка sbit
  TLSfrDecl _ -> return ()  -- TODO: Обработка sfr

-- | Собирает декларацию функции
collectFunctionDecl :: FunctionDef -> SemanticAnalyzer ()
collectFunctionDecl FunctionDef{..} = do
  -- Преобразуем тип возвращаемого значения
  returnType <- typeSpecToHIRType funcReturnType
  
  -- Преобразуем типы параметров
  paramTypes <- mapM (typeSpecToHIRType . paramType) funcParams
  
  let funcType = HIRFunction returnType paramTypes
      symbol = Symbol funcName funcType 0 funcStorageClass funcMemoryModel funcPos
  
  -- Добавляем в таблицу символов
  addSymbol symbol

-- | Собирает глобальную декларацию
collectGlobalDecl :: Declaration -> SemanticAnalyzer ()
collectGlobalDecl Declaration{..} = do
  hirType <- typeSpecToHIRType declType
  let symbol = Symbol declName hirType 0 declStorageClass declMemoryModel declPos
  addSymbol symbol

-- | Анализирует функцию
analyzeFunction :: FunctionDef -> SemanticAnalyzer HIRFunction
analyzeFunction FunctionDef{..} = do
  -- Создаем новую область видимости для функции
  funcScopeId <- newScope (Just 0)
  
  -- Анализируем параметры в контексте функции
  hirParams <- withScope funcScopeId $ do
    mapM analyzeParameter funcParams
  
  -- Анализируем тело функции
  hirBody <- withScope funcScopeId $ do
    mapM analyzeStatement (compoundStmts funcBody)
  
  -- Получаем тип функции из таблицы символов
  symbol <- lookupSymbol funcName
  funcType <- case symbol of
    Just sym -> return (symType sym)
    Nothing -> throwError $ UndefinedFunction funcName funcPos
  
  return $ HIRFunction
    { hirFuncName = funcName
    , hirFuncType = funcType
    , hirFuncParams = hirParams
    , hirFuncBody = hirBody
    , hirFuncScope = funcScopeId
    , hirFuncInterrupt = funcInterrupt
    , hirFuncPos = funcPos
    }

-- | Анализирует параметр функции
analyzeParameter :: Parameter -> SemanticAnalyzer HIRDeclaration
analyzeParameter Parameter{..} = do
  hirType <- typeSpecToHIRType paramType
  case paramName of
    Just name -> do
      currentScope <- gets ssCurrentScope
      let symbol = Symbol name hirType currentScope Nothing paramMemoryModel paramPos
      addSymbol symbol
      return $ HIRDeclaration name hirType Nothing Nothing paramMemoryModel currentScope paramPos
    Nothing -> 
      -- Параметр без имени (в прототипе функции)
      return $ HIRDeclaration (Identifier "_unnamed") hirType Nothing Nothing paramMemoryModel 0 paramPos

-- | Анализирует глобальную декларацию
analyzeGlobalDecl :: Declaration -> SemanticAnalyzer HIRDeclaration
analyzeGlobalDecl Declaration{..} = do
  hirType <- typeSpecToHIRType declType
  hirInit <- mapM analyzeExpression declInit
  
  -- Проверяем совместимость типов инициализатора
  case hirInit of
    Just initExpr -> do
      let initType = hirExprType initExpr
      unless (isCompatibleType hirType initType) $
        throwError $ TypeMismatch hirType initType declPos
    Nothing -> return ()
  
  return $ HIRDeclaration
    { hirDeclName = declName
    , hirDeclType = hirType
    , hirDeclInit = hirInit
    , hirDeclStorage = declStorageClass
    , hirDeclMemory = declMemoryModel
    , hirDeclScope = 0  -- Глобальная область
    , hirDeclPos = declPos
    }

-- | Анализирует оператор
analyzeStatement :: Stmt -> SemanticAnalyzer HIRStmt
analyzeStatement (Stmt stmtNode pos) = case stmtNode of
  SExpr expr -> do
    hirExpr <- analyzeExpression expr
    return $ HIRStmt (HIRExprStmt hirExpr) pos
  
  SDecl decl -> do
    hirDecl <- analyzeLocalDecl decl
    return $ HIRStmt (HIRDecl hirDecl) pos
  
  SCompound compound -> do
    compoundScopeId <- newScope . Just =<< gets ssCurrentScope
    hirStmts <- withScope compoundScopeId $ 
      mapM analyzeStatement (compoundStmts compound)
    return $ HIRStmt (HIRCompound hirStmts compoundScopeId) pos
  
  SIf condition thenStmt elseStmt -> do
    hirCondition <- analyzeExpression condition
    hirThenStmt <- analyzeStatement thenStmt
    hirElseStmt <- mapM analyzeStatement elseStmt
    
    -- Проверяем, что условие имеет скалярный тип
    unless (isScalarType $ hirExprType hirCondition) $
      throwError $ TypeMismatch (HIRInt True 8) (hirExprType hirCondition) pos
    
    return $ HIRStmt (HIRIf hirCondition hirThenStmt hirElseStmt) pos
  
  SReturn mexpr -> do
    hirExpr <- mapM analyzeExpression mexpr
    -- TODO: Проверка совместимости с типом возвращаемого значения функции
    return $ HIRStmt (HIRReturn hirExpr) pos
  
  _ -> error "TODO: Implement other statement types"

-- | Анализирует локальную декларацию
analyzeLocalDecl :: Declaration -> SemanticAnalyzer HIRDeclaration
analyzeLocalDecl Declaration{..} = do
  hirType <- typeSpecToHIRType declType
  hirInit <- mapM analyzeExpression declInit
  currentScope <- gets ssCurrentScope
  
  let symbol = Symbol declName hirType currentScope declStorageClass declMemoryModel declPos
  addSymbol symbol
  
  return $ HIRDeclaration
    { hirDeclName = declName
    , hirDeclType = hirType
    , hirDeclInit = hirInit
    , hirDeclStorage = declStorageClass
    , hirDeclMemory = declMemoryModel
    , hirDeclScope = currentScope
    , hirDeclPos = declPos
    }

-- | Анализирует выражение
analyzeExpression :: Expr -> SemanticAnalyzer HIRExpr
analyzeExpression (Expr exprNode pos) = case exprNode of
  ELiteral lit -> do
    let hirType = literalToHIRType lit
    return $ HIRExpr (HIRLiteral lit) hirType pos
  
  EVariable name -> do
    symbol <- lookupSymbol name
    case symbol of
      Just sym -> return $ HIRExpr (HIRVariable sym) (symType sym) pos
      Nothing -> throwError $ UndefinedSymbol name pos
  
  EBinary op left right -> do
    hirLeft <- analyzeExpression left
    hirRight <- analyzeExpression right
    
    let leftType = hirExprType hirLeft
        rightType = hirExprType hirRight
    
    -- Определяем результирующий тип бинарной операции
    resultType <- binaryOpResultType op leftType rightType pos
    
    return $ HIRExpr (HIRBinary op hirLeft hirRight) resultType pos
  
  ECall name args -> do
    symbol <- lookupSymbol name
    case symbol of
      Just sym -> case symType sym of
        HIRFunction returnType paramTypes -> do
          hirArgs <- mapM analyzeExpression args
          
          -- Проверяем количество аргументов
          let argCount = length hirArgs
              paramCount = length paramTypes
          unless (argCount == paramCount) $
            throwError $ ArgumentCountMismatch paramCount argCount pos
          
          -- Проверяем типы аргументов
          zipWithM_ checkArgumentType hirArgs (zip paramTypes [1..])
          
          return $ HIRExpr (HIRCall sym hirArgs) returnType pos
        _ -> throwError $ InvalidOperation ("'" <> identifierText name <> "' is not a function") pos
      Nothing -> throwError $ UndefinedFunction name pos
  
  _ -> error "TODO: Implement other expression types"
  where
    checkArgumentType hirArg (expectedType, argNum) = do
      let actualType = hirExprType hirArg
      unless (isCompatibleType expectedType actualType) $
        throwError $ ArgumentTypeMismatch expectedType actualType argNum pos

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Преобразует TypeSpec в HIRType
typeSpecToHIRType :: TypeSpec -> SemanticAnalyzer HIRType
typeSpecToHIRType = \case
  TSVoid -> return HIRVoid
  TSChar -> return $ HIRChar True  -- По умолчанию signed
  TSInt -> return $ HIRInt True 16  -- 16-битный signed int для 8051
  TSFloat -> return $ HIRFloat 32
  TSDouble -> return $ HIRFloat 64
  TSUnsigned -> return $ HIRInt False 16
  TSBit -> return HIRBit
  TSPointer baseType -> do
    hirBaseType <- typeSpecToHIRType baseType
    return $ HIRPointer hirBaseType MMData  -- По умолчанию data память
  TSArray baseType sizeExpr -> do
    hirBaseType <- typeSpecToHIRType baseType
    -- TODO: Вычислить размер массива из выражения
    return $ HIRArray hirBaseType 10  -- Заглушка
  _ -> error "TODO: Implement other type specs"

-- | Преобразует литерал в HIRType
literalToHIRType :: Literal -> HIRType
literalToHIRType = \case
  IntLiteral _ _ -> HIRInt True 16  -- 16-битный signed int
  CharLit _ -> HIRChar True
  StringLit _ -> HIRPointer (HIRChar True) MMCode  -- Строки в памяти программы
  BoolLit _ -> HIRBit

-- | Определяет результирующий тип бинарной операции
binaryOpResultType :: BinaryOp -> HIRType -> HIRType -> SourcePos -> SemanticAnalyzer HIRType
binaryOpResultType op leftType rightType pos = case op of
  Add | Sub | Mul | Div | Mod -> do
    -- Арифметические операции требуют числовых типов
    unless (isNumericType leftType && isNumericType rightType) $
      throwError $ InvalidOperation "Arithmetic operation requires numeric types" pos
    return $ promoteTypes leftType rightType
  
  Eq | Ne | Lt | Le | Gt | Ge -> do
    -- Операции сравнения возвращают boolean (bit)
    unless (isCompatibleType leftType rightType) $
      throwError $ TypeMismatch leftType rightType pos
    return HIRBit
  
  LogicalAnd | LogicalOr -> do
    -- Логические операции требуют скалярных типов
    unless (isScalarType leftType && isScalarType rightType) $
      throwError $ InvalidOperation "Logical operation requires scalar types" pos
    return HIRBit
  
  BitwiseAnd | BitwiseOr | BitwiseXor | ShiftLeft | ShiftRight -> do
    -- Битовые операции требуют целочисленных типов
    unless (isIntegralType leftType && isIntegralType rightType) $
      throwError $ InvalidOperation "Bitwise operation requires integral types" pos
    return $ promoteTypes leftType rightType

-- | Проверяет совместимость типов
isCompatibleType :: HIRType -> HIRType -> Bool
isCompatibleType t1 t2 = t1 == t2 || canImplicitlyCast t1 t2

-- | Проверяет возможность неявного приведения типов
canImplicitlyCast :: HIRType -> HIRType -> Bool
canImplicitlyCast from to = case (from, to) of
  (HIRChar _, HIRInt _ _) -> True
  (HIRInt _ s1, HIRInt _ s2) -> s1 <= s2
  (HIRBit, HIRInt _ _) -> True
  _ -> False

-- | Проверяет, является ли тип числовым
isNumericType :: HIRType -> Bool
isNumericType = \case
  HIRChar _ -> True
  HIRInt _ _ -> True
  HIRFloat _ -> True
  HIRBit -> True
  _ -> False

-- | Проверяет, является ли тип скалярным
isScalarType :: HIRType -> Bool
isScalarType = \case
  HIRPointer _ _ -> True
  t -> isNumericType t

-- | Проверяет, является ли тип целочисленным
isIntegralType :: HIRType -> Bool
isIntegralType = \case
  HIRChar _ -> True
  HIRInt _ _ -> True
  HIRBit -> True
  _ -> False

-- | Продвигает типы для арифметических операций
promoteTypes :: HIRType -> HIRType -> HIRType
promoteTypes t1 t2 = case (t1, t2) of
  (HIRInt s1 sz1, HIRInt s2 sz2) -> HIRInt (s1 && s2) (max sz1 sz2)
  (HIRFloat sz1, HIRFloat sz2) -> HIRFloat (max sz1 sz2)
  (HIRFloat sz, _) -> HIRFloat sz
  (_, HIRFloat sz) -> HIRFloat sz
  _ -> t1  -- Упрощение

-- | Добавляет символ в таблицу
addSymbol :: Symbol -> SemanticAnalyzer ()
addSymbol symbol = do
  existing <- lookupSymbol (symName symbol)
  case existing of
    Just existingSym -> 
      throwError $ RedefinedSymbol (symName symbol) (symPos symbol) (symPos existingSym)
    Nothing -> do
      modify $ \s -> s { ssSymbolTable = Map.insert (symName symbol) symbol (ssSymbolTable s) }

-- | Ищет символ в таблице
lookupSymbol :: Identifier -> SemanticAnalyzer (Maybe Symbol)
lookupSymbol name = do
  symbolTable <- gets ssSymbolTable
  return $ Map.lookup name symbolTable

-- | Создает новую область видимости
newScope :: Maybe ScopeId -> SemanticAnalyzer ScopeId
newScope parentId = do
  state <- get
  let scopeId = ssNextScopeId state
      parentLevel = case parentId of
        Just pid -> maybe 0 ((+1) . scopeLevel) $ Map.lookup pid (ssScopes state)
        Nothing -> 0
      newScopeObj = Scope scopeId parentId Map.empty parentLevel
  
  put $ state 
    { ssScopes = Map.insert scopeId newScopeObj (ssScopes state)
    , ssNextScopeId = scopeId + 1
    }
  
  return scopeId

-- | Выполняет действие в контексте области видимости
withScope :: ScopeId -> SemanticAnalyzer a -> SemanticAnalyzer a
withScope scopeId action = do
  oldScope <- gets ssCurrentScope
  modify $ \s -> s { ssCurrentScope = scopeId }
  result <- action
  modify $ \s -> s { ssCurrentScope = oldScope }
  return result

-- ============================================================================
-- УТИЛИТЫ
-- ============================================================================

-- | Получает тип HIR выражения
hirExprType :: HIRExpr -> HIRType
hirExprType = hirExprType

-- | Красивое отображение HIR
prettyHIR :: HIRProgram -> Text
prettyHIR HIRProgram{..} = T.unlines $
  map prettyHIRFunction hirFunctions ++
  map prettyHIRDeclaration hirGlobals
  where
    prettyHIRFunction HIRFunction{..} =
      T.concat [ prettyHIRType hirFuncType
               , " "
               , identifierText hirFuncName
               , "(...) { ... }"
               ]
    
    prettyHIRDeclaration HIRDeclaration{..} =
      T.concat [ prettyHIRType hirDeclType
               , " "
               , identifierText hirDeclName
               , ";"
               ]
    
    prettyHIRType = \case
      HIRVoid -> "void"
      HIRChar True -> "char"
      HIRChar False -> "unsigned char"
      HIRInt True sz -> "int" <> T.pack (show sz)
      HIRInt False sz -> "unsigned int" <> T.pack (show sz)
      HIRBit -> "bit"
      HIRPointer t _ -> prettyHIRType t <> "*"
      HIRArray t sz -> prettyHIRType t <> "[" <> T.pack (show sz) <> "]"
      _ -> "..."

-- | Валидация HIR программы
validateHIR :: HIRProgram -> [Diagnostic]
validateHIR program = 
  let validationState = ValidationState [] Map.empty
      (_, diagnostics) = runWriter $ runStateT (validateProgram program) validationState
  in diagnostics
  where
    validateProgram :: HIRProgram -> StateT ValidationState (Writer [Diagnostic]) ()
    validateProgram (HIRProgram decls) = mapM_ validateDeclaration decls
    
    validateDeclaration :: HIRDeclaration -> StateT ValidationState (Writer [Diagnostic]) ()
    validateDeclaration = \case
      HIRFunctionDecl retType name params body pos -> do
        -- Проверяем уникальность имени функции
        checkUniqueFunction name pos
        -- Валидируем тело функции
        validateBlock body
        -- Проверяем return statements
        checkReturnStatements retType body pos
      
      HIRVariableDecl varType name maybeInit pos -> do
        -- Проверяем уникальность имени переменной
        checkUniqueVariable name pos
        -- Валидируем инициализатор если есть
        case maybeInit of
          Just initExpr -> validateExpression initExpr
          Nothing -> return ()
      
      HIRBitDecl name bitAddr pos -> do
        checkUniqueVariable name pos
        -- Проверяем корректность битового адреса
        when (bitAddr < 0x20 || bitAddr > 0x2F) $
          emitWarning $ Diagnostic WarningLevel pos $
            "Битовый адрес " <> T.pack (show bitAddr) <> " вне стандартного диапазона"
      
      HIRSfrDecl name addr pos -> do
        checkUniqueVariable name pos
        -- Проверяем корректность SFR адреса
        when (addr < 0x80 || addr > 0xFF) $
          emitError $ Diagnostic ErrorLevel pos $
            "SFR адрес " <> T.pack (show addr) <> " вне допустимого диапазона"
    
    validateBlock :: HIRBlock -> StateT ValidationState (Writer [Diagnostic]) ()
    validateBlock (HIRBlock decls stmts _) = do
      mapM_ validateDeclaration decls
      mapM_ validateStatement stmts
    
    validateStatement :: HIRStatement -> StateT ValidationState (Writer [Diagnostic]) ()
    validateStatement = \case
      HIRExpressionStmt expr _ -> validateExpression expr
      HIRCompoundStmt block _ -> validateBlock block
      HIRIfStmt condExpr thenStmt maybeElseStmt _ -> do
        validateExpression condExpr
        validateStatement thenStmt
        case maybeElseStmt of
          Just elseStmt -> validateStatement elseStmt
          Nothing -> return ()
      HIRWhileStmt condExpr bodyStmt _ -> do
        validateExpression condExpr
        validateStatement bodyStmt
      HIRForStmt maybeInit maybeCond maybeUpdate bodyStmt _ -> do
        case maybeInit of
          Just initExpr -> validateExpression initExpr
          Nothing -> return ()
        case maybeCond of
          Just condExpr -> validateExpression condExpr
          Nothing -> return ()
        case maybeUpdate of
          Just updateExpr -> validateExpression updateExpr
          Nothing -> return ()
        validateStatement bodyStmt
      HIRReturnStmt maybeExpr _ -> do
        case maybeExpr of
          Just expr -> validateExpression expr
          Nothing -> return ()
      HIRBreakStmt _ -> return ()
      HIRContinueStmt _ -> return ()
      HIRGotoStmt _ _ -> return ()
      HIRLabelStmt _ stmt _ -> validateStatement stmt
    
    validateExpression :: HIRExpression -> StateT ValidationState (Writer [Diagnostic]) ()
    validateExpression = \case
      HIRLiteralExpr _ _ _ -> return ()
      HIRVariableExpr name _ _ -> checkVariableUsage name
      HIRBinaryExpr _ leftExpr rightExpr _ _ -> do
        validateExpression leftExpr
        validateExpression rightExpr
      HIRUnaryExpr _ expr _ _ -> validateExpression expr
      HIRAssignExpr _ lvalueExpr rvalueExpr _ _ -> do
        validateExpression lvalueExpr
        validateExpression rvalueExpr
      HIRCallExpr funcExpr args _ _ -> do
        validateExpression funcExpr
        mapM_ validateExpression args
      HIRArrayAccessExpr arrayExpr indexExpr _ _ -> do
        validateExpression arrayExpr
        validateExpression indexExpr
      HIRMemberAccessExpr structExpr _ _ _ -> validateExpression structExpr
      HIRPointerAccessExpr ptrExpr _ _ _ -> validateExpression ptrExpr
      HIRCastExpr _ expr _ -> validateExpression expr
    
    checkUniqueFunction :: Identifier -> SourcePos -> StateT ValidationState (Writer [Diagnostic]) ()
    checkUniqueFunction name pos = do
      state <- get
      case Map.lookup name (vsFunctions state) of
        Just existingPos -> 
          emitError $ Diagnostic ErrorLevel pos $
            "Функция " <> identifierText name <> " уже определена в " <> T.pack (show existingPos)
        Nothing -> 
          put state { vsFunctions = Map.insert name pos (vsFunctions state) }
    
    checkUniqueVariable :: Identifier -> SourcePos -> StateT ValidationState (Writer [Diagnostic]) ()
    checkUniqueVariable name pos = do
      state <- get
      case Map.lookup name (vsVariables state) of
        Just existingPos -> 
          emitError $ Diagnostic ErrorLevel pos $
            "Переменная " <> identifierText name <> " уже определена в " <> T.pack (show existingPos)
        Nothing -> 
          put state { vsVariables = Map.insert name pos (vsVariables state) }
    
    checkVariableUsage :: Identifier -> StateT ValidationState (Writer [Diagnostic]) ()
    checkVariableUsage name = do
      state <- get
      unless (Map.member name (vsVariables state)) $
        emitWarning $ Diagnostic WarningLevel (SourcePos "" 0 0) $
          "Использование необъявленной переменной: " <> identifierText name
    
    checkReturnStatements :: HIRType -> HIRBlock -> SourcePos -> StateT ValidationState (Writer [Diagnostic]) ()
    checkReturnStatements retType block pos = do
      let hasReturn = blockHasReturn block
      when (retType /= HIRVoidType && not hasReturn) $
        emitWarning $ Diagnostic WarningLevel pos $
          "Функция с возвращаемым типом может не возвращать значение"
    
    blockHasReturn :: HIRBlock -> Bool
    blockHasReturn (HIRBlock _ stmts _) = any statementHasReturn stmts
    
    statementHasReturn :: HIRStatement -> Bool
    statementHasReturn = \case
      HIRReturnStmt _ _ -> True
      HIRCompoundStmt block _ -> blockHasReturn block
      HIRIfStmt _ thenStmt maybeElseStmt _ -> 
        statementHasReturn thenStmt && 
        maybe False statementHasReturn maybeElseStmt
      _ -> False
    
    emitError :: Diagnostic -> StateT ValidationState (Writer [Diagnostic]) ()
    emitError diag = tell [diag]
    
    emitWarning :: Diagnostic -> StateT ValidationState (Writer [Diagnostic]) ()
    emitWarning diag = tell [diag]

-- | Состояние валидации
data ValidationState = ValidationState
  { vsErrors :: [Diagnostic]
  , vsFunctions :: Map Identifier SourcePos
  , vsVariables :: Map Identifier SourcePos
  } deriving (Show) 