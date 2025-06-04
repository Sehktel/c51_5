{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}

-- | Семантический анализ для HCL компилятора
--
-- Этот модуль выполняет семантический анализ AST и преобразует его в HIR.
-- Включает проверку типов, анализ области видимости, разрешение имён
-- и валидацию архитектурных ограничений AT89S4051.
--
-- Ключевые возможности:
-- * Типобезопасная проверка совместимости типов C
-- * Анализ области видимости с поддержкой вложенных блоков
-- * Разрешение перегрузки функций и операторов
-- * Валидация архитектурных ограничений (размеры типов, выравнивание)
-- * Проверка корректности использования расширений 8051
-- * Генерация информативных диагностических сообщений
module HCL.SemanticAnalysis
  ( -- * Основные функции
    runSemanticAnalysis
  , analyzeProgram
  , analyzeFunction
  , analyzeExpression
  , analyzeStatement
  
    -- * Анализатор и контекст
  , SemanticAnalyzer
  , AnalysisContext(..)
  , AnalysisState(..)
  , AnalysisResult(..)
  
    -- * Таблица символов
  , SymbolTable(..)
  , Symbol(..)
  , SymbolType(..)
  , Scope(..)
  , ScopeLevel(..)
  
    -- * Проверка типов
  , TypeChecker
  , TypeEnvironment(..)
  , TypeConstraint(..)
  , unifyTypes
  , checkTypeCompatibility
  
    -- * Диагностика
  , SemanticError(..)
  , SemanticWarning(..)
  , DiagnosticContext(..)
  
    -- * Утилиты
  , lookupSymbol
  , insertSymbol
  , enterScope
  , exitScope
  , resolveType
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
import Data.List (foldl', find, nub)
import Data.Ord (comparing)
import Data.Function (on)

import HCL.Types (SourcePos(..), Identifier(..), Literal(..), LiteralType(..), 
                  BinaryOp(..), UnaryOp(..), AssignOp(..), identifierText)
import HCL.AST
import HCL.IR.HIR (HIRProgram(..), HIRFunction(..), HIRExpression(..), HIRStatement(..), 
                   HIRType(..), HIRVariable(..), HIRLiteral(..), HIRBinaryOp(..), 
                   HIRUnaryOp(..), HIRBlock(..), HIRDeclaration(..))
import HCL.Error (Diagnostic(..), DiagnosticLevel(..), CompilerError(..))
import HCL.ABI (ABIType(..), ABIConfig, defaultABIConfig, getTypeSize, getTypeAlignment)

-- ============================================================================
-- ОСНОВНЫЕ ТИПЫ
-- ============================================================================

-- | Монада семантического анализа
type SemanticAnalyzer = ReaderT AnalysisContext (StateT AnalysisState (WriterT [Diagnostic] (ExceptT CompilerError IO)))

-- | Контекст анализа
data AnalysisContext = AnalysisContext
  { acABIConfig :: !ABIConfig                     -- ^ Конфигурация ABI
  , acTargetArch :: !Text                         -- ^ Целевая архитектура
  , acStrictMode :: !Bool                         -- ^ Строгий режим проверки
  , acWarningsAsErrors :: !Bool                   -- ^ Предупреждения как ошибки
  , acOptimizationLevel :: !Int                   -- ^ Уровень оптимизации
  } deriving (Eq, Show)

-- | Состояние анализа
data AnalysisState = AnalysisState
  { asSymbolTable :: !SymbolTable                 -- ^ Таблица символов
  , asTypeEnvironment :: !TypeEnvironment          -- ^ Окружение типов
  , asCurrentScope :: !ScopeLevel                 -- ^ Текущий уровень области видимости
  , asCurrentFunction :: !(Maybe Identifier)      -- ^ Текущая функция
  , asReturnType :: !(Maybe HIRType)              -- ^ Ожидаемый тип возврата
  , asLoopDepth :: !Int                           -- ^ Глубина вложенности циклов
  , asErrorCount :: !Int                          -- ^ Количество ошибок
  , asWarningCount :: !Int                        -- ^ Количество предупреждений
  } deriving (Eq, Show)

-- | Результат анализа
data AnalysisResult = AnalysisResult
  { arHIRProgram :: !HIRProgram                   -- ^ Результирующая HIR программа
  , arDiagnostics :: ![Diagnostic]                -- ^ Диагностические сообщения
  , arSymbolTable :: !SymbolTable                 -- ^ Финальная таблица символов
  , arSuccess :: !Bool                            -- ^ Успешность анализа
  } deriving (Eq, Show)

-- ============================================================================
-- ТАБЛИЦА СИМВОЛОВ
-- ============================================================================

-- | Таблица символов с поддержкой областей видимости
data SymbolTable = SymbolTable
  { stScopes :: ![Scope]                          -- ^ Стек областей видимости
  , stGlobalScope :: !Scope                       -- ^ Глобальная область видимости
  , stBuiltins :: !Scope                          -- ^ Встроенные символы
  } deriving (Eq, Show)

-- | Область видимости
data Scope = Scope
  { scopeLevel :: !ScopeLevel                     -- ^ Уровень области видимости
  , scopeSymbols :: !(Map Text Symbol)            -- ^ Символы в области видимости
  , scopeParent :: !(Maybe ScopeLevel)            -- ^ Родительская область видимости
  } deriving (Eq, Show)

-- | Уровень области видимости
data ScopeLevel
  = GlobalScope                                   -- ^ Глобальная область видимости
  | FunctionScope !Identifier                     -- ^ Область видимости функции
  | BlockScope !Int                               -- ^ Область видимости блока
  | ParameterScope !Identifier                    -- ^ Область видимости параметров
  deriving (Eq, Show, Ord)

-- | Символ в таблице символов
data Symbol = Symbol
  { symName :: !Identifier                        -- ^ Имя символа
  , symType :: !SymbolType                        -- ^ Тип символа
  , symHIRType :: !HIRType                        -- ^ HIR тип
  , symScope :: !ScopeLevel                       -- ^ Область видимости
  , symPosition :: !SourcePos                     -- ^ Позиция определения
  , symUsed :: !Bool                              -- ^ Использован ли символ
  , symMutable :: !Bool                           -- ^ Изменяемый ли символ
  } deriving (Eq, Show)

-- | Тип символа
data SymbolType
  = VariableSymbol !HIRType                       -- ^ Переменная
  | FunctionSymbol ![HIRType] !HIRType            -- ^ Функция (параметры, возврат)
  | TypeSymbol !HIRType                           -- ^ Определение типа
  | ConstantSymbol !HIRLiteral                    -- ^ Константа
  | LabelSymbol                                   -- ^ Метка
  deriving (Eq, Show)

-- ============================================================================
-- ПРОВЕРКА ТИПОВ
-- ============================================================================

-- | Монада проверки типов
type TypeChecker = ReaderT TypeEnvironment (ExceptT SemanticError SemanticAnalyzer)

-- | Окружение типов
data TypeEnvironment = TypeEnvironment
  { teTypes :: !(Map Text HIRType)                -- ^ Определения типов
  , teConstraints :: ![TypeConstraint]            -- ^ Ограничения типов
  , teCurrentType :: !(Maybe HIRType)             -- ^ Текущий ожидаемый тип
  } deriving (Eq, Show)

-- | Ограничение типов
data TypeConstraint
  = EqualityConstraint !HIRType !HIRType !SourcePos -- ^ Ограничение равенства типов
  | SubtypeConstraint !HIRType !HIRType !SourcePos  -- ^ Ограничение подтипа
  | SizeConstraint !HIRType !Int !SourcePos         -- ^ Ограничение размера
  deriving (Eq, Show)

-- ============================================================================
-- ОШИБКИ И ПРЕДУПРЕЖДЕНИЯ
-- ============================================================================

-- | Семантическая ошибка
data SemanticError
  = UndefinedSymbol !Identifier !SourcePos
  | RedefinedSymbol !Identifier !SourcePos !SourcePos
  | TypeMismatch !HIRType !HIRType !SourcePos
  | InvalidOperation !Text !HIRType !SourcePos
  | InvalidAssignment !HIRType !HIRType !SourcePos
  | InvalidFunctionCall !Identifier ![HIRType] ![HIRType] !SourcePos
  | InvalidReturnType !HIRType !HIRType !SourcePos
  | InvalidBreakContinue !SourcePos
  | InvalidArrayAccess !HIRType !SourcePos
  | InvalidPointerOperation !HIRType !SourcePos
  | ArchitecturalConstraintViolation !Text !SourcePos
  deriving (Eq, Show)

-- | Семантическое предупреждение
data SemanticWarning
  = UnusedVariable !Identifier !SourcePos
  | UnusedFunction !Identifier !SourcePos
  | ImplicitConversion !HIRType !HIRType !SourcePos
  | PossibleNullDereference !SourcePos
  | UnreachableCode !SourcePos
  | DeprecatedFeature !Text !SourcePos
  deriving (Eq, Show)

-- | Контекст диагностики
data DiagnosticContext = DiagnosticContext
  { dcFunction :: !(Maybe Identifier)             -- ^ Текущая функция
  , dcScope :: !ScopeLevel                        -- ^ Текущая область видимости
  , dcExpectedType :: !(Maybe HIRType)            -- ^ Ожидаемый тип
  } deriving (Eq, Show)

-- ============================================================================
-- ОСНОВНЫЕ ФУНКЦИИ АНАЛИЗА
-- ============================================================================

-- | Запуск семантического анализа
runSemanticAnalysis :: Program -> IO (Either CompilerError HIRProgram, [Diagnostic])
runSemanticAnalysis program = do
  let context = defaultAnalysisContext
      initialState = defaultAnalysisState
  
  result <- runExceptT $ runWriterT $ runStateT (runReaderT (analyzeProgram program) context) initialState
  
  case result of
    Left err -> return (Left err, [])
    Right ((hirProgram, _), diagnostics) -> return (Right hirProgram, diagnostics)

-- | Анализ программы
analyzeProgram :: Program -> SemanticAnalyzer HIRProgram
analyzeProgram (Program decls pos) = do
  -- Инициализируем встроенные символы
  initializeBuiltins
  
  -- Первый проход: собираем декларации функций и глобальных переменных
  mapM_ collectDeclaration decls
  
  -- Второй проход: анализируем тела функций
  hirDecls <- mapM analyzeTopLevelDecl decls
  
  -- Проверяем неиспользуемые символы
  checkUnusedSymbols
  
  return $ HIRProgram (catMaybes hirDecls) pos

-- | Сбор деклараций (первый проход)
collectDeclaration :: TopLevelDecl -> SemanticAnalyzer ()
collectDeclaration = \case
  TLFunction funcDef -> collectFunctionDeclaration funcDef
  TLDeclaration decl -> collectVariableDeclaration decl
  TLBitDecl bitDecl -> collectBitDeclaration bitDecl
  TLSfrDecl sfrDecl -> collectSfrDeclaration sfrDecl

-- | Анализ декларации верхнего уровня
analyzeTopLevelDecl :: TopLevelDecl -> SemanticAnalyzer (Maybe HIRDeclaration)
analyzeTopLevelDecl = \case
  TLFunction funcDef -> Just <$> analyzeFunctionDef funcDef
  TLDeclaration decl -> Just <$> analyzeVariableDecl decl
  TLBitDecl bitDecl -> Just <$> analyzeBitDecl bitDecl
  TLSfrDecl sfrDecl -> Just <$> analyzeSfrDecl sfrDecl

-- | Анализ функции
analyzeFunctionDef :: FunctionDef -> SemanticAnalyzer HIRDeclaration
analyzeFunctionDef FunctionDef{..} = do
  -- Преобразуем тип возврата
  hirReturnType <- convertTypeSpec funcReturnType
  
  -- Входим в область видимости функции
  enterFunctionScope funcName
  modify $ \s -> s { asCurrentFunction = Just funcName, asReturnType = Just hirReturnType }
  
  -- Анализируем параметры
  hirParams <- mapM analyzeParameter funcParams
  
  -- Анализируем тело функции
  hirBody <- analyzeCompoundStmt funcBody
  
  -- Выходим из области видимости
  exitScope
  modify $ \s -> s { asCurrentFunction = Nothing, asReturnType = Nothing }
  
  return $ HIRFunctionDecl $ HIRFunction
    { hirFuncName = funcName
    , hirFuncReturnType = hirReturnType
    , hirFuncParams = hirParams
    , hirFuncBody = hirBody
    , hirFuncPos = funcPos
    }

-- | Анализ выражения
analyzeExpression :: Expression -> SemanticAnalyzer HIRExpression
analyzeExpression expr = case expr of
  Expr (ELiteral lit) pos -> do
    hirLit <- convertLiteral lit
    return $ HIRExpr (HIRLiteralExpr hirLit) (inferLiteralType hirLit) pos
  
  Expr (EVariable ident) pos -> do
    symbol <- lookupSymbolOrError ident pos
    case symType symbol of
      VariableSymbol varType -> return $ HIRExpr (HIRVariableExpr ident) varType pos
      ConstantSymbol constLit -> return $ HIRExpr (HIRLiteralExpr constLit) (inferLiteralType constLit) pos
      _ -> throwSemanticError $ InvalidOperation "Использование не-переменной как переменной" (symHIRType symbol) pos
  
  Expr (EBinary op left right) pos -> analyzeBinaryExpr op left right pos
  Expr (EUnary op operand) pos -> analyzeUnaryExpr op operand pos
  Expr (EAssign assignOp left right) pos -> analyzeAssignExpr assignOp left right pos
  Expr (ECall func args) pos -> analyzeCallExpr func args pos
  Expr (EArrayAccess array index) pos -> analyzeArrayAccessExpr array index pos
  Expr (EMemberAccess struct member) pos -> analyzeMemberAccessExpr struct member pos
  Expr (EPointerAccess ptr member) pos -> analyzePointerAccessExpr ptr member pos
  Expr (ECast targetType expr) pos -> analyzeCastExpr targetType expr pos
  Expr (ESizeOf typeOrExpr) pos -> analyzeSizeOfExpr typeOrExpr pos

-- | Анализ оператора
analyzeStatement :: Stmt -> SemanticAnalyzer HIRStatement
analyzeStatement stmt = case stmt of
  Stmt (SExpression expr) pos -> HIRExpressionStmt <$> analyzeExpression expr <*> pure pos
  Stmt (SCompound compound) pos -> HIRCompoundStmt <$> analyzeCompoundStmt compound <*> pure pos
  Stmt (SIf cond thenStmt elseStmt) pos -> analyzeIfStmt cond thenStmt elseStmt pos
  Stmt (SWhile cond body) pos -> analyzeWhileStmt cond body pos
  Stmt (SFor init cond update body) pos -> analyzeForStmt init cond update body pos
  Stmt (SReturn expr) pos -> analyzeReturnStmt expr pos
  Stmt (SBreak) pos -> analyzeBreakStmt pos
  Stmt (SContinue) pos -> analyzeContinueStmt pos
  Stmt (SGoto label) pos -> analyzeGotoStmt label pos
  Stmt (SLabel label stmt) pos -> analyzeLabelStmt label stmt pos
  Stmt (SDecl decl) pos -> HIRDeclarationStmt <$> analyzeVariableDecl decl <*> pure pos

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Контекст анализа по умолчанию
defaultAnalysisContext :: AnalysisContext
defaultAnalysisContext = AnalysisContext
  { acABIConfig = defaultABIConfig
  , acTargetArch = "AT89S4051"
  , acStrictMode = True
  , acWarningsAsErrors = False
  , acOptimizationLevel = 2
  }

-- | Состояние анализа по умолчанию
defaultAnalysisState :: AnalysisState
defaultAnalysisState = AnalysisState
  { asSymbolTable = emptySymbolTable
  , asTypeEnvironment = emptyTypeEnvironment
  , asCurrentScope = GlobalScope
  , asCurrentFunction = Nothing
  , asReturnType = Nothing
  , asLoopDepth = 0
  , asErrorCount = 0
  , asWarningCount = 0
  }

-- | Пустая таблица символов
emptySymbolTable :: SymbolTable
emptySymbolTable = SymbolTable [] globalScope builtinScope
  where
    globalScope = Scope GlobalScope Map.empty Nothing
    builtinScope = Scope GlobalScope Map.empty Nothing

-- | Пустое окружение типов
emptyTypeEnvironment :: TypeEnvironment
emptyTypeEnvironment = TypeEnvironment Map.empty [] Nothing

-- | Поиск символа
lookupSymbol :: Identifier -> SemanticAnalyzer (Maybe Symbol)
lookupSymbol ident = do
  symbolTable <- gets asSymbolTable
  return $ findSymbolInTable ident symbolTable

-- | Поиск символа с ошибкой если не найден
lookupSymbolOrError :: Identifier -> SourcePos -> SemanticAnalyzer Symbol
lookupSymbolOrError ident pos = do
  maybeSymbol <- lookupSymbol ident
  case maybeSymbol of
    Just symbol -> return symbol
    Nothing -> throwSemanticError $ UndefinedSymbol ident pos

-- | Вставка символа
insertSymbol :: Symbol -> SemanticAnalyzer ()
insertSymbol symbol = do
  -- Проверяем на переопределение в текущей области видимости
  existing <- lookupSymbolInCurrentScope (symName symbol)
  case existing of
    Just existingSymbol -> 
      throwSemanticError $ RedefinedSymbol (symName symbol) (symPosition symbol) (symPosition existingSymbol)
    Nothing -> do
      modify $ \s -> s { asSymbolTable = insertSymbolInTable symbol (asSymbolTable s) }

-- | Вход в область видимости
enterScope :: ScopeLevel -> SemanticAnalyzer ()
enterScope level = do
  currentScope <- gets asCurrentScope
  let newScope = Scope level Map.empty (Just currentScope)
  modify $ \s -> s 
    { asSymbolTable = (asSymbolTable s) { stScopes = newScope : stScopes (asSymbolTable s) }
    , asCurrentScope = level
    }

-- | Вход в область видимости функции
enterFunctionScope :: Identifier -> SemanticAnalyzer ()
enterFunctionScope funcName = enterScope (FunctionScope funcName)

-- | Выход из области видимости
exitScope :: SemanticAnalyzer ()
exitScope = do
  scopes <- gets (stScopes . asSymbolTable)
  case scopes of
    [] -> return () -- Уже в глобальной области видимости
    (scope:restScopes) -> do
      let parentScope = fromMaybe GlobalScope (scopeParent scope)
      modify $ \s -> s 
        { asSymbolTable = (asSymbolTable s) { stScopes = restScopes }
        , asCurrentScope = parentScope
        }

-- | Разрешение типа
resolveType :: TypeSpec -> SemanticAnalyzer HIRType
resolveType = convertTypeSpec

-- | Генерация семантической ошибки
throwSemanticError :: SemanticError -> SemanticAnalyzer a
throwSemanticError err = do
  modify $ \s -> s { asErrorCount = asErrorCount s + 1 }
  let diagnostic = semanticErrorToDiagnostic err
  tell [diagnostic]
  throwError $ SemanticAnalysisError (T.pack $ show err)

-- | Генерация предупреждения
emitWarning :: SemanticWarning -> SemanticAnalyzer ()
emitWarning warning = do
  modify $ \s -> s { asWarningCount = asWarningCount s + 1 }
  let diagnostic = semanticWarningToDiagnostic warning
  tell [diagnostic]

-- Заглушки для функций, которые будут реализованы далее
initializeBuiltins :: SemanticAnalyzer ()
initializeBuiltins = return ()

collectFunctionDeclaration :: FunctionDef -> SemanticAnalyzer ()
collectFunctionDeclaration = undefined

collectVariableDeclaration :: Declaration -> SemanticAnalyzer ()
collectVariableDeclaration = undefined

collectBitDeclaration :: BitDeclaration -> SemanticAnalyzer ()
collectBitDeclaration = undefined

collectSfrDeclaration :: SfrDeclaration -> SemanticAnalyzer ()
collectSfrDeclaration = undefined

analyzeVariableDecl :: Declaration -> SemanticAnalyzer HIRDeclaration
analyzeVariableDecl = undefined

analyzeBitDecl :: BitDeclaration -> SemanticAnalyzer HIRDeclaration
analyzeBitDecl = undefined

analyzeSfrDecl :: SfrDeclaration -> SemanticAnalyzer HIRDeclaration
analyzeSfrDecl = undefined

analyzeParameter :: Parameter -> SemanticAnalyzer HIRVariable
analyzeParameter = undefined

analyzeCompoundStmt :: CompoundStmt -> SemanticAnalyzer HIRBlock
analyzeCompoundStmt = undefined

convertTypeSpec :: TypeSpec -> SemanticAnalyzer HIRType
convertTypeSpec = undefined

convertLiteral :: Literal -> SemanticAnalyzer HIRLiteral
convertLiteral = undefined

inferLiteralType :: HIRLiteral -> HIRType
inferLiteralType = undefined

analyzeBinaryExpr :: BinaryOp -> Expression -> Expression -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeBinaryExpr = undefined

analyzeUnaryExpr :: UnaryOp -> Expression -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeUnaryExpr = undefined

analyzeAssignExpr :: AssignOp -> Expression -> Expression -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeAssignExpr = undefined

analyzeCallExpr :: Expression -> [Expression] -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeCallExpr = undefined

analyzeArrayAccessExpr :: Expression -> Expression -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeArrayAccessExpr = undefined

analyzeMemberAccessExpr :: Expression -> Identifier -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeMemberAccessExpr = undefined

analyzePointerAccessExpr :: Expression -> Identifier -> SourcePos -> SemanticAnalyzer HIRExpression
analyzePointerAccessExpr = undefined

analyzeCastExpr :: TypeSpec -> Expression -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeCastExpr = undefined

analyzeSizeOfExpr :: Either TypeSpec Expression -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeSizeOfExpr = undefined

analyzeIfStmt :: Expression -> Stmt -> Maybe Stmt -> SourcePos -> SemanticAnalyzer HIRStatement
analyzeIfStmt = undefined

analyzeWhileStmt :: Expression -> Stmt -> SourcePos -> SemanticAnalyzer HIRStatement
analyzeWhileStmt = undefined

analyzeForStmt :: Maybe Expression -> Maybe Expression -> Maybe Expression -> Stmt -> SourcePos -> SemanticAnalyzer HIRStatement
analyzeForStmt = undefined

analyzeReturnStmt :: Maybe Expression -> SourcePos -> SemanticAnalyzer HIRStatement
analyzeReturnStmt = undefined

analyzeBreakStmt :: SourcePos -> SemanticAnalyzer HIRStatement
analyzeBreakStmt = undefined

analyzeContinueStmt :: SourcePos -> SemanticAnalyzer HIRStatement
analyzeContinueStmt = undefined

analyzeGotoStmt :: Identifier -> SourcePos -> SemanticAnalyzer HIRStatement
analyzeGotoStmt = undefined

analyzeLabelStmt :: Identifier -> Stmt -> SourcePos -> SemanticAnalyzer HIRStatement
analyzeLabelStmt = undefined

checkUnusedSymbols :: SemanticAnalyzer ()
checkUnusedSymbols = return ()

findSymbolInTable :: Identifier -> SymbolTable -> Maybe Symbol
findSymbolInTable ident (SymbolTable scopes) = 
  listToMaybe $ catMaybes $ map (Map.lookup ident . scopeSymbols) scopes

lookupSymbolInCurrentScope :: Identifier -> SemanticAnalyzer (Maybe Symbol)
lookupSymbolInCurrentScope ident = do
  symbolTable <- gets asSymbolTable
  case stScopes symbolTable of
    [] -> return Nothing
    (currentScope:_) -> return $ Map.lookup ident (scopeSymbols currentScope)

insertSymbolInTable :: Symbol -> SymbolTable -> SymbolTable
insertSymbolInTable symbol (SymbolTable scopes) = 
  case scopes of
    [] -> SymbolTable [Scope GlobalScope (Map.singleton (symName symbol) symbol) Nothing]
    (currentScope:restScopes) -> 
      let newCurrentScope = currentScope { scopeSymbols = Map.insert (symName symbol) symbol (scopeSymbols currentScope) }
      in SymbolTable (newCurrentScope:restScopes)

semanticErrorToDiagnostic :: SemanticError -> Diagnostic
semanticErrorToDiagnostic = \case
  TypeMismatch expected actual pos ->
    Diagnostic ErrorLevel pos $ 
      "Несоответствие типов: ожидался " <> T.pack (show expected) <> 
      ", получен " <> T.pack (show actual)
  UndefinedSymbol ident pos ->
    Diagnostic ErrorLevel pos $ 
      "Неопределённый символ: " <> identifierText ident
  RedefinedSymbol ident pos1 pos2 ->
    Diagnostic ErrorLevel pos1 $ 
      "Повторное определение символа: " <> identifierText ident <> 
      " (первое определение в " <> T.pack (show pos2) <> ")"
  NotAFunction ident pos ->
    Diagnostic ErrorLevel pos $ 
      "Символ не является функцией: " <> identifierText ident
  WrongArgumentCount expected actual pos ->
    Diagnostic ErrorLevel pos $ 
      "Неверное количество аргументов: ожидалось " <> T.pack (show expected) <> 
      ", получено " <> T.pack (show actual)
  InvalidOperation op leftType rightType pos ->
    Diagnostic ErrorLevel pos $ 
      "Недопустимая операция " <> T.pack (show op) <> 
      " для типов " <> T.pack (show leftType) <> " и " <> T.pack (show rightType)
  InvalidUnaryOperation op exprType pos ->
    Diagnostic ErrorLevel pos $ 
      "Недопустимая унарная операция " <> T.pack (show op) <> 
      " для типа " <> T.pack (show exprType)
  NotLValue pos ->
    Diagnostic ErrorLevel pos "Выражение не является lvalue"
  InvalidArrayIndex indexType pos ->
    Diagnostic ErrorLevel pos $ 
      "Недопустимый тип индекса массива: " <> T.pack (show indexType)
  NotAnArray arrayType pos ->
    Diagnostic ErrorLevel pos $ 
      "Выражение не является массивом: " <> T.pack (show arrayType)
  UndefinedMember member pos ->
    Diagnostic ErrorLevel pos $ 
      "Неопределённый член структуры: " <> identifierText member
  NotAStruct structType pos ->
    Diagnostic ErrorLevel pos $ 
      "Выражение не является структурой: " <> T.pack (show structType)
  NotAPointerToStruct ptrType pos ->
    Diagnostic ErrorLevel pos $ 
      "Выражение не является указателем на структуру: " <> T.pack (show ptrType)
  InvalidCast fromType toType pos ->
    Diagnostic ErrorLevel pos $ 
      "Недопустимое приведение типа от " <> T.pack (show fromType) <> 
      " к " <> T.pack (show toType)
  InvalidCondition condType pos ->
    Diagnostic ErrorLevel pos $ 
      "Недопустимый тип условия: " <> T.pack (show condType)

semanticWarningToDiagnostic :: SemanticWarning -> Diagnostic
semanticWarningToDiagnostic = \case
  UnusedVariable ident pos ->
    Diagnostic WarningLevel pos $ 
      "Неиспользуемая переменная: " <> identifierText ident
  UnreachableCode pos ->
    Diagnostic WarningLevel pos "Недостижимый код"
  ImplicitConversion fromType toType pos ->
    Diagnostic WarningLevel pos $ 
      "Неявное преобразование типа от " <> T.pack (show fromType) <> 
      " к " <> T.pack (show toType)

unifyTypes :: HIRType -> HIRType -> TypeChecker HIRType
unifyTypes t1 t2
  | t1 == t2 = return t1
  | otherwise = case (t1, t2) of
      (HIRIntType w1 s1, HIRIntType w2 s2) -> 
        return $ HIRIntType (max w1 w2) (s1 && s2)
      (HIRFloatType w1, HIRFloatType w2) -> 
        return $ HIRFloatType (max w1 w2)
      (HIRIntType _ _, HIRFloatType w) -> 
        return $ HIRFloatType w
      (HIRFloatType w, HIRIntType _ _) -> 
        return $ HIRFloatType w
      _ -> throwError $ TypeUnificationError t1 t2

checkTypeCompatibility :: HIRType -> HIRType -> TypeChecker Bool
checkTypeCompatibility t1 t2 = do
  result <- runExceptT $ unifyTypes t1 t2
  case result of
    Left _ -> return False
    Right _ -> return True

analyzeVariableDecl :: Declaration -> SemanticAnalyzer HIRDeclaration
analyzeVariableDecl (VarDecl typeSpec name maybeInit pos) = do
  hirType <- convertTypeSpec typeSpec
  
  hirInit <- case maybeInit of
    Just initExpr -> do
      hirExpr <- analyzeExpression initExpr
      exprType <- inferExpressionType hirExpr
      compatible <- liftTypeChecker $ checkTypeCompatibility hirType exprType
      unless compatible $ 
        throwSemanticError $ TypeMismatch hirType exprType pos
      return $ Just hirExpr
    Nothing -> return Nothing
  
  return $ HIRVariableDecl hirType name hirInit pos

analyzeBitDecl :: BitDeclaration -> SemanticAnalyzer HIRDeclaration
analyzeBitDecl (BitDecl name bitAddr pos) = 
  return $ HIRBitDecl name bitAddr pos

analyzeSfrDecl :: SfrDeclaration -> SemanticAnalyzer HIRDeclaration
analyzeSfrDecl (SfrDecl name addr pos) = 
  return $ HIRSfrDecl name addr pos

analyzeParameter :: Parameter -> SemanticAnalyzer HIRVariable
analyzeParameter (Parameter typeSpec name pos) = do
  hirType <- convertTypeSpec typeSpec
  return $ HIRVariable hirType name pos

analyzeCompoundStmt :: CompoundStmt -> SemanticAnalyzer HIRBlock
analyzeCompoundStmt (CompoundStmt decls stmts pos) = do
  -- Входим в новую область видимости
  enterScope BlockScope
  
  -- Анализируем объявления
  hirDecls <- mapM analyzeDeclaration decls
  
  -- Анализируем операторы
  hirStmts <- mapM analyzeStatement stmts
  
  -- Выходим из области видимости
  exitScope
  
  return $ HIRBlock hirDecls hirStmts pos

analyzeBinaryExpr :: BinaryOp -> Expression -> Expression -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeBinaryExpr op leftExpr rightExpr pos = do
  leftHir <- analyzeExpression leftExpr
  rightHir <- analyzeExpression rightExpr
  
  leftType <- inferExpressionType leftHir
  rightType <- inferExpressionType rightHir
  
  -- Проверяем совместимость типов для операции
  resultType <- case op of
    AddOp | isNumericType leftType && isNumericType rightType -> 
      liftTypeChecker $ unifyTypes leftType rightType
    SubOp | isNumericType leftType && isNumericType rightType -> 
      liftTypeChecker $ unifyTypes leftType rightType
    MulOp | isNumericType leftType && isNumericType rightType -> 
      liftTypeChecker $ unifyTypes leftType rightType
    DivOp | isNumericType leftType && isNumericType rightType -> 
      liftTypeChecker $ unifyTypes leftType rightType
    ModOp | isIntegerType leftType && isIntegerType rightType -> 
      liftTypeChecker $ unifyTypes leftType rightType
    EqOp | typesCompatible leftType rightType -> 
      return $ HIRIntType 8 False  -- bool
    NeOp | typesCompatible leftType rightType -> 
      return $ HIRIntType 8 False
    LtOp | isNumericType leftType && isNumericType rightType -> 
      return $ HIRIntType 8 False
    LeOp | isNumericType leftType && isNumericType rightType -> 
      return $ HIRIntType 8 False
    GtOp | isNumericType leftType && isNumericType rightType -> 
      return $ HIRIntType 8 False
    GeOp | isNumericType leftType && isNumericType rightType -> 
      return $ HIRIntType 8 False
    AndOp | isIntegerType leftType && isIntegerType rightType -> 
      liftTypeChecker $ unifyTypes leftType rightType
    OrOp | isIntegerType leftType && isIntegerType rightType -> 
      liftTypeChecker $ unifyTypes leftType rightType
    XorOp | isIntegerType leftType && isIntegerType rightType -> 
      liftTypeChecker $ unifyTypes leftType rightType
    _ -> throwSemanticError $ InvalidOperation op leftType rightType pos
  
  return $ HIRBinaryExpr op leftHir rightHir resultType pos
  where
    isNumericType (HIRIntType _ _) = True
    isNumericType (HIRFloatType _) = True
    isNumericType _ = False
    
    isIntegerType (HIRIntType _ _) = True
    isIntegerType _ = False
    
    typesCompatible t1 t2 = runTypeChecker (checkTypeCompatibility t1 t2) == Right True

analyzeUnaryExpr :: UnaryOp -> Expression -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeUnaryExpr op expr pos = do
  hirExpr <- analyzeExpression expr
  exprType <- inferExpressionType hirExpr
  
  resultType <- case op of
    NegOp | isNumericType exprType -> return exprType
    NotOp | isIntegerType exprType -> return exprType
    AddrOp -> return $ HIRPointerType exprType
    DerefOp | HIRPointerType innerType <- exprType -> return innerType
    _ -> throwSemanticError $ InvalidUnaryOperation op exprType pos
  
  return $ HIRUnaryExpr op hirExpr resultType pos
  where
    isNumericType (HIRIntType _ _) = True
    isNumericType (HIRFloatType _) = True
    isNumericType _ = False
    
    isIntegerType (HIRIntType _ _) = True
    isIntegerType _ = False

analyzeAssignExpr :: AssignOp -> Expression -> Expression -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeAssignExpr assignOp lvalueExpr rvalueExpr pos = do
  lvalueHir <- analyzeExpression lvalueExpr
  rvalueHir <- analyzeExpression rvalueExpr
  
  lvalueType <- inferExpressionType lvalueHir
  rvalueType <- inferExpressionType rvalueHir
  
  -- Проверяем, что lvalue может быть изменено
  unless (isLValue lvalueHir) $
    throwSemanticError $ NotLValue pos
  
  -- Проверяем совместимость типов
  compatible <- liftTypeChecker $ checkTypeCompatibility lvalueType rvalueType
  unless compatible $
    throwSemanticError $ TypeMismatch lvalueType rvalueType pos
  
  return $ HIRAssignExpr assignOp lvalueHir rvalueHir lvalueType pos
  where
    isLValue (HIRVariableExpr _ _ _) = True
    isLValue (HIRArrayAccessExpr _ _ _ _) = True
    isLValue (HIRMemberAccessExpr _ _ _ _) = True
    isLValue (HIRPointerAccessExpr _ _ _ _) = True
    isLValue (HIRUnaryExpr DerefOp _ _ _) = True
    isLValue _ = False

analyzeCallExpr :: Expression -> [Expression] -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeCallExpr funcExpr args pos = do
  funcHir <- analyzeExpression funcExpr
  funcType <- inferExpressionType funcHir
  
  case funcType of
    HIRFunctionType retType paramTypes -> do
      -- Проверяем количество аргументов
      unless (length args == length paramTypes) $
        throwSemanticError $ WrongArgumentCount (length paramTypes) (length args) pos
      
      -- Анализируем аргументы и проверяем типы
      hirArgs <- mapM analyzeExpression args
      argTypes <- mapM inferExpressionType hirArgs
      
      zipWithM_ checkArgument argTypes paramTypes
      
      return $ HIRCallExpr funcHir hirArgs retType pos
    _ -> throwSemanticError $ NotAFunction (Identifier "unknown") pos
  where
    checkArgument argType paramType = do
      compatible <- liftTypeChecker $ checkTypeCompatibility paramType argType
      unless compatible $
        throwSemanticError $ TypeMismatch paramType argType pos

analyzeArrayAccessExpr :: Expression -> Expression -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeArrayAccessExpr arrayExpr indexExpr pos = do
  arrayHir <- analyzeExpression arrayExpr
  indexHir <- analyzeExpression indexExpr
  
  arrayType <- inferExpressionType arrayHir
  indexType <- inferExpressionType indexHir
  
  -- Проверяем, что индекс - целое число
  unless (isIntegerType indexType) $
    throwSemanticError $ InvalidArrayIndex indexType pos
  
  -- Определяем тип элемента
  elementType <- case arrayType of
    HIRArrayType elemType _ -> return elemType
    HIRPointerType elemType -> return elemType
    _ -> throwSemanticError $ NotAnArray arrayType pos
  
  return $ HIRArrayAccessExpr arrayHir indexHir elementType pos
  where
    isIntegerType (HIRIntType _ _) = True
    isIntegerType _ = False

analyzeMemberAccessExpr :: Expression -> Identifier -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeMemberAccessExpr structExpr memberName pos = do
  structHir <- analyzeExpression structExpr
  structType <- inferExpressionType structHir
  
  memberType <- case structType of
    HIRStructType _ fields -> 
      case lookup memberName fields of
        Just fieldType -> return fieldType
        Nothing -> throwSemanticError $ UndefinedMember memberName pos
    _ -> throwSemanticError $ NotAStruct structType pos
  
  return $ HIRMemberAccessExpr structHir memberName memberType pos

analyzePointerAccessExpr :: Expression -> Identifier -> SourcePos -> SemanticAnalyzer HIRExpression
analyzePointerAccessExpr ptrExpr memberName pos = do
  ptrHir <- analyzeExpression ptrExpr
  ptrType <- inferExpressionType ptrHir
  
  memberType <- case ptrType of
    HIRPointerType (HIRStructType _ fields) ->
      case lookup memberName fields of
        Just fieldType -> return fieldType
        Nothing -> throwSemanticError $ UndefinedMember memberName pos
    _ -> throwSemanticError $ NotAPointerToStruct ptrType pos
  
  return $ HIRPointerAccessExpr ptrHir memberName memberType pos

analyzeCastExpr :: TypeSpec -> Expression -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeCastExpr targetTypeSpec expr pos = do
  hirExpr <- analyzeExpression expr
  exprType <- inferExpressionType hirExpr
  targetType <- convertTypeSpec targetTypeSpec
  
  -- Проверяем возможность приведения
  unless (canCast exprType targetType) $
    throwSemanticError $ InvalidCast exprType targetType pos
  
  return $ HIRCastExpr targetType hirExpr pos
  where
    canCast (HIRIntType _ _) (HIRIntType _ _) = True
    canCast (HIRIntType _ _) (HIRFloatType _) = True
    canCast (HIRFloatType _) (HIRIntType _ _) = True
    canCast (HIRFloatType _) (HIRFloatType _) = True
    canCast (HIRPointerType _) (HIRPointerType _) = True
    canCast _ _ = False

analyzeSizeOfExpr :: Either TypeSpec Expression -> SourcePos -> SemanticAnalyzer HIRExpression
analyzeSizeOfExpr target pos = do
  hirType <- case target of
    Left typeSpec -> convertTypeSpec typeSpec
    Right expr -> do
      hirExpr <- analyzeExpression expr
      inferExpressionType hirExpr
  
  let size = calculateTypeSize hirType
  return $ HIRLiteralExpr (HIRIntLiteral size) (HIRIntType 16 False) pos
  where
    calculateTypeSize (HIRIntType 8 _) = 1
    calculateTypeSize (HIRIntType 16 _) = 2
    calculateTypeSize (HIRIntType 32 _) = 4
    calculateTypeSize (HIRFloatType 32) = 4
    calculateTypeSize (HIRFloatType 64) = 8
    calculateTypeSize (HIRPointerType _) = 2  -- 16-bit указатели для 8051
    calculateTypeSize (HIRArrayType elemType count) = 
      calculateTypeSize elemType * count
    calculateTypeSize _ = 1  -- По умолчанию

analyzeIfStmt :: Expression -> Stmt -> Maybe Stmt -> SourcePos -> SemanticAnalyzer HIRStatement
analyzeIfStmt condExpr thenStmt maybeElseStmt pos = do
  condHir <- analyzeExpression condExpr
  condType <- inferExpressionType condHir
  
  -- Проверяем, что условие имеет скалярный тип
  unless (isScalarType condType) $
    throwSemanticError $ InvalidCondition condType pos
  
  thenHir <- analyzeStatement thenStmt
  elseHir <- case maybeElseStmt of
    Just elseStmt -> Just <$> analyzeStatement elseStmt
    Nothing -> return Nothing
  
  return $ HIRIfStmt condHir thenHir elseHir pos
  where
    isScalarType (HIRIntType _ _) = True
    isScalarType (HIRFloatType _) = True
    isScalarType (HIRPointerType _) = True
    isScalarType _ = False

analyzeWhileStmt :: Expression -> Stmt -> SourcePos -> SemanticAnalyzer HIRStatement
analyzeWhileStmt condExpr bodyStmt pos = do
  condHir <- analyzeExpression condExpr
  condType <- inferExpressionType condHir
  
  unless (isScalarType condType) $
    throwSemanticError $ InvalidCondition condType pos
  
  bodyHir <- analyzeStatement bodyStmt
  return $ HIRWhileStmt condHir bodyHir pos
  where
    isScalarType (HIRIntType _ _) = True
    isScalarType (HIRFloatType _) = True
    isScalarType (HIRPointerType _) = True
    isScalarType _ = False

analyzeForStmt :: Maybe Expression -> Maybe Expression -> Maybe Expression -> Stmt -> SourcePos -> SemanticAnalyzer HIRStatement
analyzeForStmt maybeInit maybeCond maybeUpdate bodyStmt pos = do
  initHir <- case maybeInit of
    Just initExpr -> Just <$> analyzeExpression initExpr
    Nothing -> return Nothing
  
  condHir <- case maybeCond of
    Just condExpr -> do
      hir <- analyzeExpression condExpr
      condType <- inferExpressionType hir
      unless (isScalarType condType) $
        throwSemanticError $ InvalidCondition condType pos
      return $ Just hir
    Nothing -> return Nothing
  
  updateHir <- case maybeUpdate of
    Just updateExpr -> Just <$> analyzeExpression updateExpr
    Nothing -> return Nothing
  
  bodyHir <- analyzeStatement bodyStmt
  return $ HIRForStmt initHir condHir updateHir bodyHir pos
  where
    isScalarType (HIRIntType _ _) = True
    isScalarType (HIRFloatType _) = True
    isScalarType (HIRPointerType _) = True
    isScalarType _ = False

analyzeReturnStmt :: Maybe Expression -> SourcePos -> SemanticAnalyzer HIRStatement
analyzeReturnStmt maybeExpr pos = do
  hirExpr <- case maybeExpr of
    Just expr -> Just <$> analyzeExpression expr
    Nothing -> return Nothing
  
  -- TODO: Проверить совместимость с типом возвращаемого значения функции
  return $ HIRReturnStmt hirExpr pos

analyzeBreakStmt :: SourcePos -> SemanticAnalyzer HIRStatement
analyzeBreakStmt pos = return $ HIRBreakStmt pos

analyzeContinueStmt :: SourcePos -> SemanticAnalyzer HIRStatement
analyzeContinueStmt pos = return $ HIRContinueStmt pos

analyzeGotoStmt :: Identifier -> SourcePos -> SemanticAnalyzer HIRStatement
analyzeGotoStmt label pos = return $ HIRGotoStmt label pos

analyzeLabelStmt :: Identifier -> Stmt -> SourcePos -> SemanticAnalyzer HIRStatement
analyzeLabelStmt label stmt pos = do
  hirStmt <- analyzeStatement stmt
  return $ HIRLabelStmt label hirStmt pos

-- | Вспомогательная функция для работы с TypeChecker
liftTypeChecker :: TypeChecker a -> SemanticAnalyzer a
liftTypeChecker action = do
  result <- liftIO $ runExceptT action
  case result of
    Left err -> throwSemanticError $ TypeMismatch HIRVoidType HIRVoidType (SourcePos "" 0 0)  -- Упрощение
    Right value -> return value

-- | Запуск TypeChecker
runTypeChecker :: TypeChecker a -> Either TypeCheckerError a
runTypeChecker = runIdentity . runExceptT 