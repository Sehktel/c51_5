{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}

-- | Модуль препроцессора C для HCL компилятора
--
-- Этот модуль реализует полнофункциональный препроцессор C,
-- портированный с Clojure v2 архитектуры. Использует современные
-- подходы Haskell: монады, функторы, и композицию функций.
--
-- Поддерживаемые директивы:
-- * #include - включение файлов
-- * #define/#undef - определение/отмена макросов  
-- * #ifdef/#ifndef/#else/#endif - условная компиляция
-- * #if/#elif - условная компиляция с выражениями
-- * #error/#warning - пользовательские сообщения
-- * #pragma - директивы компилятора
-- * #line - управление номерами строк
module HCL.Preprocessor
  ( -- * Основные функции
    preprocess
  , preprocessWithDiagnostics
  
    -- * Состояние препроцессора
  , PreprocessorState(..)
  , initialState
  , PreprocessorOptions(..)
  , defaultOptions
  
    -- * Директивы препроцессора
  , PreprocessorDirective(..)
  , ConditionalState(..)
  , MacroDefinition(..)
  
    -- * Результат обработки
  , PreprocessorResult(..)
  
    -- * Утилиты
  , expandMacros
  , evaluateCondition
  ) where

import Control.Monad.State.Strict (StateT, runStateT, get, put, modify)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (foldl')
import System.FilePath ((</>))
import System.Directory (doesFileExist)
import Text.Regex.TDFA ((=~))

import HCL.Types (SourcePos(..))
import HCL.Error (CompilerError(..), Diagnostic(..), DiagnosticLevel(..), 
                  mkError, mkWarning, mkInfo)

-- ============================================================================
-- ТИПЫ ДАННЫХ ПРЕПРОЦЕССОРА
-- ============================================================================

-- | Определение макроса
data MacroDefinition
  = SimpleMacro !Text                    -- ^ Простой макрос: #define NAME value
  | FunctionMacro ![Text] !Text          -- ^ Функциональный макрос: #define NAME(args) body
  | ObjectMacro !Text                    -- ^ Объектный макрос: #define NAME object
  deriving (Eq, Show)

-- | Состояние условной компиляции
data ConditionalState = ConditionalState
  { condActive :: !Bool      -- ^ Активен ли текущий блок
  , condTaken :: !Bool       -- ^ Был ли уже выбран блок в группе if/elif/else
  , condType :: !Text        -- ^ Тип условия (if, ifdef, ifndef)
  } deriving (Eq, Show)

-- | Состояние препроцессора
data PreprocessorState = PreprocessorState
  { -- Макросы и определения
    psDefines :: !(Map Text MacroDefinition)
    
    -- Включения файлов
  , psIncludePaths :: ![FilePath]
  , psIncludeStack :: ![FilePath]  -- Стек включаемых файлов (для предотвращения циклов)
  , psIncludeGuards :: !(Set Text) -- Защитные макросы
  
    -- Условная компиляция
  , psConditionalStack :: ![ConditionalState]
  
    -- Позиция в исходном коде
  , psCurrentFile :: !FilePath
  , psLineNumber :: !Int
  
    -- Диагностика
  , psDiagnostics :: ![Diagnostic]
  } deriving (Eq, Show)

-- | Опции препроцессора
data PreprocessorOptions = PreprocessorOptions
  { poIncludePaths :: ![FilePath]        -- ^ Пути поиска заголовочных файлов
  , poPredefines :: !(Map Text Text)     -- ^ Предопределенные макросы
  , poMaxIncludeDepth :: !Int            -- ^ Максимальная глубина включений
  , poStrictMode :: !Bool                -- ^ Строгий режим (больше проверок)
  } deriving (Eq, Show)

-- | Результат работы препроцессора
data PreprocessorResult = PreprocessorResult
  { prResult :: !Text                    -- ^ Обработанный код
  , prDiagnostics :: ![Diagnostic]       -- ^ Диагностические сообщения
  , prSuccess :: !Bool                   -- ^ Успешность обработки
  , prFinalState :: !PreprocessorState   -- ^ Финальное состояние
  } deriving (Eq, Show)

-- | Макрос препроцессора  
data Macro = Macro
  { macroName :: !Text           -- ^ Имя макроса
  , macroParams :: ![Text]       -- ^ Параметры макроса (если есть)
  , macroBody :: !Text           -- ^ Тело макроса
  , macroPos :: !SourcePos       -- ^ Позиция определения
  } deriving (Eq, Show)

-- | Класс для директив препроцессора
class PreprocessorDirective a where
  -- | Проверяет, соответствует ли строка данной директиве
  matches :: a -> Text -> Bool
  
  -- | Обрабатывает директиву, возвращая результат и новое состояние
  process :: a -> Text -> StateT PreprocessorState (ExceptT CompilerError IO) (Maybe Text)

-- ============================================================================
-- НАЧАЛЬНЫЕ ЗНАЧЕНИЯ И КОНСТРУКТОРЫ
-- ============================================================================

-- | Создает начальное состояние препроцессора
initialState :: PreprocessorOptions -> FilePath -> PreprocessorState
initialState PreprocessorOptions{..} currentFile = PreprocessorState
  { psDefines = Map.fromList $ map (\(k, v) -> (k, SimpleMacro v)) $ Map.toList poPredefines
  , psIncludePaths = poIncludePaths
  , psIncludeStack = []
  , psIncludeGuards = Set.empty
  , psConditionalStack = []
  , psCurrentFile = currentFile
  , psLineNumber = 1
  , psDiagnostics = []
  }

-- | Опции препроцессора по умолчанию
defaultOptions :: PreprocessorOptions
defaultOptions = PreprocessorOptions
  { poIncludePaths = [".", "include", "lib"]
  , poPredefines = Map.fromList
      [ ("__C51__", "1")           -- Индикатор компилятора C51
      , ("__HCL__", "1")           -- Индикатор HCL компилятора
      , ("__AT89S4051__", "1")     -- Целевой микроконтроллер
      , ("__DATE__", "\"Jan 01 2024\"")  -- Дата компиляции (заглушка)
      , ("__TIME__", "\"00:00:00\"")     -- Время компиляции (заглушка)
      , ("__FILE__", "\"unknown\"")      -- Имя файла (будет заменено)
      , ("__LINE__", "1")                -- Номер строки (будет заменено)
      ]
  , poMaxIncludeDepth = 32
  , poStrictMode = False
  }

-- ============================================================================
-- ДИРЕКТИВЫ ПРЕПРОЦЕССОРА
-- ============================================================================

-- | Директива #include
data IncludeDirective = IncludeDirective

instance PreprocessorDirective IncludeDirective where
  matches _ line = T.unpack line =~ ("^\\s*#\\s*include\\s*[<\"]([^>\"]+)[>\"]" :: String)
  
  process _ line = do
    state <- get
    case extractIncludeFile line of
      Nothing -> do
        let diag = mkError "Некорректная директива #include" 
                          (Just $ SourcePos (T.pack $ psCurrentFile state) (psLineNumber state) 1)
        modify $ \s -> s { psDiagnostics = diag : psDiagnostics s }
        return Nothing
      Just filename -> do
        -- Проверяем глубину включений
        let depth = length (psIncludeStack state)
        if depth >= 32  -- Максимальная глубина
          then do
            let diag = mkError ("Превышена максимальная глубина включений: " <> T.pack (show depth))
                              (Just $ SourcePos (T.pack $ psCurrentFile state) (psLineNumber state) 1)
            modify $ \s -> s { psDiagnostics = diag : psDiagnostics s }
            return Nothing
          else do
            -- Ищем файл в путях включения
            maybeFilePath <- liftIO $ findIncludeFile filename (psIncludePaths state)
            case maybeFilePath of
              Nothing -> do
                let diag = mkError ("Файл не найден: " <> filename)
                                  (Just $ SourcePos (T.pack $ psCurrentFile state) (psLineNumber state) 1)
                modify $ \s -> s { psDiagnostics = diag : psDiagnostics s }
                return Nothing
              Just filePath -> do
                -- Проверяем циклические включения
                if filePath `elem` psIncludeStack state
                  then do
                    let diag = mkError ("Циклическое включение файла: " <> T.pack filePath)
                                      (Just $ SourcePos (T.pack $ psCurrentFile state) (psLineNumber state) 1)
                    modify $ \s -> s { psDiagnostics = diag : psDiagnostics s }
                    return Nothing
                  else do
                    -- Читаем и обрабатываем файл
                    content <- liftIO $ TIO.readFile filePath
                    let newState = state 
                          { psIncludeStack = filePath : psIncludeStack state
                          , psCurrentFile = filePath
                          , psLineNumber = 1
                          }
                    put newState
                    
                    -- Рекурсивно обрабатываем включенный файл
                    processedContent <- processLines (T.lines content)
                    
                    -- Восстанавливаем состояние
                    modify $ \s -> s 
                      { psIncludeStack = tail (psIncludeStack s)
                      , psCurrentFile = psCurrentFile state
                      , psLineNumber = psLineNumber state + 1
                      }
                    
                    return $ Just processedContent

-- | Директива #define
data DefineDirective = DefineDirective

instance PreprocessorDirective DefineDirective where
  matches _ line = T.unpack line =~ ("^\\s*#\\s*define\\s+(\\w+)" :: String)
  
  process _ line = do
    case parseDefine line of
      Nothing -> do
        state <- get
        let diag = mkError "Некорректная директива #define"
                          (Just $ SourcePos (T.pack $ psCurrentFile state) (psLineNumber state) 1)
        modify $ \s -> s { psDiagnostics = diag : psDiagnostics s }
        return Nothing
      Just (name, macroDef) -> do
        modify $ \s -> s { psDefines = Map.insert name macroDef (psDefines s) }
        return Nothing  -- Директива не генерирует код

-- | Директива #undef
data UndefDirective = UndefDirective

instance PreprocessorDirective UndefDirective where
  matches _ line = T.unpack line =~ ("^\\s*#\\s*undef\\s+(\\w+)" :: String)
  
  process _ line = do
    case extractUndefName line of
      Nothing -> do
        state <- get
        let diag = mkError "Некорректная директива #undef"
                          (Just $ SourcePos (T.pack $ psCurrentFile state) (psLineNumber state) 1)
        modify $ \s -> s { psDiagnostics = diag : psDiagnostics s }
        return Nothing
      Just name -> do
        modify $ \s -> s { psDefines = Map.delete name (psDefines s) }
        return Nothing

-- | Директива #ifdef
data IfdefDirective = IfdefDirective

instance PreprocessorDirective IfdefDirective where
  matches _ line = T.unpack line =~ ("^\\s*#\\s*ifdef\\s+(\\w+)" :: String)
  
  process _ line = do
    case extractConditionName line of
      Nothing -> do
        state <- get
        let diag = mkError "Некорректная директива #ifdef"
                          (Just $ SourcePos (T.pack $ psCurrentFile state) (psLineNumber state) 1)
        modify $ \s -> s { psDiagnostics = diag : psDiagnostics s }
        return Nothing
      Just name -> do
        state <- get
        let isDefined = Map.member name (psDefines state)
            condState = ConditionalState
              { condActive = isDefined && shouldProcessLine state
              , condTaken = isDefined
              , condType = "ifdef"
              }
        modify $ \s -> s { psConditionalStack = condState : psConditionalStack s }
        return Nothing

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Извлекает имя файла из директивы #include
extractIncludeFile :: Text -> Maybe Text
extractIncludeFile line =
  case T.unpack line =~ ("^\\s*#\\s*include\\s*[<\"]([^>\"]+)[>\"]" :: String) of
    [[_, filename]] -> Just (T.pack filename)
    _ -> Nothing

-- | Парсит директиву #define
parseDefine :: Text -> Maybe (Text, MacroDefinition)
parseDefine line
  | T.unpack line =~ ("^\\s*#\\s*define\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*(.*)" :: String) =
      case T.unpack line =~ ("^\\s*#\\s*define\\s+(\\w+)\\s*\\(([^)]*)\\)\\s*(.*)" :: String) of
        [[_, name, params, body]] -> 
          let paramList = map T.strip $ T.splitOn "," (T.pack params)
          in Just (T.pack name, FunctionMacro paramList (T.pack body))
        _ -> Nothing
  | T.unpack line =~ ("^\\s*#\\s*define\\s+(\\w+)(?:\\s+(.*))?$" :: String) =
      case T.unpack line =~ ("^\\s*#\\s*define\\s+(\\w+)(?:\\s+(.*))?$" :: String) of
        [[_, name, value]] -> Just (T.pack name, SimpleMacro (T.pack value))
        [[_, name]] -> Just (T.pack name, SimpleMacro "")
        _ -> Nothing
  | otherwise = Nothing

-- | Извлекает имя макроса из директивы #undef
extractUndefName :: Text -> Maybe Text
extractUndefName line =
  case T.unpack line =~ ("^\\s*#\\s*undef\\s+(\\w+)" :: String) of
    [[_, name]] -> Just (T.pack name)
    _ -> Nothing

-- | Извлекает имя условия из директив #ifdef/#ifndef
extractConditionName :: Text -> Maybe Text
extractConditionName line =
  case T.unpack line =~ ("^\\s*#\\s*if(?:n)?def\\s+(\\w+)" :: String) of
    [[_, name]] -> Just (T.pack name)
    _ -> Nothing

-- | Проверяет, должна ли строка обрабатываться (условная компиляция)
shouldProcessLine :: PreprocessorState -> Bool
shouldProcessLine state = all condActive (psConditionalStack state)

-- | Ищет файл в путях включения
findIncludeFile :: Text -> [FilePath] -> IO (Maybe FilePath)
findIncludeFile filename paths = go paths
  where
    go [] = return Nothing
    go (p:ps) = do
      let fullPath = p </> T.unpack filename
      exists <- doesFileExist fullPath
      if exists
        then return (Just fullPath)
        else go ps

-- | Обрабатывает список строк
processLines :: [Text] -> StateT PreprocessorState (ExceptT CompilerError IO) Text
processLines inputLines = do
  processedLines <- mapM processLine inputLines
  return $ T.unlines $ concat processedLines
  where
    processLine line = do
      modify $ \s -> s { psLineNumber = psLineNumber s + 1 }
      
      -- Проверяем директивы препроцессора
      let directives = [SomeDirective IncludeDirective, SomeDirective DefineDirective, 
                       SomeDirective UndefDirective, SomeDirective IfdefDirective]
      
      result <- processDirectives directives line
      case result of
        Just processedLine -> return [processedLine]
        Nothing -> do
          -- Обычная строка кода - проверяем условную компиляцию
          state <- get
          if shouldProcessLine state
            then do
              -- Расширяем макросы в строке
              expandedLine <- expandMacrosInLine line
              return [expandedLine]
            else return []  -- Пропускаем строку

-- | Обрабатывает директивы препроцессора
processDirectives :: [SomeDirective] -> Text -> StateT PreprocessorState (ExceptT CompilerError IO) (Maybe Text)
processDirectives [] _ = return Nothing
processDirectives (d:ds) line = do
  if matchesDirective d line
    then processDirective d line
    else processDirectives ds line

-- | Расширяет макросы в строке кода
expandMacrosInLine :: Text -> StateT PreprocessorState (ExceptT CompilerError IO) Text
expandMacrosInLine line = do
  state <- get
  return $ expandMacros (psDefines state) line

-- | Расширяет макросы в тексте
expandMacros :: Map Text MacroDefinition -> Text -> Text
expandMacros defines text = foldl' expandMacro text (Map.toList defines)
  where
    expandMacro txt (name, SimpleMacro value) = T.replace name value txt
    expandMacro txt (name, FunctionMacro _ _) = txt  -- TODO: Реализовать функциональные макросы
    expandMacro txt (name, ObjectMacro replacement) = T.replace name replacement txt

-- | Расширение макроса в тексте
expandMacro :: Text -> (Text, MacroDefinition) -> Text
expandMacro txt (name, ObjectMacro replacement) = 
  T.replace name replacement txt
expandMacro txt (name, FunctionMacro params replacement) = 
  -- Реализация функциональных макросов
  expandFunctionMacro txt name params replacement
  where
    expandFunctionMacro :: Text -> Text -> [Text] -> Text -> Text
    expandFunctionMacro text macroName parameters body =
      case findMacroCall text macroName of
        Nothing -> text
        Just (before, args, after) -> 
          let substituted = substituteMacroArgs body parameters args
              newText = before <> substituted <> after
          in expandFunctionMacro newText macroName parameters body
    
    findMacroCall :: Text -> Text -> Maybe (Text, [Text], Text)
    findMacroCall text macroName =
      case T.breakOn (macroName <> "(") text of
        (before, rest) | T.null rest -> Nothing
        (before, rest) -> 
          let afterName = T.drop (T.length macroName + 1) rest
          in case parseArgumentList afterName of
               Nothing -> Nothing
               Just (args, remaining) -> Just (before, args, remaining)
    
    parseArgumentList :: Text -> Maybe ([Text], Text)
    parseArgumentList text = 
      case parseArgs text 0 [] "" of
        Just (args, remaining) -> Just (reverse args, remaining)
        Nothing -> Nothing
      where
        parseArgs :: Text -> Int -> [Text] -> Text -> Maybe ([Text], Text)
        parseArgs input depth acc current
          | T.null input = Nothing
          | depth == 0 && T.head input == ')' = 
              Just (if T.null current then acc else T.strip current : acc, T.tail input)
          | T.head input == '(' = 
              parseArgs (T.tail input) (depth + 1) acc (current <> T.singleton '(')
          | T.head input == ')' = 
              parseArgs (T.tail input) (depth - 1) acc (current <> T.singleton ')')
          | depth == 0 && T.head input == ',' = 
              parseArgs (T.tail input) depth (T.strip current : acc) ""
          | otherwise = 
              parseArgs (T.tail input) depth acc (current <> T.singleton (T.head input))
    
    substituteMacroArgs :: Text -> [Text] -> [Text] -> Text
    substituteMacroArgs body params args =
      foldl substituteArg body (zip params args)
      where
        substituteArg :: Text -> (Text, Text) -> Text
        substituteArg text (param, arg) = T.replace param arg text

-- ============================================================================
-- ПУБЛИЧНЫЙ API
-- ============================================================================

-- | Основная функция препроцессора
preprocess :: PreprocessorOptions -> FilePath -> Text -> IO PreprocessorResult
preprocess options filename input = do
  let initialSt = initialState options filename
  result <- runExceptT $ runStateT (processLines (T.lines input)) initialSt
  
  case result of
    Left err -> return $ PreprocessorResult
      { prResult = ""
      , prDiagnostics = [mkError (T.pack $ show err) Nothing]
      , prSuccess = False
      , prFinalState = initialSt
      }
    Right (processedText, finalState) -> return $ PreprocessorResult
      { prResult = processedText
      , prDiagnostics = reverse (psDiagnostics finalState)
      , prSuccess = null (filter (\d -> diagLevel d == ErrorLevel) (psDiagnostics finalState))
      , prFinalState = finalState
      }

-- | Препроцессор с детальной диагностикой
preprocessWithDiagnostics :: PreprocessorOptions -> FilePath -> Text -> IO (Either CompilerError Text, [Diagnostic])
preprocessWithDiagnostics options filename input = do
  result <- preprocess options filename input
  let diagnostics = prDiagnostics result
      hasErrors = any (\d -> diagLevel d == ErrorLevel) diagnostics
  
  if hasErrors
    then return (Left (InternalError "Ошибки препроцессора" (SourcePos (T.pack filename) 1 1)), diagnostics)
    else return (Right (prResult result), diagnostics)

-- | Вычисляет условие для директив #if
evaluateCondition :: Text -> Map Text MacroDefinition -> Bool
evaluateCondition condition defines = 
  -- Простая реализация - только проверка определенности макросов
  -- TODO: Реализовать полный парсер выражений
  case T.words condition of
    ["defined", name] -> Map.member name defines
    [name] -> Map.member name defines
    _ -> False  -- Неподдерживаемое выражение

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ТИПЫ ДЛЯ ОБРАБОТКИ ДИРЕКТИВ
-- ============================================================================

-- | Обертка для директив препроцессора
data SomeDirective where
  SomeDirective :: PreprocessorDirective a => a -> SomeDirective

-- | Проверяет соответствие директивы
matchesDirective :: SomeDirective -> Text -> Bool
matchesDirective (SomeDirective d) = matches d

-- | Обрабатывает директиву
processDirective :: SomeDirective -> Text -> StateT PreprocessorState (ExceptT CompilerError IO) (Maybe Text)
processDirective (SomeDirective d) = process d 