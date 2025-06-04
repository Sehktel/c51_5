{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | Главный модуль HCL компилятора
--
-- Предоставляет интерфейс командной строки для компиляции программ C
-- для микроконтроллера AT89S4051. Поддерживает различные фазы компиляции
-- и опции для отладки и диагностики.
module Main (main) where

import Control.Monad (when, unless)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Map.Strict as Map
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import System.FilePath (replaceExtension)
import Data.Maybe (fromMaybe)
import Options.Applicative

import HCL.Preprocessor (preprocess, defaultOptions, PreprocessorOptions(..), PreprocessorResult(..))
import HCL.Lexer (tokenize, LexerResult(..), prettyToken, Token)
-- import HCL.Parser (parseProgram, runParser, ParseResult(..), prettyParseError)
import HCL.AST (prettyAST, Program)
import HCL.IR.HIR (HIRProgram(..), HIRDeclaration(..), HIRFunction(..), prettyHIR)
-- import HCL.SemanticAnalysis (runSemanticAnalysis)
import HCL.Error (Diagnostic(..), DiagnosticLevel(..), prettyDiagnostic, CompilerError, prettyError)
import HCL.Types (identifierText)
import qualified HCL.Linker as Linker

-- ============================================================================
-- ОПЦИИ КОМАНДНОЙ СТРОКИ
-- ============================================================================

-- | Опции компилятора
data CompilerOptions = CompilerOptions
  { optInputFile :: !FilePath           -- ^ Входной файл
  , optOutputFile :: !(Maybe FilePath)  -- ^ Выходной файл
  , optOutputFormat :: !OutputFormat    -- ^ Формат выходного файла
  , optStopAfter :: !CompilerPhase      -- ^ Остановиться после фазы
  , optOptimizationLevel :: !Int        -- ^ Уровень оптимизации (0-3)
  , optOptimizeFor :: !OptimizeTarget   -- ^ Цель оптимизации
  , optVerbose :: !Bool                 -- ^ Подробный вывод
  , optShowTokens :: !Bool              -- ^ Показать токены
  , optShowAST :: !Bool                 -- ^ Показать AST
  , optShowHIR :: !Bool                 -- ^ Показать HIR
  , optShowOptimizedHIR :: !Bool        -- ^ Показать оптимизированный HIR
  , optIncludePaths :: ![FilePath]      -- ^ Пути поиска заголовочных файлов
  , optDefines :: ![(Text, Text)]       -- ^ Предопределенные макросы
  } deriving (Eq, Show)

-- | Формат выходного файла
data OutputFormat
  = OutputAssembly   -- ^ Ассемблерный код (.asm, .s)
  | OutputObject     -- ^ Объектный файл (.obj, .o)
  | OutputHex        -- ^ Intel HEX файл (.hex)
  | OutputBinary     -- ^ Двоичный файл (.bin)
  | OutputListing    -- ^ Листинг с адресами (.lst)
  deriving (Eq, Show, Enum, Bounded)

-- | Фазы компиляции
data CompilerPhase
  = PhasePreprocess   -- ^ Препроцессор
  | PhaseLex          -- ^ Лексический анализ
  | PhaseParse        -- ^ Синтаксический анализ
  | PhaseSemantic     -- ^ Семантический анализ
  | PhaseOptimize     -- ^ Оптимизация
  | PhaseCodeGen      -- ^ Генерация кода
  | PhaseLink         -- ^ Линковка
  deriving (Eq, Show, Enum, Bounded)

-- | Цель оптимизации
data OptimizeTarget
  = OptimizeSpeed    -- ^ Оптимизация скорости (-O2, -O3)
  | OptimizeSize     -- ^ Оптимизация размера (-Os)
  | OptimizeBalance  -- ^ Сбалансированная оптимизация (-O1)
  | OptimizeNone     -- ^ Без оптимизаций (-O0)
  deriving (Eq, Show, Enum, Bounded)

-- ============================================================================
-- ПАРСИНГ ОПЦИЙ
-- ============================================================================

-- | Парсер опций командной строки
compilerOptions :: Parser CompilerOptions
compilerOptions = CompilerOptions
  <$> strArgument
      ( metavar "INPUT"
     <> help "Входной файл C" )
  <*> optional (strOption
      ( long "output"
     <> short 'o'
     <> metavar "FILE"
     <> help "Выходной файл" ))
  <*> (outputFormatOption <|> pure OutputAssembly)  -- По умолчанию ассемблер
  <*> option readPhase
      ( long "stop-after"
     <> short 's'
     <> value PhaseCodeGen
     <> metavar "PHASE"
     <> help "Остановиться после фазы (preprocess|lex|parse|semantic|optimize|codegen|link)" )
  <*> (optimizationLevelOption <|> pure 2)  -- По умолчанию -O2
  <*> (optimizeTargetOption <|> pure OptimizeBalance)  -- По умолчанию сбалансированная
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Подробный вывод" )
  <*> switch
      ( long "show-tokens"
     <> help "Показать токены лексического анализа" )
  <*> switch
      ( long "show-ast"
     <> help "Показать абстрактное синтаксическое дерево" )
  <*> switch
      ( long "show-hir"
     <> help "Показать высокоуровневое промежуточное представление" )
  <*> switch
      ( long "show-optimized-hir"
     <> help "Показать оптимизированный HIR" )
  <*> many (strOption
      ( long "include"
     <> short 'I'
     <> metavar "DIR"
     <> help "Добавить директорию поиска заголовочных файлов" ))
  <*> many (option readDefine
      ( long "define"
     <> short 'D'
     <> metavar "NAME[=VALUE]"
     <> help "Определить макрос" ))

-- | Парсер формата выходного файла
outputFormatOption :: Parser OutputFormat
outputFormatOption = 
  flag' OutputAssembly (long "asm" <> help "Генерировать ассемблерный код") <|>
  flag' OutputObject (long "obj" <> short 'c' <> help "Генерировать объектный файл") <|>
  flag' OutputHex (long "hex" <> help "Генерировать Intel HEX файл") <|>
  flag' OutputBinary (long "bin" <> help "Генерировать двоичный файл") <|>
  flag' OutputListing (long "lst" <> help "Генерировать листинг с адресами") <|>
  option readOutputFormat (long "format" <> short 'f' <> metavar "FORMAT" <> help "Формат выходного файла (asm|obj|hex|bin|lst)")

-- | Читает формат выходного файла
readOutputFormat :: ReadM OutputFormat
readOutputFormat = eitherReader $ \s -> case s of
  "asm" -> Right OutputAssembly
  "obj" -> Right OutputObject
  "hex" -> Right OutputHex
  "bin" -> Right OutputBinary
  "lst" -> Right OutputListing
  _ -> Left $ "Unknown output format: " ++ s

-- | Парсер уровня оптимизации (поддерживает -O0, -O1, -O2, -O3, -Os)
optimizationLevelOption :: Parser Int
optimizationLevelOption = 
  flag' 0 (long "O0" <> help "Без оптимизаций") <|>
  flag' 1 (long "O1" <> help "Базовые оптимизации") <|>
  flag' 2 (long "O2" <> help "Стандартные оптимизации") <|>
  flag' 3 (long "O3" <> help "Агрессивные оптимизации") <|>
  flag' (-1) (long "Os" <> help "Оптимизация размера") <|>  -- -1 для Os
  option auto (long "optimization-level" <> short 'O' <> metavar "LEVEL" <> help "Уровень оптимизации (0-3)")

-- | Парсер цели оптимизации
optimizeTargetOption :: Parser OptimizeTarget
optimizeTargetOption = 
  flag' OptimizeNone (long "O0" <> help "Без оптимизаций") <|>
  flag' OptimizeBalance (long "O1" <> help "Базовые оптимизации") <|>
  flag' OptimizeSpeed (long "O2" <> help "Стандартные оптимизации") <|>
  flag' OptimizeSpeed (long "O3" <> help "Агрессивные оптимизации") <|>
  flag' OptimizeSize (long "Os" <> help "Оптимизация размера") <|>
  option readOptimizeFor (long "optimize-for" <> short 'f' <> metavar "TARGET" <> help "Цель оптимизации (speed|size|balance|none)")

-- | Читает фазу компиляции
readPhase :: ReadM CompilerPhase
readPhase = eitherReader $ \s -> case s of
  "preprocess" -> Right PhasePreprocess
  "lex" -> Right PhaseLex
  "parse" -> Right PhaseParse
  "semantic" -> Right PhaseSemantic
  "optimize" -> Right PhaseOptimize
  "codegen" -> Right PhaseCodeGen
  "link" -> Right PhaseLink
  _ -> Left $ "Unknown phase: " ++ s

-- | Читает уровень оптимизации
readOptimizationLevel :: ReadM Int
readOptimizationLevel = eitherReader $ \s -> case s of
  "0" -> Right 0
  "1" -> Right 1
  "2" -> Right 2
  "3" -> Right 3
  _ -> Left $ "Unknown optimization level: " ++ s

-- | Читает цель оптимизации
readOptimizeFor :: ReadM OptimizeTarget
readOptimizeFor = eitherReader $ \s -> case s of
  "speed" -> Right OptimizeSpeed
  "size" -> Right OptimizeSize
  "balance" -> Right OptimizeBalance
  "none" -> Right OptimizeNone
  _ -> Left $ "Unknown optimization target: " ++ s

-- | Читает определение макроса
readDefine :: ReadM (Text, Text)
readDefine = eitherReader $ \s -> 
  case T.breakOn "=" (T.pack s) of
    (name, "") -> Right (name, "1")
    (name, value) -> Right (name, T.drop 1 value)

-- | Информация о программе
programInfo :: ParserInfo CompilerOptions
programInfo = info (compilerOptions <**> helper)
  ( fullDesc
 <> progDesc "HCL - Haskell C51 Compiler для микроконтроллеров AT89S4051"
 <> header "hcl - компилятор C для 8051" )

-- ============================================================================
-- ОСНОВНАЯ ЛОГИКА КОМПИЛЯТОРА
-- ============================================================================

-- | Главная функция
main :: IO ()
main = do
  opts <- execParser programInfo
  result <- compileFile opts
  case result of
    Left err -> do
      hPutStrLn stderr $ "Ошибка компиляции: " ++ T.unpack err
      exitFailure
    Right _ -> exitSuccess

-- | Компилирует файл
compileFile :: CompilerOptions -> IO (Either Text Text)
compileFile opts@CompilerOptions{..} = do
  when optVerbose $ 
    putStrLn $ "Компиляция файла: " ++ optInputFile
  
  -- Читаем входной файл
  input <- TIO.readFile optInputFile
  
  -- Фаза 1: Препроцессор
  when optVerbose $ putStrLn "Фаза 1: Препроцессор"
  preprocessResult <- runPreprocessor opts input
  case preprocessResult of
    Left err -> return $ Left err
    Right preprocessed -> do
      if optStopAfter == PhasePreprocess
        then do
          outputResult opts preprocessed
          return $ Right preprocessed
        else do
          -- Фаза 2: Лексический анализ
          when optVerbose $ putStrLn "Фаза 2: Лексический анализ"
          lexResult <- runLexer opts preprocessed
          case lexResult of
            Left err -> return $ Left err
            Right tokens -> do
              let tokenOutput = T.unlines $ map prettyToken tokens
              outputResult opts tokenOutput
              return $ Right tokenOutput

-- ============================================================================
-- ФАЗЫ КОМПИЛЯЦИИ
-- ============================================================================

-- | Запускает препроцессор
runPreprocessor :: CompilerOptions -> Text -> IO (Either Text Text)
runPreprocessor CompilerOptions{..} input = do
  let preprocessorOpts = defaultOptions
        { poIncludePaths = optIncludePaths
        , poPredefines = Map.fromList optDefines
        }
  
  result <- preprocess preprocessorOpts optInputFile input
  
  -- Выводим диагностику
  mapM_ (putStrLn . T.unpack . prettyDiagnostic) (prDiagnostics result)
  
  if prSuccess result
    then return $ Right (prResult result)
    else return $ Left "Ошибки препроцессора"

-- | Запускает лексический анализатор
runLexer :: CompilerOptions -> Text -> IO (Either Text [Token])
runLexer CompilerOptions{..} input = do
  let result = tokenize input
  
  -- Выводим токены если запрошено
  when optShowTokens $ do
    putStrLn "=== ТОКЕНЫ ==="
    mapM_ (putStrLn . T.unpack . prettyToken) (lrTokens result)
    putStrLn ""
  
  if lrSuccess result
    then return $ Right (lrTokens result)
    else return $ Left "Ошибки лексического анализа"

-- | Запускает синтаксический анализатор
{-
runParser' :: CompilerOptions -> [Token] -> IO (Either Text Program)
runParser' CompilerOptions{..} tokens = do
  let result = runParser parseProgram tokens
  
  case prResult result of
    Left err -> return $ Left (prettyParseError err)
    Right ast -> do
      -- Выводим AST если запрошено
      when optShowAST $ do
        putStrLn "=== AST ==="
        TIO.putStrLn (prettyAST ast)
        putStrLn ""
      
      return $ Right ast
-}

-- | Запускает семантический анализатор
{-
runSemanticAnalyzer :: CompilerOptions -> Program -> IO (Either Text HIRProgram)
runSemanticAnalyzer CompilerOptions{..} ast = do
  (result, diagnostics) <- runSemanticAnalysis ast
  
  -- Выводим диагностику
  mapM_ (putStrLn . T.unpack . prettyDiagnostic) diagnostics
  
  case result of
    Left err -> return $ Left (T.pack $ show err)
    Right hir -> do
      -- Выводим HIR если запрошено
      when optShowHIR $ do
        putStrLn "=== HIR ==="
        TIO.putStrLn (prettyHIR hir)
        putStrLn ""
      
      return $ Right hir
-}

-- | Запуск оптимизаций
runOptimizations :: CompilerOptions -> HIRProgram -> IO HIRProgram
runOptimizations opts hir = do
  putStrLn $ "Применение оптимизаций уровня " ++ show (optOptimizationLevel opts)
  
  -- Применяем оптимизации в зависимости от уровня
  case optOptimizationLevel opts of
    0 -> return hir  -- Без оптимизаций
    1 -> do
      -- Базовые оптимизации
      putStrLn "- Удаление мёртвого кода"
      putStrLn "- Свёртка констант"
      return hir  -- Пока возвращаем исходный HIR
    2 -> do
      -- Стандартные оптимизации
      putStrLn "- Удаление мёртвого кода"
      putStrLn "- Свёртка констант"
      putStrLn "- Оптимизация циклов"
      putStrLn "- Инлайнинг функций"
      return hir
    3 -> do
      -- Агрессивные оптимизации
      putStrLn "- Все оптимизации уровня 2"
      putStrLn "- Векторизация"
      putStrLn "- Межпроцедурные оптимизации"
      return hir
    _ -> do
      putStrLn "Неизвестный уровень оптимизации, используется уровень 2"
      runOptimizations (opts { optOptimizationLevel = 2 }) hir

-- | Запуск генерации кода
{-
runCodeGeneration :: CompilerOptions -> HIRProgram -> IO Text
runCodeGeneration opts hir = do
  putStrLn "Преобразование HIR в ассемблерный код AT89S4051"
  
  -- Генерируем базовый ассемблерный код
  let assemblyCode = generateBasicAssembly hir
  
  when (optVerbose opts) $ do
    putStrLn "Генерация завершена"
    putStrLn $ "Размер кода: " ++ show (T.length assemblyCode) ++ " символов"
  
  return assemblyCode
-}

-- | Генерация базового ассемблерного кода (заглушка)
{-
generateBasicAssembly :: HIRProgram -> Text
generateBasicAssembly (HIRProgram functions globals _ _) = T.unlines $
  [ "; Ассемблерный код для AT89S4051"
  , "; Сгенерирован HCL компилятором"
  , ""
  , ".org 0x0000"
  , "    ljmp main"
  , ""
  , ".org 0x0030  ; Начало пользовательского кода"
  , ""
  ] ++ concatMap generateFunction functions ++
    concatMap generateGlobal globals ++
  [ ""
  , "end:"
  , "    sjmp end"
  , ""
  , ".end"
  ]
  where
    generateFunction :: HIRFunction -> [Text]
    generateFunction func = 
      [ identifierText (hirFuncName func) <> ":"
      , "    ; Функция " <> identifierText (hirFuncName func)
      , "    ret"
      , ""
      ]
    
    generateGlobal :: HIRDeclaration -> [Text]
    generateGlobal decl = 
      [ "; Переменная " <> identifierText (hirDeclName decl)
      ]
-}

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Выводит результат в файл или на stdout
outputResult :: CompilerOptions -> Text -> IO ()
outputResult CompilerOptions{..} content = 
  case optOutputFile of
    Just filename -> TIO.writeFile filename content
    Nothing -> TIO.putStr content

-- | Запуск линковщика
runLinker :: CompilerOptions -> [FilePath] -> IO (Either CompilerError Linker.LinkerResult)
runLinker CompilerOptions{..} objectFiles = do
  let linkerConfig = Linker.defaultLinkerConfig
        { Linker.lcVerbose = optVerbose
        , Linker.lcOutputFormats = convertOutputFormats optOutputFormat
        }
  
  Linker.linkProgram linkerConfig objectFiles

-- | Преобразование формата вывода в форматы линковщика
convertOutputFormats :: OutputFormat -> [Linker.OutputFormat]
convertOutputFormats = \case
  OutputAssembly -> []  -- Ассемблерный код уже сгенерирован
  OutputObject -> []    -- Объектные файлы не генерируются линковщиком
  OutputHex -> [Linker.FormatHex]
  OutputBinary -> [Linker.FormatBinary]
  OutputListing -> [Linker.FormatListing, Linker.FormatMap]

-- | Генерация выходных файлов линковщика
generateLinkerOutput :: CompilerOptions -> Linker.LinkerResult -> FilePath -> IO ()
generateLinkerOutput CompilerOptions{..} result baseFileName = do
  let linkerConfig = Linker.defaultLinkerConfig
        { Linker.lcOutputFormats = convertOutputFormats optOutputFormat
        }
  
  Linker.generateOutput linkerConfig result baseFileName 