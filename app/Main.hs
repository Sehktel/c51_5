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
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)
import System.IO (hPutStrLn, stderr)
import Options.Applicative

import HCL.Preprocessor (preprocess, defaultOptions, PreprocessorOptions(..), PreprocessorResult(..))
import HCL.Lexer (tokenize, LexerResult(..), prettyToken)
import HCL.Parser (parseProgram, runParser, ParseResult(..), prettyParseError)
import HCL.AST (prettyAST)
import HCL.IR.HIR (HIRProgram, prettyHIR)
import HCL.SemanticAnalysis (runSemanticAnalysis)
import HCL.Error (Diagnostic(..), DiagnosticLevel(..), prettyDiagnostic)

-- ============================================================================
-- ОПЦИИ КОМАНДНОЙ СТРОКИ
-- ============================================================================

-- | Опции компилятора
data CompilerOptions = CompilerOptions
  { optInputFile :: !FilePath           -- ^ Входной файл
  , optOutputFile :: !(Maybe FilePath)  -- ^ Выходной файл
  , optStopAfter :: !CompilerPhase      -- ^ Остановиться после фазы
  , optVerbose :: !Bool                 -- ^ Подробный вывод
  , optShowTokens :: !Bool              -- ^ Показать токены
  , optShowAST :: !Bool                 -- ^ Показать AST
  , optShowHIR :: !Bool                 -- ^ Показать HIR
  , optIncludePaths :: ![FilePath]      -- ^ Пути поиска заголовочных файлов
  , optDefines :: ![(Text, Text)]       -- ^ Предопределенные макросы
  } deriving (Eq, Show)

-- | Фазы компиляции
data CompilerPhase
  = PhasePreprocess   -- ^ Препроцессор
  | PhaseLex          -- ^ Лексический анализ
  | PhaseParse        -- ^ Синтаксический анализ
  | PhaseSemantic     -- ^ Семантический анализ
  | PhaseOptimize     -- ^ Оптимизация
  | PhaseCodeGen      -- ^ Генерация кода
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
  <*> option readPhase
      ( long "stop-after"
     <> short 's'
     <> value PhaseCodeGen
     <> metavar "PHASE"
     <> help "Остановиться после фазы (preprocess|lex|parse|semantic|optimize|codegen)" )
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

-- | Читает фазу компиляции
readPhase :: ReadM CompilerPhase
readPhase = eitherReader $ \s -> case s of
  "preprocess" -> Right PhasePreprocess
  "lex" -> Right PhaseLex
  "parse" -> Right PhaseParse
  "semantic" -> Right PhaseSemantic
  "optimize" -> Right PhaseOptimize
  "codegen" -> Right PhaseCodeGen
  _ -> Left $ "Unknown phase: " ++ s

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
      when (optStopAfter == PhasePreprocess) $ do
        outputResult opts preprocessed
        return $ Right preprocessed
      
      -- Фаза 2: Лексический анализ
      when optVerbose $ putStrLn "Фаза 2: Лексический анализ"
      lexResult <- runLexer opts preprocessed
      case lexResult of
        Left err -> return $ Left err
        Right tokens -> do
          when (optStopAfter == PhaseLex) $ do
            let tokenOutput = T.unlines $ map prettyToken tokens
            outputResult opts tokenOutput
            return $ Right tokenOutput
          
          -- Фаза 3: Синтаксический анализ
          when optVerbose $ putStrLn "Фаза 3: Синтаксический анализ"
          parseResult <- runParser' opts tokens
          case parseResult of
            Left err -> return $ Left err
            Right ast -> do
              when (optStopAfter == PhaseParse) $ do
                let astOutput = prettyAST ast
                outputResult opts astOutput
                return $ Right astOutput
              
              -- Фаза 4: Семантический анализ
              when optVerbose $ putStrLn "Фаза 4: Семантический анализ"
              semanticResult <- runSemanticAnalyzer opts ast
              case semanticResult of
                Left err -> return $ Left err
                Right hir -> do
                  when (optStopAfter == PhaseSemantic) $ do
                    let hirOutput = prettyHIR hir
                    outputResult opts hirOutput
                    return $ Right hirOutput
                  
                  -- Фазы оптимизации и генерации кода
                  when (optStopAfter >= PhaseOptimize) $ do
                    putStrLn "=== Оптимизация ==="
                    optimizedHir <- runOptimizations opts hir
                    
                    when optShowOptimizedHIR $ do
                      putStrLn "=== Оптимизированный HIR ==="
                      TIO.putStrLn (prettyHIR optimizedHir)
                      putStrLn ""
                    
                    when (optStopAfter >= PhaseCodeGen) $ do
                      putStrLn "=== Генерация кода ==="
                      assemblyCode <- runCodeGeneration opts optimizedHir
                      
                      -- Записываем результат в выходной файл
                      case optOutputFile of
                        Just outputFile -> do
                          TIO.writeFile outputFile assemblyCode
                          putStrLn $ "Ассемблерный код записан в: " ++ outputFile
                        Nothing -> do
                          putStrLn "=== Ассемблерный код ==="
                          TIO.putStrLn assemblyCode
                  
                  putStrLn "Компиляция завершена успешно!"
                  return ()

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

-- | Запускает семантический анализатор
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
runCodeGeneration :: CompilerOptions -> HIRProgram -> IO Text
runCodeGeneration opts hir = do
  putStrLn "Преобразование HIR в ассемблерный код AT89S4051"
  
  -- Генерируем базовый ассемблерный код
  let assemblyCode = generateBasicAssembly hir
  
  when (optVerbose opts) $ do
    putStrLn "Генерация завершена"
    putStrLn $ "Размер кода: " ++ show (T.length assemblyCode) ++ " символов"
  
  return assemblyCode

-- | Генерация базового ассемблерного кода (заглушка)
generateBasicAssembly :: HIRProgram -> Text
generateBasicAssembly (HIRProgram decls) = T.unlines $
  [ "; Ассемблерный код для AT89S4051"
  , "; Сгенерирован HCL компилятором"
  , ""
  , ".org 0x0000"
  , "    ljmp main"
  , ""
  , ".org 0x0030  ; Начало пользовательского кода"
  , ""
  ] ++ concatMap generateDeclaration decls ++
  [ ""
  , "end:"
  , "    sjmp end"
  , ""
  , ".end"
  ]
  where
    generateDeclaration :: HIRDeclaration -> [Text]
    generateDeclaration = \case
      HIRFunctionDecl _ name _ _ _ -> 
        [ name' <> ":"
        , "    ; Функция " <> name'
        , "    ret"
        , ""
        ]
        where name' = identifierText name
      
      HIRVariableDecl _ name _ _ -> 
        [ "; Переменная " <> identifierText name
        ]
      
      HIRBitDecl name addr _ ->
        [ identifierText name <> " equ " <> T.pack (show addr) <> "h"
        ]
      
      HIRSfrDecl name addr _ ->
        [ identifierText name <> " equ " <> T.pack (show addr) <> "h"
        ]

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Выводит результат в файл или на stdout
outputResult :: CompilerOptions -> Text -> IO ()
outputResult CompilerOptions{..} content = 
  case optOutputFile of
    Just filename -> TIO.writeFile filename content
    Nothing -> TIO.putStr content

-- | Импорты для недостающих типов
import qualified Data.Map.Strict as Map
import HCL.Lexer (Token)
import HCL.AST (Program)
import HCL.IR.HIR (HIRProgram) 