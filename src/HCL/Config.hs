{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | Модуль конфигурации HCL компилятора
--
-- Предоставляет типы и функции для конфигурации различных компонентов
-- компилятора: оптимизатора, генератора кода, линковщика и других.
-- Поддерживает загрузку конфигурации из файлов и переменных окружения.
module HCL.Config
  ( -- * Основные типы конфигурации
    CompilerConfig(..)
  , OptimizationConfig(..)
  , CodeGenConfig(..)
  , LinkerConfig(..)
  , TargetConfig(..)
  
    -- * Уровни и опции
  , OptimizationLevel(..)
  , TargetArchitecture(..)
  , OutputFormat(..)
  , DebugLevel(..)
  
    -- * Конфигурация по умолчанию
  , defaultCompilerConfig
  , defaultOptimizationConfig
  , defaultCodeGenConfig
  , defaultLinkerConfig
  , defaultTargetConfig
  
    -- * Загрузка и сохранение конфигурации
  , loadConfig
  , saveConfig
  , loadConfigFromFile
  , saveConfigToFile
  
    -- * Утилиты
  , mergeConfigs
  , validateConfig
  , prettyConfig
  ) where

import Control.Monad (when)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Word (Word16, Word32)
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), (.:?), (.!=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Encode.Pretty as AesonPretty
import qualified Data.ByteString.Lazy
import qualified Data.Yaml as Yaml
import System.Environment (lookupEnv)
import System.FilePath (takeExtension)
import GHC.Generics (Generic)

-- ============================================================================
-- ОСНОВНЫЕ ТИПЫ КОНФИГУРАЦИИ
-- ============================================================================

-- | Конфигурация компилятора
data CompilerConfig = CompilerConfig
  { ccOptimization :: !OptimizationConfig      -- ^ Конфигурация оптимизации
  , ccCodeGen :: !CodeGenConfig                -- ^ Конфигурация генерации кода
  , ccLinker :: !LinkerConfig                  -- ^ Конфигурация линковщика
  , ccTarget :: !TargetConfig                  -- ^ Конфигурация целевой платформы
  , ccVerbose :: !Bool                         -- ^ Подробный вывод
  , ccDebugLevel :: !DebugLevel                -- ^ Уровень отладки
  , ccIncludePaths :: ![FilePath]              -- ^ Пути поиска заголовочных файлов
  , ccDefines :: !(Map Text Text)              -- ^ Предопределённые макросы
  , ccOutputDir :: !(Maybe FilePath)           -- ^ Директория вывода
  } deriving (Eq, Show, Generic)

-- | Конфигурация оптимизации
data OptimizationConfig = OptimizationConfig
  { ocLevel :: !OptimizationLevel              -- ^ Уровень оптимизации
  , ocDeadCodeElimination :: !Bool             -- ^ Удаление мёртвого кода
  , ocConstantFolding :: !Bool                 -- ^ Свёртка констант
  , ocLoopOptimization :: !Bool                -- ^ Оптимизация циклов
  , ocInlining :: !Bool                        -- ^ Инлайнинг функций
  , ocPeepholeOptimization :: !Bool            -- ^ Peephole оптимизации
  , ocRegisterAllocation :: !Bool              -- ^ Распределение регистров
  , ocOptimizeForSize :: !Bool                 -- ^ Оптимизация размера
  , ocOptimizeForSpeed :: !Bool                -- ^ Оптимизация скорости
  } deriving (Eq, Show, Generic)

-- | Конфигурация генерации кода
data CodeGenConfig = CodeGenConfig
  { cgcTargetArchitecture :: !TargetArchitecture  -- ^ Целевая архитектура
  , cgcGenerateDebugInfo :: !Bool              -- ^ Генерировать отладочную информацию
  , cgcGenerateComments :: !Bool               -- ^ Генерировать комментарии в коде
  , cgcUseInterrupts :: !Bool                  -- ^ Использовать прерывания
  , cgcStackSize :: !Word16                    -- ^ Размер стека
  , cgcCodeStartAddress :: !Word16             -- ^ Начальный адрес кода
  , cgcDataStartAddress :: !Word16             -- ^ Начальный адрес данных
  , cgcRegisterPrefix :: !Text                 -- ^ Префикс для регистров
  } deriving (Eq, Show, Generic)

-- | Конфигурация линковщика
data LinkerConfig = LinkerConfig
  { lcOutputFormats :: ![OutputFormat]         -- ^ Форматы выходных файлов
  , lcEntryPoint :: !Text                      -- ^ Точка входа
  , lcOptimizeLayout :: !Bool                  -- ^ Оптимизировать размещение
  , lcGenerateMap :: !Bool                     -- ^ Генерировать карту памяти
  , lcStripUnused :: !Bool                     -- ^ Удалять неиспользуемые символы
  , lcCheckOverflow :: !Bool                   -- ^ Проверять переполнение памяти
  , lcLibraryPaths :: ![FilePath]              -- ^ Пути поиска библиотек
  } deriving (Eq, Show, Generic)

-- | Конфигурация целевой платформы
data TargetConfig = TargetConfig
  { tcArchitecture :: !TargetArchitecture      -- ^ Архитектура процессора
  , tcDeviceName :: !Text                      -- ^ Название устройства
  , tcFlashSize :: !Word16                     -- ^ Размер Flash памяти
  , tcRamSize :: !Word16                       -- ^ Размер RAM
  , tcClockFrequency :: !Word32                -- ^ Частота тактирования (Гц)
  , tcVoltage :: !Double                       -- ^ Напряжение питания (В)
  , tcSupportedFeatures :: ![Text]             -- ^ Поддерживаемые функции
  } deriving (Eq, Show, Generic)

-- ============================================================================
-- УРОВНИ И ОПЦИИ
-- ============================================================================

-- | Уровень оптимизации
data OptimizationLevel
  = O0  -- ^ Без оптимизаций
  | O1  -- ^ Базовые оптимизации
  | O2  -- ^ Стандартные оптимизации
  | O3  -- ^ Агрессивные оптимизации
  | Os  -- ^ Оптимизация размера
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | Целевая архитектура
data TargetArchitecture
  = Intel8051     -- ^ Intel 8051
  | AT89S4051     -- ^ Atmel AT89S4051
  | AT89S8252     -- ^ Atmel AT89S8252
  | P89C51        -- ^ Philips P89C51
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | Формат выходного файла
data OutputFormat
  = FormatAsm      -- ^ Ассемблерный код
  | FormatObj      -- ^ Объектный файл
  | FormatHex      -- ^ Intel HEX
  | FormatBin      -- ^ Двоичный файл
  | FormatLst      -- ^ Листинг
  | FormatMap      -- ^ Карта памяти
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | Уровень отладки
data DebugLevel
  = DebugNone      -- ^ Без отладки
  | DebugMinimal   -- ^ Минимальная отладка
  | DebugStandard  -- ^ Стандартная отладка
  | DebugVerbose   -- ^ Подробная отладка
  | DebugTrace     -- ^ Трассировка выполнения
  deriving (Eq, Show, Enum, Bounded, Generic)

-- ============================================================================
-- КОНФИГУРАЦИЯ ПО УМОЛЧАНИЮ
-- ============================================================================

-- | Конфигурация компилятора по умолчанию
defaultCompilerConfig :: CompilerConfig
defaultCompilerConfig = CompilerConfig
  { ccOptimization = defaultOptimizationConfig
  , ccCodeGen = defaultCodeGenConfig
  , ccLinker = defaultLinkerConfig
  , ccTarget = defaultTargetConfig
  , ccVerbose = False
  , ccDebugLevel = DebugNone
  , ccIncludePaths = [".", "include"]
  , ccDefines = Map.fromList [("__HCL__", "1"), ("__8051__", "1")]
  , ccOutputDir = Nothing
  }

-- | Конфигурация оптимизации по умолчанию
defaultOptimizationConfig :: OptimizationConfig
defaultOptimizationConfig = OptimizationConfig
  { ocLevel = O2
  , ocDeadCodeElimination = True
  , ocConstantFolding = True
  , ocLoopOptimization = True
  , ocInlining = True
  , ocPeepholeOptimization = True
  , ocRegisterAllocation = True
  , ocOptimizeForSize = False
  , ocOptimizeForSpeed = True
  }

-- | Конфигурация генерации кода по умолчанию
defaultCodeGenConfig :: CodeGenConfig
defaultCodeGenConfig = CodeGenConfig
  { cgcTargetArchitecture = AT89S4051
  , cgcGenerateDebugInfo = False
  , cgcGenerateComments = True
  , cgcUseInterrupts = True
  , cgcStackSize = 32
  , cgcCodeStartAddress = 0x0000
  , cgcDataStartAddress = 0x0030
  , cgcRegisterPrefix = "R"
  }

-- | Конфигурация линковщика по умолчанию
defaultLinkerConfig :: LinkerConfig
defaultLinkerConfig = LinkerConfig
  { lcOutputFormats = [FormatHex, FormatLst]
  , lcEntryPoint = "main"
  , lcOptimizeLayout = True
  , lcGenerateMap = True
  , lcStripUnused = True
  , lcCheckOverflow = True
  , lcLibraryPaths = ["lib", "libraries"]
  }

-- | Конфигурация целевой платформы по умолчанию (AT89S4051)
defaultTargetConfig :: TargetConfig
defaultTargetConfig = TargetConfig
  { tcArchitecture = AT89S4051
  , tcDeviceName = "AT89S4051"
  , tcFlashSize = 2048      -- 2KB Flash
  , tcRamSize = 128         -- 128 байт RAM
  , tcClockFrequency = 12000000  -- 12 МГц
  , tcVoltage = 5.0         -- 5В
  , tcSupportedFeatures = ["timers", "uart", "interrupts", "pwm"]
  }

-- ============================================================================
-- ЗАГРУЗКА И СОХРАНЕНИЕ КОНФИГУРАЦИИ
-- ============================================================================

-- | Загрузка конфигурации из переменных окружения
loadConfig :: IO CompilerConfig
loadConfig = do
  baseConfig <- return defaultCompilerConfig
  
  -- Загружаем переменные окружения
  verbose <- lookupEnv "HCL_VERBOSE"
  debugLevel <- lookupEnv "HCL_DEBUG_LEVEL"
  outputDir <- lookupEnv "HCL_OUTPUT_DIR"
  
  return baseConfig
    { ccVerbose = maybe False (== "1") verbose
    , ccDebugLevel = maybe DebugNone parseDebugLevel debugLevel
    , ccOutputDir = outputDir
    }

-- | Сохранение конфигурации (заглушка)
saveConfig :: CompilerConfig -> IO ()
saveConfig _config = do
  putStrLn "Сохранение конфигурации пока не реализовано"

-- | Загрузка конфигурации из файла
loadConfigFromFile :: FilePath -> IO (Either String CompilerConfig)
loadConfigFromFile filePath = do
  case takeExtension filePath of
    ".yaml" -> loadYamlConfig filePath
    ".yml"  -> loadYamlConfig filePath
    ".json" -> loadJsonConfig filePath
    ext     -> return $ Left $ "Неподдерживаемый формат файла: " ++ ext

-- | Загрузка YAML конфигурации
loadYamlConfig :: FilePath -> IO (Either String CompilerConfig)
loadYamlConfig filePath = do
  result <- Yaml.decodeFileEither filePath
  case result of
    Left err -> return $ Left $ Yaml.prettyPrintParseException err
    Right config -> return $ Right config

-- | Загрузка JSON конфигурации
loadJsonConfig :: FilePath -> IO (Either String CompilerConfig)
loadJsonConfig filePath = do
  content <- TIO.readFile filePath
  case Aeson.eitherDecodeStrict' (Data.Text.Encoding.encodeUtf8 content) of
    Left err -> return $ Left err
    Right config -> return $ Right config

-- | Сохранение конфигурации в файл
saveConfigToFile :: FilePath -> CompilerConfig -> IO (Either String ())
saveConfigToFile filePath config = do
  case takeExtension filePath of
    ".yaml" -> saveYamlConfig filePath config
    ".yml"  -> saveYamlConfig filePath config
    ".json" -> saveJsonConfig filePath config
    ext     -> return $ Left $ "Неподдерживаемый формат файла: " ++ ext

-- | Сохранение YAML конфигурации
saveYamlConfig :: FilePath -> CompilerConfig -> IO (Either String ())
saveYamlConfig filePath config = do
  Yaml.encodeFile filePath config
  return $ Right ()

-- | Сохранение JSON конфигурации
saveJsonConfig :: FilePath -> CompilerConfig -> IO (Either String ())
saveJsonConfig filePath config = do
  let jsonContent = AesonPretty.encodePretty config
  Data.ByteString.Lazy.writeFile filePath jsonContent
  return $ Right ()

-- ============================================================================
-- УТИЛИТЫ
-- ============================================================================

-- | Объединение конфигураций (приоритет у второй)
mergeConfigs :: CompilerConfig -> CompilerConfig -> CompilerConfig
mergeConfigs base override = CompilerConfig
  { ccOptimization = ccOptimization override
  , ccCodeGen = ccCodeGen override  
  , ccLinker = ccLinker override
  , ccTarget = ccTarget override
  , ccVerbose = ccVerbose override
  , ccDebugLevel = ccDebugLevel override
  , ccIncludePaths = ccIncludePaths base ++ ccIncludePaths override
  , ccDefines = Map.union (ccDefines override) (ccDefines base)
  , ccOutputDir = ccOutputDir override
  }

-- | Валидация конфигурации
validateConfig :: CompilerConfig -> Either String CompilerConfig
validateConfig config = do
  -- Проверяем размеры памяти
  let targetConfig = ccTarget config
  when (tcFlashSize targetConfig == 0) $
    Left "Размер Flash памяти не может быть нулевым"
  
  when (tcRamSize targetConfig == 0) $
    Left "Размер RAM не может быть нулевым"
  
  -- Проверяем адреса
  let codeGenConfig = ccCodeGen config
  when (cgcCodeStartAddress codeGenConfig >= tcFlashSize targetConfig) $
    Left "Начальный адрес кода превышает размер Flash памяти"
  
  when (cgcDataStartAddress codeGenConfig >= tcRamSize targetConfig) $
    Left "Начальный адрес данных превышает размер RAM"
  
  return config

-- | Красивый вывод конфигурации
prettyConfig :: CompilerConfig -> Text
prettyConfig config = T.unlines
  [ "Конфигурация HCL компилятора:"
  , "=============================="
  , ""
  , "Целевая платформа:"
  , "  Архитектура: " <> T.pack (show (tcArchitecture (ccTarget config)))
  , "  Устройство: " <> tcDeviceName (ccTarget config)
  , "  Flash: " <> T.pack (show (tcFlashSize (ccTarget config))) <> " байт"
  , "  RAM: " <> T.pack (show (tcRamSize (ccTarget config))) <> " байт"
  , ""
  , "Оптимизация:"
  , "  Уровень: " <> T.pack (show (ocLevel (ccOptimization config)))
  , "  Удаление мёртвого кода: " <> showBool (ocDeadCodeElimination (ccOptimization config))
  , "  Свёртка констант: " <> showBool (ocConstantFolding (ccOptimization config))
  , "  Инлайнинг: " <> showBool (ocInlining (ccOptimization config))
  , ""
  , "Генерация кода:"
  , "  Отладочная информация: " <> showBool (cgcGenerateDebugInfo (ccCodeGen config))
  , "  Комментарии: " <> showBool (cgcGenerateComments (ccCodeGen config))
  , "  Размер стека: " <> T.pack (show (cgcStackSize (ccCodeGen config)))
  , ""
  , "Линковщик:"
  , "  Точка входа: " <> lcEntryPoint (ccLinker config)
  , "  Оптимизация размещения: " <> showBool (lcOptimizeLayout (ccLinker config))
  , "  Генерация карты: " <> showBool (lcGenerateMap (ccLinker config))
  ]
  where
    showBool True = "да"
    showBool False = "нет"

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Парсинг уровня отладки
parseDebugLevel :: String -> DebugLevel
parseDebugLevel = \case
  "none" -> DebugNone
  "minimal" -> DebugMinimal
  "standard" -> DebugStandard
  "verbose" -> DebugVerbose
  "trace" -> DebugTrace
  _ -> DebugNone

-- ============================================================================
-- ЭКЗЕМПЛЯРЫ AESON
-- ============================================================================

-- Для упрощения используем автоматический деривинг через Generic
instance ToJSON CompilerConfig where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON CompilerConfig where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON OptimizationConfig where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON OptimizationConfig where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON CodeGenConfig where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON CodeGenConfig where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON LinkerConfig where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON LinkerConfig where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON TargetConfig where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON TargetConfig where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON OptimizationLevel where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON OptimizationLevel where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON TargetArchitecture where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON TargetArchitecture where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON OutputFormat where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON OutputFormat where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions

instance ToJSON DebugLevel where
  toJSON = Aeson.genericToJSON Aeson.defaultOptions

instance FromJSON DebugLevel where
  parseJSON = Aeson.genericParseJSON Aeson.defaultOptions 