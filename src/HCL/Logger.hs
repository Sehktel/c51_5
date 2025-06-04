{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- | Модуль логирования HCL компилятора
--
-- Предоставляет систему логирования для всех компонентов компилятора.
-- Поддерживает различные уровни логирования, форматирование и вывод
-- в файлы или консоль.
module HCL.Logger
  ( -- * Основные типы
    Logger(..)
  , LogLevel(..)
  , LogMessage(..)
  , LogConfig(..)
  
    -- * Создание логгера
  , createLogger
  , createFileLogger
  , createConsoleLogger
  , createNullLogger
  
    -- * Логирование
  , logTrace
  , logDebug
  , logInfo
  , logWarn
  , logError
  , logFatal
  
    -- * Конфигурация
  , defaultLogConfig
  , setLogLevel
  , setLogFormat
  
    -- * Утилиты
  , formatLogMessage
  , currentTimestamp
  ) where

import Control.Monad (when)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getCurrentTime, formatTime, defaultTimeLocale)
import System.IO (Handle, stdout, stderr, openFile, IOMode(..), hClose, hFlush)
import GHC.Generics (Generic)

-- ============================================================================
-- ОСНОВНЫЕ ТИПЫ
-- ============================================================================

-- | Уровень логирования
data LogLevel
  = Trace    -- ^ Трассировка (самый подробный уровень)
  | Debug    -- ^ Отладочная информация
  | Info     -- ^ Информационные сообщения
  | Warn     -- ^ Предупреждения
  | Error    -- ^ Ошибки
  | Fatal    -- ^ Критические ошибки
  deriving (Eq, Ord, Show, Enum, Bounded, Generic)

-- | Сообщение лога
data LogMessage = LogMessage
  { lmLevel :: !LogLevel      -- ^ Уровень сообщения
  , lmTime :: !Text           -- ^ Время создания сообщения
  , lmModule :: !Text         -- ^ Модуль, создавший сообщение
  , lmMessage :: !Text        -- ^ Текст сообщения
  } deriving (Eq, Show, Generic)

-- | Конфигурация логгера
data LogConfig = LogConfig
  { lcMinLevel :: !LogLevel        -- ^ Минимальный уровень для записи
  , lcShowTime :: !Bool            -- ^ Показывать время
  , lcShowLevel :: !Bool           -- ^ Показывать уровень
  , lcShowModule :: !Bool          -- ^ Показывать модуль
  , lcColorOutput :: !Bool         -- ^ Цветной вывод (для консоли)
  , lcFlushImmediately :: !Bool    -- ^ Немедленная запись на диск
  } deriving (Eq, Show, Generic)

-- | Логгер
data Logger = Logger
  { loggerConfig :: !LogConfig     -- ^ Конфигурация логгера
  , loggerHandle :: !(Maybe Handle) -- ^ Handle для записи (Nothing = stdout)
  , loggerWrite :: !(Text -> IO ()) -- ^ Функция записи
  }

-- ============================================================================
-- СОЗДАНИЕ ЛОГГЕРА
-- ============================================================================

-- | Создание базового логгера
createLogger :: LogConfig -> (Text -> IO ()) -> Logger
createLogger config writeFunc = Logger
  { loggerConfig = config
  , loggerHandle = Nothing
  , loggerWrite = writeFunc
  }

-- | Создание файлового логгера
createFileLogger :: LogConfig -> FilePath -> IO Logger
createFileLogger config filePath = do
  handle <- openFile filePath WriteMode
  return Logger
    { loggerConfig = config
    , loggerHandle = Just handle
    , loggerWrite = \msg -> do
        TIO.hPutStrLn handle msg
        when (lcFlushImmediately config) $ hFlush handle
    }

-- | Создание консольного логгера
createConsoleLogger :: LogConfig -> Logger
createConsoleLogger config = Logger
  { loggerConfig = config
  , loggerHandle = Nothing
  , loggerWrite = \msg -> do
      TIO.putStrLn msg
      when (lcFlushImmediately config) $ hFlush stdout
  }

-- | Создание null логгера (ничего не записывает)
createNullLogger :: Logger
createNullLogger = Logger
  { loggerConfig = defaultLogConfig { lcMinLevel = Fatal }
  , loggerHandle = Nothing
  , loggerWrite = \_ -> return ()
  }

-- ============================================================================
-- ЛОГИРОВАНИЕ
-- ============================================================================

-- | Трассировка
logTrace :: MonadIO m => Logger -> Text -> Text -> m ()
logTrace logger = logMessage logger Trace

-- | Отладочная информация
logDebug :: MonadIO m => Logger -> Text -> Text -> m ()
logDebug logger = logMessage logger Debug

-- | Информационное сообщение
logInfo :: MonadIO m => Logger -> Text -> Text -> m ()
logInfo logger = logMessage logger Info

-- | Предупреждение
logWarn :: MonadIO m => Logger -> Text -> Text -> m ()
logWarn logger = logMessage logger Warn

-- | Ошибка
logError :: MonadIO m => Logger -> Text -> Text -> m ()
logError logger = logMessage logger Error

-- | Критическая ошибка
logFatal :: MonadIO m => Logger -> Text -> Text -> m ()
logFatal logger = logMessage logger Fatal

-- | Базовая функция логирования
logMessage :: MonadIO m => Logger -> LogLevel -> Text -> Text -> m ()
logMessage logger level moduleName message = liftIO $ do
  let config = loggerConfig logger
  when (level >= lcMinLevel config) $ do
    timestamp <- currentTimestamp
    let logMsg = LogMessage
          { lmLevel = level
          , lmTime = timestamp
          , lmModule = moduleName
          , lmMessage = message
          }
    formatted <- formatLogMessage config logMsg
    loggerWrite logger formatted

-- ============================================================================
-- КОНФИГУРАЦИЯ
-- ============================================================================

-- | Конфигурация логгера по умолчанию
defaultLogConfig :: LogConfig
defaultLogConfig = LogConfig
  { lcMinLevel = Info
  , lcShowTime = True
  , lcShowLevel = True
  , lcShowModule = True
  , lcColorOutput = False
  , lcFlushImmediately = True
  }

-- | Установка минимального уровня логирования
setLogLevel :: LogLevel -> LogConfig -> LogConfig
setLogLevel level config = config { lcMinLevel = level }

-- | Установка формата логирования
setLogFormat :: Bool -> Bool -> Bool -> LogConfig -> LogConfig
setLogFormat showTime showLevel showModule config = config
  { lcShowTime = showTime
  , lcShowLevel = showLevel
  , lcShowModule = showModule
  }

-- ============================================================================
-- УТИЛИТЫ
-- ============================================================================

-- | Форматирование сообщения лога
formatLogMessage :: LogConfig -> LogMessage -> IO Text
formatLogMessage config LogMessage{..} = do
  let parts = []
      
  -- Время
  let parts1 = if lcShowTime config 
               then lmTime : parts
               else parts
  
  -- Уровень
  let parts2 = if lcShowLevel config
               then formatLevel config lmLevel : parts1
               else parts1
  
  -- Модуль
  let parts3 = if lcShowModule config
               then ("[" <> lmModule <> "]") : parts2
               else parts2
  
  -- Сообщение
  let parts4 = lmMessage : parts3
  
  return $ T.intercalate " " (reverse parts4)

-- | Форматирование уровня логирования
formatLevel :: LogConfig -> LogLevel -> Text
formatLevel config level
  | lcColorOutput config = colorizeLevel level (levelText level)
  | otherwise = "[" <> levelText level <> "]"
  where
    levelText = \case
      Trace -> "TRACE"
      Debug -> "DEBUG"
      Info  -> "INFO "
      Warn  -> "WARN "
      Error -> "ERROR"
      Fatal -> "FATAL"

-- | Раскрашивание уровня для консольного вывода
colorizeLevel :: LogLevel -> Text -> Text
colorizeLevel level text = case level of
  Trace -> "\ESC[37m" <> text <> "\ESC[0m"  -- Серый
  Debug -> "\ESC[36m" <> text <> "\ESC[0m"  -- Голубой
  Info  -> "\ESC[32m" <> text <> "\ESC[0m"  -- Зелёный
  Warn  -> "\ESC[33m" <> text <> "\ESC[0m"  -- Жёлтый
  Error -> "\ESC[31m" <> text <> "\ESC[0m"  -- Красный
  Fatal -> "\ESC[35m" <> text <> "\ESC[0m"  -- Пурпурный

-- | Получение текущего времени в виде строки
currentTimestamp :: IO Text
currentTimestamp = do
  time <- getCurrentTime
  return $ T.pack $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" time 