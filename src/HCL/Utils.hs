{-# LANGUAGE OverloadedStrings #-}

-- | Модуль утилит для HCL компилятора
--
-- Предоставляет общие вспомогательные функции, которые используются
-- в различных компонентах компилятора.
module HCL.Utils
  ( -- * Работа с текстом
    indent
  , indentBy
  , capitalize
  , uncapitalize
  , camelCase
  , snakeCase
  , pascalCase
  
    -- * Работа со списками
  , takeUntil
  , dropUntil
  , splitWhen
  , chunksOf
  , unique
  , groupBy'
  
    -- * Работа с Maybe и Either
  , whenJust
  , whenNothing
  , maybeToEither
  , eitherToMaybe
  , fromEitherOr
  
    -- * Форматирование
  , showHexValue
  , showBin
  , showPadded
  , formatSize
  , formatTime
  
    -- * Работа с файлами
  , ensureDirectoryExists
  , withTempFileHCL
  , copyFileIfNewer
  , getFileSize
  
    -- * Отладка и диагностика
  , debug
  , trace'
  , timeIt
  , measureMemory
  
    -- * Математические утилиты
  , align
  , roundUp
  , roundDown
  , clamp
  , between
  ) where

import Control.Monad (when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List (groupBy, nub, sort, isPrefixOf)
import Data.Char (toUpper, toLower, isUpper, isLower)
import Data.Maybe (fromMaybe, isJust, isNothing)
import System.Directory (createDirectoryIfMissing, doesFileExist, copyFile)
import qualified System.Directory as Dir
import System.FilePath (takeDirectory)
import System.IO (withFile, IOMode(..), hFileSize)
import System.IO.Temp (withTempFile)
import Text.Printf (printf)
import Numeric (showHex, showIntAtBase)
import Data.Time (getCurrentTime, diffUTCTime)

-- ============================================================================
-- РАБОТА С ТЕКСТОМ
-- ============================================================================

-- | Добавление отступа (2 пробела) к каждой строке
indent :: Text -> Text
indent = indentBy 2

-- | Добавление указанного количества пробелов к каждой строке
indentBy :: Int -> Text -> Text
indentBy n text = T.unlines $ map (T.replicate n " " <>) (T.lines text)

-- | Сделать первую букву заглавной
capitalize :: Text -> Text
capitalize text
  | T.null text = text
  | otherwise = T.cons (toUpper $ T.head text) (T.tail text)

-- | Сделать первую букву строчной
uncapitalize :: Text -> Text
uncapitalize text
  | T.null text = text
  | otherwise = T.cons (toLower $ T.head text) (T.tail text)

-- | Преобразование в camelCase
camelCase :: Text -> Text
camelCase text = 
  let words' = T.splitOn "_" text
  in case words' of
    [] -> ""
    (first:rest) -> T.concat (uncapitalize first : map capitalize rest)

-- | Преобразование в snake_case
snakeCase :: Text -> Text
snakeCase = T.pack . go . T.unpack
  where
    go [] = []
    go (c:cs)
      | isUpper c && not (null cs) = '_' : toLower c : go cs
      | otherwise = toLower c : go cs

-- | Преобразование в PascalCase
pascalCase :: Text -> Text
pascalCase text = T.concat $ map capitalize (T.splitOn "_" text)

-- ============================================================================
-- РАБОТА СО СПИСКАМИ
-- ============================================================================

-- | Взять элементы до тех пор, пока предикат не станет истинным
takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil _ [] = []
takeUntil p (x:xs)
  | p x = []
  | otherwise = x : takeUntil p xs

-- | Отбросить элементы до тех пор, пока предикат не станет истинным
dropUntil :: (a -> Bool) -> [a] -> [a]
dropUntil _ [] = []
dropUntil p xs@(x:xs')
  | p x = xs
  | otherwise = dropUntil p xs'

-- | Разбить список когда предикат истинен
splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen _ [] = []
splitWhen p xs = 
  let (chunk, rest) = break p xs
  in chunk : case rest of
    [] -> []
    (_:rest') -> splitWhen p rest'

-- | Разбить список на куски указанного размера
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = 
  let (chunk, rest) = splitAt n xs
  in chunk : chunksOf n rest

-- | Убрать дубликаты, сохранив порядок
unique :: Eq a => [a] -> [a]
unique = nub

-- | Группировка с сохранением соседних элементов
groupBy' :: (a -> a -> Bool) -> [a] -> [[a]]
groupBy' = groupBy

-- ============================================================================
-- РАБОТА С MAYBE И EITHER
-- ============================================================================

-- | Выполнить действие, если значение Just
whenJust :: Monad m => Maybe a -> (a -> m ()) -> m ()
whenJust Nothing _ = return ()
whenJust (Just a) f = f a

-- | Выполнить действие, если значение Nothing
whenNothing :: Monad m => Maybe a -> m () -> m ()
whenNothing Nothing action = action
whenNothing (Just _) _ = return ()

-- | Преобразование Maybe в Either
maybeToEither :: e -> Maybe a -> Either e a
maybeToEither err Nothing = Left err
maybeToEither _ (Just a) = Right a

-- | Преобразование Either в Maybe
eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Left _) = Nothing
eitherToMaybe (Right a) = Just a

-- | Получить значение из Either или значение по умолчанию
fromEitherOr :: a -> Either e a -> a
fromEitherOr def (Left _) = def
fromEitherOr _ (Right a) = a

-- ============================================================================
-- ФОРМАТИРОВАНИЕ
-- ============================================================================

-- | Показать число в шестнадцатеричном виде
showHexValue :: (Integral a, Show a) => a -> String
showHexValue n = "0x" ++ Numeric.showHex n ""

-- | Показать число в двоичном виде
showBin :: (Integral a, Show a) => a -> String
showBin n = "0b" ++ showIntAtBase 2 (\i -> if i == 0 then '0' else '1') n ""

-- | Показать число с заполнением нулями
showPadded :: Int -> Integer -> String
showPadded width n = printf ("%0" ++ show width ++ "d") n

-- | Форматирование размера в байтах
formatSize :: Integer -> String
formatSize bytes
  | bytes < 1024 = show bytes ++ " B"
  | bytes < 1024^2 = printf "%.1f KB" (fromInteger bytes / 1024 :: Double)
  | bytes < 1024^3 = printf "%.1f MB" (fromInteger bytes / 1024^2 :: Double)
  | otherwise = printf "%.1f GB" (fromInteger bytes / 1024^3 :: Double)

-- | Форматирование времени в секундах
formatTime :: Double -> String
formatTime seconds
  | seconds < 1 = printf "%.1f ms" (seconds * 1000)
  | seconds < 60 = printf "%.1f s" seconds
  | otherwise = printf "%.1f min" (seconds / 60)

-- ============================================================================
-- РАБОТА С ФАЙЛАМИ
-- ============================================================================

-- | Убедиться, что директория существует
ensureDirectoryExists :: FilePath -> IO ()
ensureDirectoryExists path = createDirectoryIfMissing True (takeDirectory path)

-- | Работа с временным файлом
withTempFileHCL :: String -> String -> (FilePath -> IO a) -> IO a
withTempFileHCL template ext action = 
  System.IO.Temp.withTempFile "/tmp" (template ++ ext) (\path _ -> action path)

-- | Копировать файл, если он новее
copyFileIfNewer :: FilePath -> FilePath -> IO Bool
copyFileIfNewer src dest = do
  srcExists <- doesFileExist src
  destExists <- doesFileExist dest
  
  if not srcExists
    then return False
    else if not destExists
      then do
        copyFile src dest
        return True
      else do
        srcSize <- Dir.getFileSize src
        destSize <- Dir.getFileSize dest
        if srcSize /= destSize
          then do
            copyFile src dest
            return True
          else return False

-- | Получить размер файла
getFileSize :: FilePath -> IO Integer
getFileSize path = do
  withFile path ReadMode $ \h -> do
    size <- hFileSize h
    return size

-- ============================================================================
-- ОТЛАДКА И ДИАГНОСТИКА
-- ============================================================================

-- | Отладочный вывод
debug :: MonadIO m => String -> m ()
debug msg = liftIO $ putStrLn $ "[DEBUG] " ++ msg

-- | Трассировка значения
trace' :: Show a => String -> a -> a
trace' msg value = 
  -- В production версии это должно быть закомментировано
  -- Debug.Trace.trace (msg ++ ": " ++ show value) value
  value

-- | Измерение времени выполнения
timeIt :: MonadIO m => String -> m a -> m a
timeIt label action = do
  start <- liftIO getCurrentTime
  result <- action
  end <- liftIO getCurrentTime
  let elapsed = realToFrac $ diffUTCTime end start
  liftIO $ putStrLn $ label ++ ": " ++ formatTime elapsed
  return result

-- | Измерение использования памяти (заглушка)
measureMemory :: MonadIO m => String -> m a -> m a
measureMemory _label action = action  -- Пока просто заглушка

-- ============================================================================
-- МАТЕМАТИЧЕСКИЕ УТИЛИТЫ
-- ============================================================================

-- | Выравнивание числа до ближайшего кратного
align :: Integral a => a -> a -> a
align alignment value = ((value + alignment - 1) `div` alignment) * alignment

-- | Округление вверх до ближайшего кратного
roundUp :: Integral a => a -> a -> a
roundUp = align

-- | Округление вниз до ближайшего кратного
roundDown :: Integral a => a -> a -> a
roundDown alignment value = (value `div` alignment) * alignment

-- | Ограничение значения диапазоном
clamp :: Ord a => a -> a -> a -> a
clamp minVal maxVal value = max minVal (min maxVal value)

-- | Проверка, находится ли значение в диапазоне
between :: Ord a => a -> a -> a -> Bool
between minVal maxVal value = value >= minVal && value <= maxVal 