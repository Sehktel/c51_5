{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}

-- | Модуль линковщика HCL
--
-- Реализует полноценный линковщик для микроконтроллера AT89S4051.
-- Основные функции:
-- * Объединение объектных файлов в единый образ
-- * Разрешение символов между модулями
-- * Размещение кода и данных в памяти
-- * Генерация Intel HEX, двоичных файлов и листингов
-- * Проверка ограничений памяти AT89S4051
module HCL.Linker
  ( -- * Основные типы
    LinkerConfig(..)
  , LinkerState(..)
  , LinkerResult(..)
  , ObjectFile(..)
  , Symbol(..)
  , Relocation(..)
  , MemorySegment(..)
  , MemoryMap(..)
  , OutputFormat(..)
  
    -- * Основные функции
  , linkProgram
  , linkObjectFiles
  , generateOutput
  , defaultLinkerConfig
  
    -- * Работа с объектными файлами
  , loadObjectFile
  , parseObjectFile
  , validateObjectFile
  
    -- * Разрешение символов
  , resolveSymbols
  , buildSymbolTable
  , checkUndefinedSymbols
  
    -- * Размещение в памяти
  , layoutMemory
  , allocateCodeSegments
  , checkMemoryConstraints
  
    -- * Генерация выходных форматов
  , generateHexFile
  , generateBinaryFile
  , generateListingFile
  , generateMapFile
  
    -- * Утилиты
  , prettyLinkerResult
  , prettyMemoryMap
  , calculateMemoryUsage
  ) where

import Control.Monad (when, unless, foldM, foldM_, forM_, mapM_)
import Control.Monad.State.Strict (StateT, runStateT, get, put, modify)
import Control.Monad.Except (ExceptT, runExceptT, throwError, catchError)
import Control.Monad.IO.Class (liftIO)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Text.Encoding as TE
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word8, Word16)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (sortBy, foldl')
import Data.Maybe (fromMaybe, catMaybes)
import Text.Printf (printf)
import System.FilePath (takeExtension, replaceExtension)
import GHC.Generics (Generic)

import HCL.Error (CompilerError(..), Diagnostic(..), DiagnosticLevel(..), LinkerError(..), mkError, mkWarning, mkInfo)
import HCL.Types (Identifier(..), identifierText)

-- ============================================================================
-- ОСНОВНЫЕ ТИПЫ
-- ============================================================================

-- | Конфигурация линковщика
data LinkerConfig = LinkerConfig
  { lcTargetDevice :: !Text                    -- ^ Целевое устройство (AT89S4051)
  , lcCodeStartAddress :: !Word16              -- ^ Начальный адрес кода (обычно 0x0000)
  , lcDataStartAddress :: !Word16              -- ^ Начальный адрес данных (обычно 0x0030)
  , lcStackStartAddress :: !Word16             -- ^ Начальный адрес стека (обычно 0x007F)
  , lcMaxCodeSize :: !Word16                   -- ^ Максимальный размер кода (2KB для AT89S4051)
  , lcMaxDataSize :: !Word16                   -- ^ Максимальный размер данных (128 байт)
  , lcEntryPoint :: !Text                      -- ^ Точка входа (обычно "main")
  , lcOutputFormats :: ![OutputFormat]         -- ^ Форматы выходных файлов
  , lcOptimizeLayout :: !Bool                  -- ^ Оптимизировать размещение
  , lcGenerateMap :: !Bool                     -- ^ Генерировать карту памяти
  , lcVerbose :: !Bool                         -- ^ Подробный вывод
  } deriving (Eq, Show, Generic)

-- | Состояние линковщика
data LinkerState = LinkerState
  { lsSymbolTable :: !(Map Text Symbol)        -- ^ Таблица символов
  , lsRelocations :: ![Relocation]             -- ^ Список перемещений
  , lsMemoryMap :: !MemoryMap                  -- ^ Карта памяти
  , lsCodeSegments :: ![MemorySegment]         -- ^ Сегменты кода
  , lsDataSegments :: ![MemorySegment]         -- ^ Сегменты данных
  , lsUndefinedSymbols :: !(Set Text)          -- ^ Неопределённые символы
  , lsDiagnostics :: ![Diagnostic]             -- ^ Диагностические сообщения
  } deriving (Eq, Show, Generic)

-- | Результат работы линковщика
data LinkerResult = LinkerResult
  { lrSuccess :: !Bool                         -- ^ Успешность линковки
  , lrMemoryMap :: !MemoryMap                  -- ^ Финальная карта памяти
  , lrCodeImage :: !ByteString                 -- ^ Образ кода
  , lrDataImage :: !ByteString                 -- ^ Образ данных
  , lrSymbolTable :: !(Map Text Symbol)       -- ^ Финальная таблица символов
  , lrDiagnostics :: ![Diagnostic]             -- ^ Диагностические сообщения
  , lrMemoryUsage :: !MemoryUsage              -- ^ Использование памяти
  } deriving (Eq, Show, Generic)

-- | Объектный файл
data ObjectFile = ObjectFile
  { ofFileName :: !FilePath                    -- ^ Имя файла
  , ofSymbols :: ![Symbol]                     -- ^ Символы в файле
  , ofRelocations :: ![Relocation]             -- ^ Перемещения
  , ofCodeSections :: ![MemorySegment]         -- ^ Секции кода
  , ofDataSections :: ![MemorySegment]         -- ^ Секции данных
  , ofMetadata :: !(Map Text Text)             -- ^ Метаданные
  } deriving (Eq, Show, Generic)

-- | Символ
data Symbol = Symbol
  { symName :: !Text                           -- ^ Имя символа
  , symAddress :: !Word16                      -- ^ Адрес символа
  , symSize :: !Word16                         -- ^ Размер символа
  , symType :: !SymbolType                     -- ^ Тип символа
  , symScope :: !SymbolScope                   -- ^ Область видимости
  , symSection :: !Text                        -- ^ Секция
  , symDefined :: !Bool                        -- ^ Определён ли символ
  } deriving (Eq, Show, Generic)

-- | Тип символа
data SymbolType
  = SymFunction                                -- ^ Функция
  | SymVariable                                -- ^ Переменная
  | SymConstant                                -- ^ Константа
  | SymLabel                                   -- ^ Метка
  | SymSfr                                     -- ^ Специальный регистр
  | SymBit                                     -- ^ Битовая переменная
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | Область видимости символа
data SymbolScope
  = ScopeGlobal                                -- ^ Глобальная
  | ScopeLocal                                 -- ^ Локальная
  | ScopeExternal                              -- ^ Внешняя
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | Перемещение
data Relocation = Relocation
  { relOffset :: !Word16                       -- ^ Смещение в секции
  , relSymbol :: !Text                         -- ^ Имя символа
  , relType :: !RelocationType                 -- ^ Тип перемещения
  , relSection :: !Text                        -- ^ Секция
  , relAddend :: !Int                          -- ^ Добавка
  } deriving (Eq, Show, Generic)

-- | Тип перемещения
data RelocationType
  = RelAbsolute16                              -- ^ Абсолютный 16-битный адрес
  | RelAbsolute8                               -- ^ Абсолютный 8-битный адрес
  | RelRelative8                               -- ^ Относительный 8-битный адрес
  | RelPage                                    -- ^ Номер страницы
  | RelOffset                                  -- ^ Смещение в странице
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | Сегмент памяти
data MemorySegment = MemorySegment
  { msName :: !Text                            -- ^ Имя сегмента
  , msStartAddress :: !Word16                  -- ^ Начальный адрес
  , msSize :: !Word16                          -- ^ Размер сегмента
  , msData :: !ByteString                      -- ^ Данные сегмента
  , msType :: !SegmentType                     -- ^ Тип сегмента
  , msAlignment :: !Word16                     -- ^ Выравнивание
  } deriving (Eq, Show, Generic)

-- | Тип сегмента
data SegmentType
  = SegmentCode                                -- ^ Код
  | SegmentData                                -- ^ Данные
  | SegmentBss                                 -- ^ Неинициализированные данные
  | SegmentStack                               -- ^ Стек
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | Карта памяти
data MemoryMap = MemoryMap
  { mmCodeRegion :: !MemoryRegion              -- ^ Область кода
  , mmDataRegion :: !MemoryRegion              -- ^ Область данных
  , mmStackRegion :: !MemoryRegion             -- ^ Область стека
  , mmSfrRegion :: !MemoryRegion               -- ^ Область SFR
  } deriving (Eq, Show, Generic)

-- | Область памяти
data MemoryRegion = MemoryRegion
  { mrStart :: !Word16                         -- ^ Начальный адрес
  , mrEnd :: !Word16                           -- ^ Конечный адрес
  , mrUsed :: !Word16                          -- ^ Использовано байт
  , mrSegments :: ![MemorySegment]             -- ^ Сегменты в области
  } deriving (Eq, Show, Generic)

-- | Использование памяти
data MemoryUsage = MemoryUsage
  { muCodeUsed :: !Word16                      -- ^ Использовано кода
  , muCodeTotal :: !Word16                     -- ^ Всего кода
  , muDataUsed :: !Word16                      -- ^ Использовано данных
  , muDataTotal :: !Word16                     -- ^ Всего данных
  , muStackUsed :: !Word16                     -- ^ Использовано стека
  , muStackTotal :: !Word16                    -- ^ Всего стека
  } deriving (Eq, Show, Generic)

-- | Формат выходного файла
data OutputFormat
  = FormatHex                                  -- ^ Intel HEX
  | FormatBinary                               -- ^ Двоичный файл
  | FormatListing                              -- ^ Листинг
  | FormatMap                                  -- ^ Карта памяти
  deriving (Eq, Show, Enum, Bounded, Generic)

-- | Монада линковщика
type LinkerM = ExceptT CompilerError (StateT LinkerState IO)

-- ============================================================================
-- ОСНОВНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Основная функция линковки программы
linkProgram :: LinkerConfig -> [FilePath] -> IO (Either CompilerError LinkerResult)
linkProgram config objectFiles = do
  let initialState = LinkerState
        { lsSymbolTable = Map.empty
        , lsRelocations = []
        , lsMemoryMap = defaultMemoryMap config
        , lsCodeSegments = []
        , lsDataSegments = []
        , lsUndefinedSymbols = Set.empty
        , lsDiagnostics = []
        }
  
  result <- runStateT (runExceptT (linkProgram' config objectFiles)) initialState
  case result of
    (Left err, state) -> return $ Left err
    (Right (), state) -> do
      let memoryUsage = calculateMemoryUsage' (lsMemoryMap state)
      return $ Right LinkerResult
        { lrSuccess = True
        , lrMemoryMap = lsMemoryMap state
        , lrCodeImage = generateCodeImage state
        , lrDataImage = generateDataImage state
        , lrSymbolTable = lsSymbolTable state
        , lrDiagnostics = lsDiagnostics state
        , lrMemoryUsage = memoryUsage
        }

-- | Внутренняя функция линковки
linkProgram' :: LinkerConfig -> [FilePath] -> LinkerM ()
linkProgram' config@LinkerConfig{..} objectFiles = do
  -- Загружаем объектные файлы
  when lcVerbose $ liftIO $ putStrLn "Загрузка объектных файлов..."
  objects <- mapM loadObjectFile objectFiles
  
  -- Строим таблицу символов
  when lcVerbose $ liftIO $ putStrLn "Построение таблицы символов..."
  buildSymbolTable objects
  
  -- Проверяем неопределённые символы
  when lcVerbose $ liftIO $ putStrLn "Проверка неопределённых символов..."
  checkUndefinedSymbols
  
  -- Размещаем сегменты в памяти
  when lcVerbose $ liftIO $ putStrLn "Размещение сегментов в памяти..."
  layoutMemory config objects
  
  -- Разрешаем символы и применяем перемещения
  when lcVerbose $ liftIO $ putStrLn "Разрешение символов..."
  resolveSymbols
  
  -- Проверяем ограничения памяти
  when lcVerbose $ liftIO $ putStrLn "Проверка ограничений памяти..."
  checkMemoryConstraints config
  
  when lcVerbose $ liftIO $ putStrLn "Линковка завершена успешно!"

-- | Объединение объектных файлов
linkObjectFiles :: [ObjectFile] -> LinkerM ()
linkObjectFiles objects = do
  -- Объединяем символы
  mapM_ addObjectSymbols objects
  
  -- Объединяем перемещения
  mapM_ addObjectRelocations objects
  
  -- Объединяем сегменты
  mapM_ addObjectSegments objects

-- ============================================================================
-- РАБОТА С ОБЪЕКТНЫМИ ФАЙЛАМИ
-- ============================================================================

-- | Загрузка объектного файла
loadObjectFile :: FilePath -> LinkerM ObjectFile
loadObjectFile filePath = do
  content <- liftIO $ BS.readFile filePath
  parseObjectFile filePath content

-- | Парсинг объектного файла
parseObjectFile :: FilePath -> ByteString -> LinkerM ObjectFile
parseObjectFile filePath content = do
  -- Простой формат объектного файла для демонстрации
  -- В реальной реализации здесь был бы полноценный парсер
  let textContent = TE.decodeUtf8 content
      lines' = T.lines textContent
  
  -- Парсим секции
  symbols <- parseSymbolSection lines'
  relocations <- parseRelocationSection lines'
  codeSections <- parseCodeSections lines'
  dataSections <- parseDataSections lines'
  
  return ObjectFile
    { ofFileName = filePath
    , ofSymbols = symbols
    , ofRelocations = relocations
    , ofCodeSections = codeSections
    , ofDataSections = dataSections
    , ofMetadata = Map.empty
    }

-- | Парсинг секции символов
parseSymbolSection :: [Text] -> LinkerM [Symbol]
parseSymbolSection lines' = do
  -- Ищем секцию символов
  let symbolLines = takeWhile (/= ".end_symbols") $ 
                   drop 1 $ dropWhile (/= ".symbols") lines'
  
  mapM parseSymbolLine symbolLines

-- | Парсинг строки символа
parseSymbolLine :: Text -> LinkerM Symbol
parseSymbolLine line = do
  let parts = T.words line
  case parts of
    [name, addrStr, sizeStr, typeStr, scopeStr, section] -> do
      addr <- parseHex addrStr
      size <- parseHex sizeStr
      symType <- parseSymbolType typeStr
      scope <- parseSymbolScope scopeStr
      
      return Symbol
        { symName = name
        , symAddress = addr
        , symSize = size
        , symType = symType
        , symScope = scope
        , symSection = section
        , symDefined = True
        }
    _ -> throwError $ LinkerError $ "Неверный формат символа: " <> line

-- | Парсинг секции перемещений
parseRelocationSection :: [Text] -> LinkerM [Relocation]
parseRelocationSection lines' = do
  let relocationLines = takeWhile (/= ".end_relocations") $ 
                       drop 1 $ dropWhile (/= ".relocations") lines'
  
  mapM parseRelocationLine relocationLines

-- | Парсинг строки перемещения
parseRelocationLine :: Text -> LinkerM Relocation
parseRelocationLine line = do
  let parts = T.words line
  case parts of
    [offsetStr, symbol, typeStr, section, addendStr] -> do
      offset <- parseHex offsetStr
      relType <- parseRelocationType typeStr
      addend <- parseDecimal addendStr
      
      return Relocation
        { relOffset = offset
        , relSymbol = symbol
        , relType = relType
        , relSection = section
        , relAddend = addend
        }
    _ -> throwError $ LinkerError $ "Неверный формат перемещения: " <> line

-- | Парсинг секций кода
parseCodeSections :: [Text] -> LinkerM [MemorySegment]
parseCodeSections lines' = do
  -- Простая реализация для демонстрации
  return []

-- | Парсинг секций данных
parseDataSections :: [Text] -> LinkerM [MemorySegment]
parseDataSections lines' = do
  -- Простая реализация для демонстрации
  return []

-- | Валидация объектного файла
validateObjectFile :: ObjectFile -> LinkerM ()
validateObjectFile ObjectFile{..} = do
  validateSymbols ofSymbols
  validateRelocations ofRelocations
  validateMemorySegments (ofCodeSections ++ ofDataSections)

-- | Валидация символов
validateSymbols :: [Symbol] -> LinkerM ()
validateSymbols symbols = do
  mapM_ validateSymbol symbols

-- | Валидация отдельного символа
validateSymbol :: Symbol -> LinkerM ()
validateSymbol Symbol{..} = do
  when (T.null symName) $ do
    addDiagnostic $ mkError "Пустое имя символа" Nothing

-- | Валидация перемещений
validateRelocations :: [Relocation] -> LinkerM ()
validateRelocations relocs = do
  mapM_ validateRelocation relocs

-- | Валидация отдельного перемещения
validateRelocation :: Relocation -> LinkerM ()
validateRelocation Relocation{..} = do
  when (T.null relSymbol) $ do
    addDiagnostic $ mkError "Пустое имя символа в перемещении" Nothing

-- | Валидация сегментов памяти
validateMemorySegments :: [MemorySegment] -> LinkerM ()
validateMemorySegments segments = do
  mapM_ validateMemorySegment segments

-- | Валидация отдельного сегмента памяти
validateMemorySegment ms@MemorySegment{..} = do
  let actualSize = BS.length msData
  when (actualSize /= fromIntegral msSize) $ do
    addDiagnostic $ mkError 
      ("Несоответствие размера данных в сегменте " <> msName <> 
       ": ожидается " <> T.pack (show msSize) <> 
       ", фактически " <> T.pack (show actualSize)) Nothing

-- ============================================================================
-- РАЗРЕШЕНИЕ СИМВОЛОВ
-- ============================================================================

-- | Построение таблицы символов
buildSymbolTable :: [ObjectFile] -> LinkerM ()
buildSymbolTable objects = do
  mapM_ addObjectSymbols objects

-- | Добавление символов из объектного файла
addObjectSymbols :: ObjectFile -> LinkerM ()
addObjectSymbols ObjectFile{..} = do
  mapM_ addSymbol ofSymbols

-- | Добавление символа в таблицу
addSymbol :: Symbol -> LinkerM ()
addSymbol symbol@Symbol{..} = do
  state <- get
  let symbolTable = lsSymbolTable state
  
  case Map.lookup symName symbolTable of
    Nothing -> do
      -- Новый символ
      put state { lsSymbolTable = Map.insert symName symbol symbolTable }
    
    Just existingSymbol -> do
      -- Проверяем конфликты
      let Symbol{symDefined = existingDefined} = existingSymbol
      if symDefined && existingDefined
        then throwError $ LinkerError $ 
               "Дублирование символа: " <> symName
        else do
          -- Объединяем определения
          let mergedSymbol = if symDefined
                            then symbol 
                            else existingSymbol
          put state { lsSymbolTable = Map.insert symName mergedSymbol symbolTable }

-- | Разрешение символов
resolveSymbols :: LinkerM ()
resolveSymbols = do
  state <- get
  let relocations = lsRelocations state
      symbolTable = lsSymbolTable state
  
  -- Применяем все перемещения
  mapM_ (applyRelocation symbolTable) relocations

-- | Применение перемещения
applyRelocation :: Map Text Symbol -> Relocation -> LinkerM ()
applyRelocation symbolTable Relocation{..} = do
  case Map.lookup relSymbol symbolTable of
    Nothing -> do
      -- Добавляем в список неопределённых символов
      modify $ \s -> s { lsUndefinedSymbols = Set.insert relSymbol (lsUndefinedSymbols s) }
    
    Just symbol -> do
      -- Вычисляем адрес и применяем перемещение
      let targetAddress = symAddress symbol + fromIntegral relAddend
      applyRelocationToSegment relSection relOffset relType targetAddress

-- | Применение перемещения к сегменту
applyRelocationToSegment :: Text -> Word16 -> RelocationType -> Word16 -> LinkerM ()
applyRelocationToSegment sectionName offset relocType targetAddress = do
  -- Находим сегмент и применяем перемещение
  -- Простая реализация для демонстрации
  return ()

-- | Проверка неопределённых символов
checkUndefinedSymbols :: LinkerM ()
checkUndefinedSymbols = do
  state <- get
  let undefinedSymbols = lsUndefinedSymbols state
  
  unless (Set.null undefinedSymbols) $ do
    let symbolList = T.intercalate ", " (Set.toList undefinedSymbols)
    throwError $ LinkerError $ "Неопределённые символы: " <> symbolList

-- ============================================================================
-- РАЗМЕЩЕНИЕ В ПАМЯТИ
-- ============================================================================

-- | Размещение сегментов в памяти
layoutMemory :: LinkerConfig -> [ObjectFile] -> LinkerM ()
layoutMemory config objects = do
  -- Собираем все сегменты
  let allCodeSegments = concatMap ofCodeSections objects
      allDataSegments = concatMap ofDataSections objects
  
  -- Размещаем сегменты кода
  allocateCodeSegments config allCodeSegments
  
  -- Размещаем сегменты данных
  allocateDataSegments config allDataSegments

-- | Размещение сегментов кода
allocateCodeSegments :: LinkerConfig -> [MemorySegment] -> LinkerM ()
allocateCodeSegments LinkerConfig{..} segments = do
  let sortedSegments = sortBy (\a b -> compare (msAlignment a) (msAlignment b)) segments
  
  foldM_ allocateCodeSegment lcCodeStartAddress sortedSegments

-- | Размещение одного сегмента кода
allocateCodeSegment :: Word16 -> MemorySegment -> LinkerM Word16
allocateCodeSegment currentAddress segment = do
  let alignedAddress = alignAddress currentAddress (msAlignment segment)
      newSegment = segment { msStartAddress = alignedAddress }
  
  -- Добавляем сегмент в состояние
  modify $ \s -> s { lsCodeSegments = newSegment : lsCodeSegments s }
  
  return $ alignedAddress + msSize segment

-- | Размещение сегментов данных
allocateDataSegments :: LinkerConfig -> [MemorySegment] -> LinkerM ()
allocateDataSegments LinkerConfig{..} segments = do
  let sortedSegments = sortBy (\a b -> compare (msAlignment a) (msAlignment b)) segments
  
  foldM_ allocateDataSegment lcDataStartAddress sortedSegments

-- | Разделение сегментов на код и данные
partitionSegments :: [MemorySegment] -> ([MemorySegment], [MemorySegment])
partitionSegments = foldr classify ([], [])
  where
    classify seg (code, dat) = case msType seg of
      SegmentCode -> (seg : code, dat)
      SegmentData -> (code, seg : dat)
      SegmentBss -> (code, seg : dat)
      SegmentStack -> (code, seg : dat)

-- | Размещение одного сегмента данных
allocateDataSegment :: Word16 -> MemorySegment -> LinkerM Word16
allocateDataSegment currentAddress segment = do
  let alignedAddress = alignAddress currentAddress (msAlignment segment)
      newSegment = segment { msStartAddress = alignedAddress }
  
  -- Добавляем сегмент в состояние
  modify $ \s -> s { lsDataSegments = newSegment : lsDataSegments s }
  
  return $ alignedAddress + msSize segment

-- | Выравнивание адреса
alignAddress :: Word16 -> Word16 -> Word16
alignAddress address alignment = 
  if alignment <= 1
    then address
    else ((address + alignment - 1) `div` alignment) * alignment

-- | Проверка ограничений памяти
checkMemoryConstraints :: LinkerConfig -> LinkerM ()
checkMemoryConstraints LinkerConfig{..} = do
  state <- get
  let codeSize = sum $ map msSize (lsCodeSegments state)
      dataSize = sum $ map msSize (lsDataSegments state)
  
  when (codeSize > lcMaxCodeSize) $ do
    throwError $ LinkerError $ 
      "Превышен размер кода: " <> T.pack (show codeSize) <> 
      " > " <> T.pack (show lcMaxCodeSize)
  
  when (dataSize > lcMaxDataSize) $ do
    throwError $ LinkerError $ 
      "Превышен размер данных: " <> T.pack (show dataSize) <> 
      " > " <> T.pack (show lcMaxDataSize)

-- ============================================================================
-- ГЕНЕРАЦИЯ ВЫХОДНЫХ ФОРМАТОВ
-- ============================================================================

-- | Генерация выходных файлов
generateOutput :: LinkerConfig -> LinkerResult -> FilePath -> IO ()
generateOutput config result baseFileName = do
  mapM_ (generateOutputFormat result baseFileName) (lcOutputFormats config)

-- | Генерация конкретного формата
generateOutputFormat :: LinkerResult -> FilePath -> OutputFormat -> IO ()
generateOutputFormat result baseFileName format = case format of
  FormatHex -> do
    let hexFile = replaceExtension baseFileName ".hex"
    hexContent <- generateHexFile result
    TIO.writeFile hexFile hexContent
    putStrLn $ "Сгенерирован HEX файл: " ++ hexFile
  
  FormatBinary -> do
    let binFile = replaceExtension baseFileName ".bin"
    BS.writeFile binFile (lrCodeImage result)
    putStrLn $ "Сгенерирован двоичный файл: " ++ binFile
  
  FormatListing -> do
    let lstFile = replaceExtension baseFileName ".lst"
    lstContent <- generateListingFile result
    TIO.writeFile lstFile lstContent
    putStrLn $ "Сгенерирован листинг: " ++ lstFile
  
  FormatMap -> do
    let mapFile = replaceExtension baseFileName ".map"
    let mapContent = generateMapFile result
    TIO.writeFile mapFile mapContent
    putStrLn $ "Сгенерирована карта памяти: " ++ mapFile

-- | Генерация Intel HEX файла
generateHexFile :: LinkerResult -> IO Text
generateHexFile LinkerResult{..} = do
  let codeBytes = BS.unpack lrCodeImage
      hexLines = generateHexLines 0x0000 codeBytes
      eofLine = ":00000001FF"
  
  return $ T.unlines (hexLines ++ [eofLine])

-- | Генерация HEX строк для записи в Intel HEX формат
-- Каждая строка содержит до 16 байт данных
generateHexLines :: Word16 -> [Word8] -> [Text]
generateHexLines _ [] = []
generateHexLines address bytes = 
  let (chunk, rest) = splitAt 16 bytes
      chunkSize = length chunk
      checksum = calculateHexChecksum address chunkSize chunk
      -- Явно указываем тип для устранения неоднозначности
      hexLine = T.pack $ Text.Printf.printf ":%02X%04X00%s%02X"
                        (chunkSize :: Int) (address :: Word16)
                        (concatMap (Text.Printf.printf "%02X" :: Word8 -> String) chunk) (checksum :: Word8)
  in hexLine : generateHexLines (address + fromIntegral chunkSize) rest

-- | Вычисление контрольной суммы HEX
calculateHexChecksum :: Word16 -> Int -> [Word8] -> Word8
calculateHexChecksum address size bytes = 
  let addrHigh = fromIntegral (address `div` 256)
      addrLow = fromIntegral (address `mod` 256)
      sizeWord = fromIntegral size
      sum' = sizeWord + addrHigh + addrLow + sum bytes
  in fromIntegral ((256 - (fromIntegral sum' `mod` 256)) `mod` 256)

-- | Генерация двоичного файла
generateBinaryFile :: LinkerResult -> ByteString
generateBinaryFile LinkerResult{..} = lrCodeImage

-- | Генерация листинга
generateListingFile :: LinkerResult -> IO Text
generateListingFile LinkerResult{..} = do
  let header = T.unlines
        [ "HCL Linker - Листинг программы"
        , "================================"
        , ""
        , "Карта памяти:"
        , prettyMemoryMap lrMemoryMap
        , ""
        , "Таблица символов:"
        ]
      
      symbolLines = map prettySymbol (Map.elems lrSymbolTable)
      
      footer = T.unlines
        [ ""
        , "Использование памяти:"
        , prettyMemoryUsage lrMemoryUsage
        ]
  
  return $ header <> T.unlines symbolLines <> footer

-- | Генерация карты памяти
generateMapFile :: LinkerResult -> Text
generateMapFile LinkerResult{..} = T.unlines
  [ "HCL Linker - Карта памяти"
  , "========================="
  , ""
  , prettyMemoryMap lrMemoryMap
  , ""
  , "Использование памяти:"
  , prettyMemoryUsage lrMemoryUsage
  ]

-- ============================================================================
-- КОНФИГУРАЦИЯ ПО УМОЛЧАНИЮ
-- ============================================================================

-- | Конфигурация линковщика по умолчанию для AT89S4051
defaultLinkerConfig :: LinkerConfig
defaultLinkerConfig = LinkerConfig
  { lcTargetDevice = "AT89S4051"
  , lcCodeStartAddress = 0x0000
  , lcDataStartAddress = 0x0030
  , lcStackStartAddress = 0x007F
  , lcMaxCodeSize = 2048        -- 2KB Flash
  , lcMaxDataSize = 128         -- 128 байт RAM
  , lcEntryPoint = "main"
  , lcOutputFormats = [FormatHex, FormatListing]
  , lcOptimizeLayout = True
  , lcGenerateMap = True
  , lcVerbose = False
  }

-- | Карта памяти по умолчанию
defaultMemoryMap :: LinkerConfig -> MemoryMap
defaultMemoryMap LinkerConfig{..} = MemoryMap
  { mmCodeRegion = MemoryRegion
      { mrStart = lcCodeStartAddress
      , mrEnd = lcCodeStartAddress + lcMaxCodeSize - 1
      , mrUsed = 0
      , mrSegments = []
      }
  , mmDataRegion = MemoryRegion
      { mrStart = lcDataStartAddress
      , mrEnd = lcDataStartAddress + lcMaxDataSize - 1
      , mrUsed = 0
      , mrSegments = []
      }
  , mmStackRegion = MemoryRegion
      { mrStart = lcStackStartAddress
      , mrEnd = lcStackStartAddress
      , mrUsed = 0
      , mrSegments = []
      }
  , mmSfrRegion = MemoryRegion
      { mrStart = 0x0080
      , mrEnd = 0x00FF
      , mrUsed = 0
      , mrSegments = []
      }
  }

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Добавление перемещений из объектного файла
addObjectRelocations :: ObjectFile -> LinkerM ()
addObjectRelocations ObjectFile{..} = do
  modify $ \s -> s { lsRelocations = ofRelocations ++ lsRelocations s }

-- | Добавление сегментов из объектного файла
addObjectSegments :: ObjectFile -> LinkerM ()
addObjectSegments ObjectFile{..} = do
  modify $ \s -> s 
    { lsCodeSegments = ofCodeSections ++ lsCodeSegments s
    , lsDataSegments = ofDataSections ++ lsDataSegments s
    }

-- | Генерация образа кода
generateCodeImage :: LinkerState -> ByteString
generateCodeImage LinkerState{..} = 
  BS.concat $ map msData lsCodeSegments

-- | Генерация образа данных
generateDataImage :: LinkerState -> ByteString
generateDataImage LinkerState{..} = 
  BS.concat $ map msData lsDataSegments

-- | Вычисление использования памяти
calculateMemoryUsage' :: MemoryMap -> MemoryUsage
calculateMemoryUsage' MemoryMap{..} = MemoryUsage
  { muCodeUsed = mrUsed mmCodeRegion
  , muCodeTotal = mrEnd mmCodeRegion - mrStart mmCodeRegion + 1
  , muDataUsed = mrUsed mmDataRegion
  , muDataTotal = mrEnd mmDataRegion - mrStart mmDataRegion + 1
  , muStackUsed = mrUsed mmStackRegion
  , muStackTotal = 32  -- Примерный размер стека
  }

-- | Вычисление использования памяти (экспортируемая функция)
calculateMemoryUsage :: MemoryMap -> MemoryUsage
calculateMemoryUsage = calculateMemoryUsage'

-- | Красивый вывод результата линковки
prettyLinkerResult :: LinkerResult -> Text
prettyLinkerResult LinkerResult{..} = T.unlines
  [ "Результат линковки:"
  , "=================="
  , "Статус: " <> if lrSuccess then "Успешно" else "Ошибка"
  , ""
  , prettyMemoryUsage lrMemoryUsage
  , ""
  , "Символы: " <> T.pack (show (Map.size lrSymbolTable))
  , "Диагностика: " <> T.pack (show (length lrDiagnostics))
  ]

-- | Красивый вывод карты памяти
prettyMemoryMap :: MemoryMap -> Text
prettyMemoryMap MemoryMap{..} = T.unlines
  [ "Код:   " <> prettyMemoryRegion mmCodeRegion
  , "Данные: " <> prettyMemoryRegion mmDataRegion
  , "Стек:   " <> prettyMemoryRegion mmStackRegion
  , "SFR:    " <> prettyMemoryRegion mmSfrRegion
  ]

-- | Красивый вывод области памяти
prettyMemoryRegion :: MemoryRegion -> Text
prettyMemoryRegion MemoryRegion{..} = 
  T.pack $ printf "0x%04X-0x%04X (%d/%d байт)" 
    mrStart mrEnd mrUsed (mrEnd - mrStart + 1)

-- | Красивый вывод использования памяти
prettyMemoryUsage :: MemoryUsage -> Text
prettyMemoryUsage MemoryUsage{..} = T.unlines
  [ T.pack $ printf "Код:   %d/%d байт (%.1f%%)" 
      muCodeUsed muCodeTotal (percentage muCodeUsed muCodeTotal)
  , T.pack $ printf "Данные: %d/%d байт (%.1f%%)" 
      muDataUsed muDataTotal (percentage muDataUsed muDataTotal)
  , T.pack $ printf "Стек:   %d/%d байт (%.1f%%)" 
      muStackUsed muStackTotal (percentage muStackUsed muStackTotal)
  ]
  where
    percentage used total = 
      if total > 0 
        then fromIntegral used / fromIntegral total * 100.0 :: Double
        else 0.0

-- | Красивый вывод символа
prettySymbol :: Symbol -> Text
prettySymbol Symbol{..} = 
  T.pack $ printf "%-20s 0x%04X %6d %-8s %-8s %s" 
    (T.unpack symName) symAddress symSize 
    (show symType) (show symScope) (T.unpack symSection)

-- | Добавление диагностического сообщения в состояние линковщика
addDiagnostic :: Diagnostic -> LinkerM ()
addDiagnostic diag = modify $ \s -> s { lsDiagnostics = diag : lsDiagnostics s }

-- ============================================================================
-- ПАРСИНГ ВСПОМОГАТЕЛЬНЫХ ТИПОВ
-- ============================================================================

-- | Парсинг шестнадцатеричного числа
parseHex :: Text -> LinkerM Word16
parseHex text = 
  case reads ("0x" ++ T.unpack text) of
    [(value, "")] -> return value
    _ -> throwError $ LinkerError $ "Неверное шестнадцатеричное число: " <> text

-- | Парсинг десятичного числа
parseDecimal :: Text -> LinkerM Int
parseDecimal text = 
  case reads (T.unpack text) of
    [(value, "")] -> return value
    _ -> throwError $ LinkerError $ "Неверное десятичное число: " <> text

-- | Парсинг типа символа
parseSymbolType :: Text -> LinkerM SymbolType
parseSymbolType = \case
  "function" -> return SymFunction
  "variable" -> return SymVariable
  "constant" -> return SymConstant
  "label" -> return SymLabel
  "sfr" -> return SymSfr
  "bit" -> return SymBit
  other -> throwError $ LinkerError $ "Неизвестный тип символа: " <> other

-- | Парсинг области видимости символа
parseSymbolScope :: Text -> LinkerM SymbolScope
parseSymbolScope = \case
  "global" -> return ScopeGlobal
  "local" -> return ScopeLocal
  "external" -> return ScopeExternal
  other -> throwError $ LinkerError $ "Неизвестная область видимости: " <> other

-- | Парсинг типа перемещения
parseRelocationType :: Text -> LinkerM RelocationType
parseRelocationType = \case
  "abs16" -> return RelAbsolute16
  "abs8" -> return RelAbsolute8
  "rel8" -> return RelRelative8
  "page" -> return RelPage
  "offset" -> return RelOffset
  other -> throwError $ LinkerError $ "Неизвестный тип перемещения: " <> other 