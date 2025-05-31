{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- | Модуль лексического анализатора для HCL компилятора
module HCL.Lexer
  ( -- * Основные функции
    tokenize
  , tokenizeWithPos
  
    -- * Типы токенов
  , Token(..)
  , TokenType(..)
  , Keyword(..)
  , Operator(..)
  , Delimiter(..)
  
    -- * Лексер
  , Lexer
  , LexerResult(..)
  
    -- * Утилиты
  , prettyToken
  , tokenPosition
  , isKeyword
  , isOperator
  ) where

import Control.Monad (void)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import HCL.Types (SourcePos(..), Literal(..), LiteralType(..), Identifier(..), mkIdentifier)
import HCL.Error (LexerError(..))

-- ============================================================================
-- ТИПЫ ТОКЕНОВ
-- ============================================================================

-- | Ключевые слова языка C и расширения для 8051
data Keyword
  -- Базовые типы данных
  = KVoid | KChar | KInt | KFloat | KDouble
  | KShort | KLong | KSigned | KUnsigned
  
  -- Модификаторы
  | KConst | KVolatile | KStatic | KExtern | KRegister | KAuto
  
  -- Управляющие конструкции
  | KIf | KElse | KSwitch | KCase | KDefault
  | KWhile | KFor | KDo | KBreak | KContinue
  | KReturn | KGoto
  
  -- Структуры и объединения
  | KStruct | KUnion | KEnum | KTypedef
  
  -- Размеры
  | KSizeof
  
  -- Расширения для 8051
  | KSbit     -- ^ Битовая переменная
  | KSfr      -- ^ Special Function Register
  | KSfr16    -- ^ 16-битный SFR
  | KData     -- ^ Внутренняя память данных
  | KIdata    -- ^ Косвенно адресуемая внутренняя память
  | KXdata    -- ^ Внешняя память данных
  | KCode     -- ^ Память программы
  | KBit      -- ^ Битовый тип
  | KInterrupt -- ^ Обработчик прерывания
  | KUsing    -- ^ Использование банка регистров
  | KReentrant -- ^ Реентерабельная функция
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Операторы
data Operator
  -- Арифметические
  = OpPlus | OpMinus | OpMul | OpDiv | OpMod
  
  -- Сравнения
  | OpEq | OpNe | OpLt | OpLe | OpGt | OpGe
  
  -- Логические
  | OpAnd | OpOr | OpNot
  
  -- Битовые
  | OpBitAnd | OpBitOr | OpBitXor | OpBitNot | OpShiftL | OpShiftR
  
  -- Присваивания
  | OpAssign | OpPlusAssign | OpMinusAssign | OpMulAssign | OpDivAssign
  | OpModAssign | OpAndAssign | OpOrAssign | OpXorAssign
  | OpShiftLAssign | OpShiftRAssign
  
  -- Инкремент/декремент
  | OpIncrement | OpDecrement
  
  -- Указатели
  | OpAddress | OpDeref | OpArrow
  
  -- Тернарный
  | OpQuestion | OpColon
  
  -- Доступ к элементам
  | OpDot
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Разделители
data Delimiter
  = DelimLParen | DelimRParen     -- ( )
  | DelimLBrace | DelimRBrace     -- { }
  | DelimLBracket | DelimRBracket -- [ ]
  | DelimSemicolon                -- ;
  | DelimComma                    -- ,
  | DelimHash                     -- # (для препроцессора)
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Тип токена
data TokenType
  = TKeyword !Keyword
  | TIdentifier !Identifier
  | TLiteral !Literal
  | TOperator !Operator
  | TDelimiter !Delimiter
  | TEOF  -- Конец файла
  deriving (Eq, Show)

-- | Токен с позицией в исходном коде
data Token = Token
  { tokenType :: !TokenType
  , tokenPos :: !SourcePos
  , tokenText :: !Text  -- Оригинальный текст токена
  } deriving (Eq, Show)

-- ============================================================================
-- ЛЕКСЕР И РЕЗУЛЬТАТЫ
-- ============================================================================

-- | Тип лексера на основе Megaparsec
type Lexer = Parsec Void Text

-- | Результат лексического анализа
data LexerResult = LexerResult
  { lrTokens :: ![Token]
  , lrErrors :: ![LexerError]
  , lrSuccess :: !Bool
  } deriving (Eq, Show)

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ ДЛЯ ПАРСИНГА
-- ============================================================================

-- | Пропускает пробелы и комментарии
spaceConsumer :: Lexer ()
spaceConsumer = L.space
  space1
  (L.skipLineComment "//")
  (L.skipBlockComment "/*" "*/")

-- | Лексема - токен с пропуском пробелов после
lexeme :: Lexer a -> Lexer a
lexeme = L.lexeme spaceConsumer

-- | Символ с пропуском пробелов
symbol :: Text -> Lexer Text
symbol = L.symbol spaceConsumer

-- | Получает текущую позицию как SourcePos
getCurrentSourcePos :: Lexer SourcePos
getCurrentSourcePos = do
  pos <- getSourcePos
  return $ SourcePos "input" (unPos $ sourceLine pos) (unPos $ sourceColumn pos)

-- ============================================================================
-- ПАРСЕРЫ КЛЮЧЕВЫХ СЛОВ
-- ============================================================================

-- | Таблица ключевых слов
keywordTable :: [(Text, Keyword)]
keywordTable =
  [ -- Базовые типы
    ("void", KVoid), ("char", KChar), ("int", KInt)
  , ("float", KFloat), ("double", KDouble)
  , ("short", KShort), ("long", KLong)
  , ("signed", KSigned), ("unsigned", KUnsigned)
  
    -- Модификаторы
  , ("const", KConst), ("volatile", KVolatile)
  , ("static", KStatic), ("extern", KExtern)
  , ("register", KRegister), ("auto", KAuto)
  
    -- Управляющие конструкции
  , ("if", KIf), ("else", KElse)
  , ("switch", KSwitch), ("case", KCase), ("default", KDefault)
  , ("while", KWhile), ("for", KFor), ("do", KDo)
  , ("break", KBreak), ("continue", KContinue)
  , ("return", KReturn), ("goto", KGoto)
  
    -- Структуры
  , ("struct", KStruct), ("union", KUnion)
  , ("enum", KEnum), ("typedef", KTypedef)
  
    -- Размеры
  , ("sizeof", KSizeof)
  
    -- Расширения для 8051
  , ("sbit", KSbit), ("sfr", KSfr), ("sfr16", KSfr16)
  , ("data", KData), ("idata", KIdata), ("xdata", KXdata)
  , ("code", KCode), ("bit", KBit)
  , ("interrupt", KInterrupt), ("using", KUsing)
  , ("reentrant", KReentrant)
  ]

-- | Парсер идентификаторов и ключевых слов
parseIdentifierOrKeyword :: Lexer TokenType
parseIdentifierOrKeyword = do
  first <- letterChar <|> char '_'
  rest <- many (alphaNumChar <|> char '_')
  let word = T.pack (first : rest)
  case lookup word keywordTable of
    Just kw -> return $ TKeyword kw
    Nothing -> case mkIdentifier word of
      Just ident -> return $ TIdentifier ident
      Nothing -> fail $ "Invalid identifier: " ++ T.unpack word

-- ============================================================================
-- ПАРСЕРЫ ОПЕРАТОРОВ
-- ============================================================================

-- | Парсер операторов
parseOperator :: Lexer Operator
parseOperator = choice
  [ -- Составные операторы (длинные сначала)
    try (string "<<=") >> return OpShiftLAssign
  , try (string ">>=") >> return OpShiftRAssign
  , try (string "+=") >> return OpPlusAssign
  , try (string "-=") >> return OpMinusAssign
  , try (string "*=") >> return OpMulAssign
  , try (string "/=") >> return OpDivAssign
  , try (string "%=") >> return OpModAssign
  , try (string "&=") >> return OpAndAssign
  , try (string "|=") >> return OpOrAssign
  , try (string "^=") >> return OpXorAssign
  
    -- Двухсимвольные операторы
  , try (string "==") >> return OpEq
  , try (string "!=") >> return OpNe
  , try (string "<=") >> return OpLe
  , try (string ">=") >> return OpGe
  , try (string "&&") >> return OpAnd
  , try (string "||") >> return OpOr
  , try (string "<<") >> return OpShiftL
  , try (string ">>") >> return OpShiftR
  , try (string "++") >> return OpIncrement
  , try (string "--") >> return OpDecrement
  , try (string "->") >> return OpArrow
  
    -- Односимвольные операторы
  , char '+' >> return OpPlus
  , char '-' >> return OpMinus
  , char '*' >> return OpMul
  , char '/' >> return OpDiv
  , char '%' >> return OpMod
  , char '<' >> return OpLt
  , char '>' >> return OpGt
  , char '!' >> return OpNot
  , char '~' >> return OpBitNot
  , char '&' >> return OpBitAnd
  , char '|' >> return OpBitOr
  , char '^' >> return OpBitXor
  , char '=' >> return OpAssign
  , char '?' >> return OpQuestion
  , char ':' >> return OpColon
  , char '.' >> return OpDot
  ]

-- ============================================================================
-- ПАРСЕРЫ РАЗДЕЛИТЕЛЕЙ
-- ============================================================================

-- | Парсер разделителей
parseDelimiter :: Lexer Delimiter
parseDelimiter = choice
  [ char '(' >> return DelimLParen
  , char ')' >> return DelimRParen
  , char '{' >> return DelimLBrace
  , char '}' >> return DelimRBrace
  , char '[' >> return DelimLBracket
  , char ']' >> return DelimRBracket
  , char ';' >> return DelimSemicolon
  , char ',' >> return DelimComma
  , char '#' >> return DelimHash
  ]

-- ============================================================================
-- ПАРСЕРЫ ЛИТЕРАЛОВ
-- ============================================================================

-- | Парсер целых чисел
parseIntLiteral :: Lexer Literal
parseIntLiteral = do
  numStr <- choice
    [ try $ do
        _ <- string "0x" <|> string "0X"
        digits <- some hexDigitChar
        return $ "0x" ++ digits
    , try $ do
        _ <- char '0'
        digits <- some octDigitChar
        return $ "0" ++ digits
    , some digitChar
    ]
  case reads numStr of
    [(n, "")] -> return $ IntLiteral n NumberLiteral
    _ -> fail $ "Invalid number: " ++ numStr

-- | Парсер символьных литералов
parseCharLiteral :: Lexer Literal
parseCharLiteral = do
  _ <- char '\''
  c <- choice
    [ try $ char '\\' >> parseEscapeChar
    , satisfy (/= '\'')
    ]
  _ <- char '\''
  return $ CharLit c

-- | Парсер escape-последовательностей
parseEscapeChar :: Lexer Char
parseEscapeChar = choice
  [ char 'n' >> return '\n'
  , char 't' >> return '\t'
  , char 'r' >> return '\r'
  , char '\\' >> return '\\'
  , char '\'' >> return '\''
  , char '\"' >> return '\"'
  , char '0' >> return '\0'
  ]

-- | Парсер строковых литералов
parseStringLiteral :: Lexer Literal
parseStringLiteral = do
  _ <- char '\"'
  str <- many $ choice
    [ try $ char '\\' >> parseEscapeChar
    , satisfy (/= '\"')
    ]
  _ <- char '\"'
  return $ StringLit (T.pack str)

-- ============================================================================
-- ОСНОВНОЙ ПАРСЕР ТОКЕНОВ
-- ============================================================================

-- | Парсер одного токена
parseToken :: Lexer Token
parseToken = do
  pos <- getCurrentSourcePos
  tokenType <- choice
    [ -- Литералы (проверяем первыми, чтобы избежать конфликтов)
      TLiteral <$> choice
        [ try parseIntLiteral
        , try parseCharLiteral
        , parseStringLiteral
        ]
    
    -- Идентификаторы и ключевые слова
    , parseIdentifierOrKeyword
    
    -- Операторы
    , TOperator <$> parseOperator
    
    -- Разделители
    , TDelimiter <$> parseDelimiter
    ]
  
  -- Получаем оригинальный текст токена (упрощенно)
  let tokenText = case tokenType of
        TKeyword kw -> T.pack $ show kw
        TIdentifier (Identifier txt) -> txt
        TLiteral lit -> T.pack $ show lit
        TOperator op -> T.pack $ show op
        TDelimiter delim -> T.pack $ show delim
        TEOF -> ""
  
  return $ Token tokenType pos tokenText

-- | Парсер всех токенов
parseTokens :: Lexer [Token]
parseTokens = do
  spaceConsumer  -- Пропускаем начальные пробелы
  tokens <- many $ lexeme parseToken
  eof
  pos <- getCurrentSourcePos
  return $ tokens ++ [Token TEOF pos ""]

-- ============================================================================
-- ПУБЛИЧНЫЙ API
-- ============================================================================

-- | Основная функция токенизации
tokenize :: Text -> LexerResult
tokenize input = 
  case runParser parseTokens "input" input of
    Left err -> LexerResult
      { lrTokens = []
      , lrErrors = [parseErrorToLexerError err]
      , lrSuccess = False
      }
    Right tokens -> LexerResult
      { lrTokens = tokens
      , lrErrors = []
      , lrSuccess = True
      }

-- | Токенизация с сохранением позиций
tokenizeWithPos :: FilePath -> Text -> LexerResult
tokenizeWithPos filename input =
  case runParser parseTokens filename input of
    Left err -> LexerResult
      { lrTokens = []
      , lrErrors = [parseErrorToLexerError err]
      , lrSuccess = False
      }
    Right tokens -> LexerResult
      { lrTokens = tokens
      , lrErrors = []
      , lrSuccess = True
      }

-- ============================================================================
-- УТИЛИТЫ
-- ============================================================================

-- | Преобразует ошибку парсера в ошибку лексера
parseErrorToLexerError :: ParseErrorBundle Text Void -> LexerError
parseErrorToLexerError bundle =
  let pos = bundlePos bundle
      sourcePos = SourcePos "input" (unPos $ sourceLine pos) (unPos $ sourceColumn pos)
  in UnexpectedCharacter '?' sourcePos  -- Упрощение

-- | Красивое отображение токена
prettyToken :: Token -> Text
prettyToken Token{..} = 
  T.concat [T.pack $ show tokenType, " at ", T.pack $ show tokenPos]

-- | Получает позицию токена
tokenPosition :: Token -> SourcePos
tokenPosition = tokenPos

-- | Проверяет, является ли токен ключевым словом
isKeyword :: Token -> Bool
isKeyword Token{tokenType = TKeyword _} = True
isKeyword _ = False

-- | Проверяет, является ли токен оператором
isOperator :: Token -> Bool
isOperator Token{tokenType = TOperator _} = True
isOperator _ = False 