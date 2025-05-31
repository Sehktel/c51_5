{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

-- | Модуль синтаксического анализатора для HCL компилятора
--
-- Этот модуль реализует recursive descent парсер, который преобразует
-- поток токенов в абстрактное синтаксическое дерево (AST). Парсер
-- поддерживает полный синтаксис C с расширениями для AT89S4051.
--
-- Архитектурные принципы:
-- * Использование Megaparsec для композиции парсеров
-- * Обработка приоритетов операторов
-- * Восстановление после ошибок
-- * Сохранение позиций в исходном коде
module HCL.Parser
  ( -- * Основные функции парсинга
    parseProgram
  , parseExpression
  , parseStatement
  
    -- * Парсер и результаты
  , Parser
  , ParseResult(..)
  , ParseError(..)
  
    -- * Вспомогательные парсеры
  , parseType
  , parseDeclaration
  , parseFunctionDef
  
    -- * Утилиты
  , runParser
  , prettyParseError
  ) where

import Control.Monad (void, when)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Void (Void)
import Data.Maybe (fromMaybe, isJust)
import Text.Megaparsec hiding (Token, runParser)
import qualified Text.Megaparsec as MP
import Text.Megaparsec.Char

import HCL.Types (SourcePos(..), Identifier(..), Literal(..), LiteralType(..), 
                  BinaryOp(..), UnaryOp(..), AssignOp(..), mkIdentifier)
import HCL.Lexer (Token(..), TokenType(..), Keyword(..), Operator(..), Delimiter(..), 
                  LexerResult(..), tokenize)
import HCL.AST
import HCL.Error (CompilerError(..), ParseError(..), mkError)

-- ============================================================================
-- ТИПЫ ПАРСЕРА
-- ============================================================================

-- | Тип парсера на основе Megaparsec
type Parser = Parsec Void [Token]

-- | Результат парсинга
data ParseResult a = ParseResult
  { prResult :: !(Either ParseError a)  -- ^ Результат или ошибка
  , prTokensConsumed :: !Int            -- ^ Количество обработанных токенов
  , prSuccess :: !Bool                  -- ^ Успешность парсинга
  } deriving (Eq, Show)

-- ============================================================================
-- ВСПОМОГАТЕЛЬНЫЕ ФУНКЦИИ
-- ============================================================================

-- | Запускает парсер на списке токенов
runParser :: Parser a -> [Token] -> ParseResult a
runParser parser tokens =
  case MP.runParser parser "input" tokens of
    Left err -> ParseResult
      { prResult = Left (MegaparsecError (T.pack $ errorBundlePretty err))
      , prTokensConsumed = 0
      , prSuccess = False
      }
    Right result -> ParseResult
      { prResult = Right result
      , prTokensConsumed = length tokens
      , prSuccess = True
      }

-- | Получает позицию из токена
tokenPos :: Token -> SourcePos
tokenPos = tokenPos

-- | Парсер конкретного токена
token :: (TokenType -> Maybe a) -> Parser a
token test = MP.token testToken Nothing
  where
    testToken tok = case test (tokenType tok) of
      Just a -> Just a
      Nothing -> Nothing

-- | Парсер ключевого слова
keyword :: Keyword -> Parser ()
keyword kw = void $ token $ \case
  TKeyword k | k == kw -> Just ()
  _ -> Nothing

-- | Парсер оператора
operator :: Operator -> Parser ()
operator op = void $ token $ \case
  TOperator o | o == op -> Just ()
  _ -> Nothing

-- | Парсер разделителя
delimiter :: Delimiter -> Parser ()
delimiter delim = void $ token $ \case
  TDelimiter d | d == delim -> Just ()
  _ -> Nothing

-- | Парсер идентификатора
identifier :: Parser Identifier
identifier = token $ \case
  TIdentifier ident -> Just ident
  _ -> Nothing

-- | Парсер литерала
literal :: Parser Literal
literal = token $ \case
  TLiteral lit -> Just lit
  _ -> Nothing

-- | Парсер в скобках
parens :: Parser a -> Parser a
parens p = delimiter DelimLParen *> p <* delimiter DelimRParen

-- | Парсер в фигурных скобках
braces :: Parser a -> Parser a
braces p = delimiter DelimLBrace *> p <* delimiter DelimRBrace

-- | Парсер в квадратных скобках
brackets :: Parser a -> Parser a
brackets p = delimiter DelimLBracket *> p <* delimiter DelimRBracket

-- | Парсер списка, разделенного запятыми
commaSep :: Parser a -> Parser [a]
commaSep p = sepBy p (delimiter DelimComma)

-- | Парсер списка, разделенного запятыми (не пустой)
commaSep1 :: Parser a -> Parser [a]
commaSep1 p = sepBy1 p (delimiter DelimComma)

-- | Получает текущую позицию
getCurrentPos :: Parser SourcePos
getCurrentPos = do
  pos <- getSourcePos
  return $ SourcePos "input" (unPos $ sourceLine pos) (unPos $ sourceColumn pos)

-- ============================================================================
-- ПАРСЕРЫ ТИПОВ
-- ============================================================================

-- | Парсер спецификации типа
parseType :: Parser TypeSpec
parseType = choice
  [ keyword KVoid >> return TSVoid
  , keyword KChar >> return TSChar
  , keyword KInt >> return TSInt
  , keyword KFloat >> return TSFloat
  , keyword KDouble >> return TSDouble
  , keyword KShort >> return TSShort
  , keyword KLong >> return TSLong
  , keyword KSigned >> return TSSigned
  , keyword KUnsigned >> return TSUnsigned
  , keyword KBit >> return TSBit
  , parseStructType
  , parseUnionType
  , parseEnumType
  , parseTypedefType
  ] >>= parseTypeModifiers
  where
    parseTypeModifiers baseType = choice
      [ do
          delimiter DelimMul
          parseTypeModifiers (TSPointer baseType)
      , do
          size <- brackets (optional parseExpression)
          parseTypeModifiers (TSArray baseType size)
      , return baseType
      ]

-- | Парсер структуры
parseStructType :: Parser TypeSpec
parseStructType = do
  keyword KStruct
  name <- identifier
  fields <- optional $ braces (many parseDeclaration)
  return $ TSStruct name (fromMaybe [] fields)

-- | Парсер объединения
parseUnionType :: Parser TypeSpec
parseUnionType = do
  keyword KUnion
  name <- identifier
  fields <- optional $ braces (many parseDeclaration)
  return $ TSUnion name (fromMaybe [] fields)

-- | Парсер перечисления
parseEnumType :: Parser TypeSpec
parseEnumType = do
  keyword KEnum
  name <- identifier
  values <- optional $ braces (commaSep1 identifier)
  return $ TSEnum name (fromMaybe [] values)

-- | Парсер пользовательского типа
parseTypedefType :: Parser TypeSpec
parseTypedefType = TSTypedef <$> identifier

-- | Парсер класса хранения
parseStorageClass :: Parser StorageClass
parseStorageClass = choice
  [ keyword KStatic >> return SCStatic
  , keyword KExtern >> return SCExtern
  , keyword KRegister >> return SCRegister
  , keyword KAuto >> return SCAuto
  ]

-- | Парсер квалификатора типа
parseTypeQualifier :: Parser TypeQualifier
parseTypeQualifier = choice
  [ keyword KConst >> return TQConst
  , keyword KVolatile >> return TQVolatile
  ]

-- | Парсер модели памяти (расширение для 8051)
parseMemoryModel :: Parser MemoryModel
parseMemoryModel = choice
  [ keyword KData >> return MMData
  , keyword KIdata >> return MMIdata
  , keyword KXdata >> return MMXdata
  , keyword KCode >> return MMCode
  ]

-- ============================================================================
-- ПАРСЕРЫ ВЫРАЖЕНИЙ
-- ============================================================================

-- | Парсер выражения с учетом приоритетов операторов
parseExpression :: Parser Expr
parseExpression = makeExprParser parseTerm operatorTable
  where
    operatorTable =
      [ -- Постфиксные операторы (наивысший приоритет)
        [ Postfix $ do
            args <- parens (commaSep parseExpression)
            pos <- getCurrentPos
            return $ \expr -> case expr of
              Expr (EVariable name) _ -> Expr (ECall name args) pos
              _ -> expr  -- Ошибка, но пропускаем для простоты
        , Postfix $ do
            index <- brackets parseExpression
            pos <- getCurrentPos
            return $ \expr -> Expr (EArrayAccess expr index) pos
        , Postfix $ do
            delimiter DelimDot
            field <- identifier
            pos <- getCurrentPos
            return $ \expr -> Expr (EFieldAccess expr field) pos
        ]
      
      -- Унарные операторы
      , [ Prefix $ do
            operator OpIncrement
            pos <- getCurrentPos
            return $ \expr -> Expr (EUnary PreIncrement expr) pos
        , Prefix $ do
            operator OpDecrement
            pos <- getCurrentPos
            return $ \expr -> Expr (EUnary PreDecrement expr) pos
        , Prefix $ do
            operator OpPlus
            pos <- getCurrentPos
            return $ \expr -> Expr (EUnary UnaryPlus expr) pos
        , Prefix $ do
            operator OpMinus
            pos <- getCurrentPos
            return $ \expr -> Expr (EUnary UnaryMinus expr) pos
        , Prefix $ do
            operator OpNot
            pos <- getCurrentPos
            return $ \expr -> Expr (EUnary LogicalNot expr) pos
        , Prefix $ do
            operator OpBitNot
            pos <- getCurrentPos
            return $ \expr -> Expr (EUnary BitwiseNot expr) pos
        , Prefix $ do
            operator OpAddress
            pos <- getCurrentPos
            return $ \expr -> Expr (EUnary AddressOf expr) pos
        , Prefix $ do
            operator OpDeref
            pos <- getCurrentPos
            return $ \expr -> Expr (EUnary Dereference expr) pos
        ]
      
      -- Мультипликативные операторы
      , [ InfixL $ do
            operator OpMul
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary Mul left right) pos
        , InfixL $ do
            operator OpDiv
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary Div left right) pos
        , InfixL $ do
            operator OpMod
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary Mod left right) pos
        ]
      
      -- Аддитивные операторы
      , [ InfixL $ do
            operator OpPlus
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary Add left right) pos
        , InfixL $ do
            operator OpMinus
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary Sub left right) pos
        ]
      
      -- Операторы сдвига
      , [ InfixL $ do
            operator OpShiftL
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary ShiftLeft left right) pos
        , InfixL $ do
            operator OpShiftR
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary ShiftRight left right) pos
        ]
      
      -- Операторы сравнения
      , [ InfixL $ do
            operator OpLt
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary Lt left right) pos
        , InfixL $ do
            operator OpLe
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary Le left right) pos
        , InfixL $ do
            operator OpGt
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary Gt left right) pos
        , InfixL $ do
            operator OpGe
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary Ge left right) pos
        ]
      
      -- Операторы равенства
      , [ InfixL $ do
            operator OpEq
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary Eq left right) pos
        , InfixL $ do
            operator OpNe
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary Ne left right) pos
        ]
      
      -- Битовые операторы
      , [ InfixL $ do
            operator OpBitAnd
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary BitwiseAnd left right) pos
        ]
      , [ InfixL $ do
            operator OpBitXor
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary BitwiseXor left right) pos
        ]
      , [ InfixL $ do
            operator OpBitOr
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary BitwiseOr left right) pos
        ]
      
      -- Логические операторы
      , [ InfixL $ do
            operator OpAnd
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary LogicalAnd left right) pos
        ]
      , [ InfixL $ do
            operator OpOr
            pos <- getCurrentPos
            return $ \left right -> Expr (EBinary LogicalOr left right) pos
        ]
      
      -- Тернарный оператор
      , [ InfixR $ do
            operator OpQuestion
            thenExpr <- parseExpression
            operator OpColon
            pos <- getCurrentPos
            return $ \condExpr elseExpr -> Expr (ETernary condExpr thenExpr elseExpr) pos
        ]
      
      -- Операторы присваивания (наименьший приоритет)
      , [ InfixR $ do
            operator OpAssign
            pos <- getCurrentPos
            return $ \left right -> Expr (EAssign SimpleAssign left right) pos
        , InfixR $ do
            operator OpPlusAssign
            pos <- getCurrentPos
            return $ \left right -> Expr (EAssign AddAssign left right) pos
        , InfixR $ do
            operator OpMinusAssign
            pos <- getCurrentPos
            return $ \left right -> Expr (EAssign SubAssign left right) pos
        ]
      ]

-- | Парсер терминальных выражений
parseTerm :: Parser Expr
parseTerm = choice
  [ parseParenExpr
  , parseLiteralExpr
  , parseVariableExpr
  , parseSizeofExpr
  , parseCastExpr
  ]

-- | Парсер выражения в скобках
parseParenExpr :: Parser Expr
parseParenExpr = parens parseExpression

-- | Парсер литерального выражения
parseLiteralExpr :: Parser Expr
parseLiteralExpr = do
  lit <- literal
  pos <- getCurrentPos
  return $ Expr (ELiteral lit) pos

-- | Парсер переменной
parseVariableExpr :: Parser Expr
parseVariableExpr = do
  name <- identifier
  pos <- getCurrentPos
  return $ Expr (EVariable name) pos

-- | Парсер sizeof
parseSizeofExpr :: Parser Expr
parseSizeofExpr = do
  keyword KSizeof
  pos <- getCurrentPos
  typeSpec <- parens parseType
  return $ Expr (ESizeOf typeSpec) pos

-- | Парсер приведения типа
parseCastExpr :: Parser Expr
parseCastExpr = do
  typeSpec <- parens parseType
  expr <- parseTerm
  pos <- getCurrentPos
  return $ Expr (ECast typeSpec expr) pos

-- ============================================================================
-- ПАРСЕРЫ ОПЕРАТОРОВ
-- ============================================================================

-- | Парсер оператора
parseStatement :: Parser Stmt
parseStatement = choice
  [ parseExprStmt
  , parseCompoundStmt
  , parseIfStmt
  , parseWhileStmt
  , parseForStmt
  , parseDoWhileStmt
  , parseSwitchStmt
  , parseBreakStmt
  , parseContinueStmt
  , parseReturnStmt
  , parseGotoStmt
  , parseLabelStmt
  , parseDeclarationStmt
  ]

-- | Парсер оператора-выражения
parseExprStmt :: Parser Stmt
parseExprStmt = do
  expr <- parseExpression
  delimiter DelimSemicolon
  pos <- getCurrentPos
  return $ Stmt (SExpr expr) pos

-- | Парсер составного оператора
parseCompoundStmt :: Parser Stmt
parseCompoundStmt = do
  pos <- getCurrentPos
  compound <- braces $ do
    stmts <- many parseStatement
    compoundPos <- getCurrentPos
    return $ CompoundStmt stmts compoundPos
  return $ Stmt (SCompound compound) pos

-- | Парсер if-оператора
parseIfStmt :: Parser Stmt
parseIfStmt = do
  keyword KIf
  condition <- parens parseExpression
  thenStmt <- parseStatement
  elseStmt <- optional $ keyword KElse >> parseStatement
  pos <- getCurrentPos
  return $ Stmt (SIf condition thenStmt elseStmt) pos

-- | Парсер while-оператора
parseWhileStmt :: Parser Stmt
parseWhileStmt = do
  keyword KWhile
  condition <- parens parseExpression
  body <- parseStatement
  pos <- getCurrentPos
  return $ Stmt (SWhile condition body) pos

-- | Парсер for-оператора
parseForStmt :: Parser Stmt
parseForStmt = do
  keyword KFor
  (init, condition, increment) <- parens $ do
    init <- optional parseExpression
    delimiter DelimSemicolon
    condition <- optional parseExpression
    delimiter DelimSemicolon
    increment <- optional parseExpression
    return (init, condition, increment)
  body <- parseStatement
  pos <- getCurrentPos
  return $ Stmt (SFor init condition increment body) pos

-- | Парсер do-while оператора
parseDoWhileStmt :: Parser Stmt
parseDoWhileStmt = do
  keyword KDo
  body <- parseStatement
  keyword KWhile
  condition <- parens parseExpression
  delimiter DelimSemicolon
  pos <- getCurrentPos
  return $ Stmt (SDoWhile body condition) pos

-- | Парсер switch-оператора
parseSwitchStmt :: Parser Stmt
parseSwitchStmt = do
  keyword KSwitch
  expr <- parens parseExpression
  cases <- braces (many parseCaseStmt)
  pos <- getCurrentPos
  return $ Stmt (SSwitch expr cases) pos

-- | Парсер case-ветки
parseCaseStmt :: Parser CaseStmt
parseCaseStmt = choice
  [ do
      keyword KCase
      value <- parseExpression
      operator OpColon
      stmts <- many parseStatement
      pos <- getCurrentPos
      return $ CaseStmt value stmts pos
  , do
      keyword KDefault
      operator OpColon
      stmts <- many parseStatement
      pos <- getCurrentPos
      return $ DefaultStmt stmts pos
  ]

-- | Парсер break-оператора
parseBreakStmt :: Parser Stmt
parseBreakStmt = do
  keyword KBreak
  delimiter DelimSemicolon
  pos <- getCurrentPos
  return $ Stmt SBreak pos

-- | Парсер continue-оператора
parseContinueStmt :: Parser Stmt
parseContinueStmt = do
  keyword KContinue
  delimiter DelimSemicolon
  pos <- getCurrentPos
  return $ Stmt SContinue pos

-- | Парсер return-оператора
parseReturnStmt :: Parser Stmt
parseReturnStmt = do
  keyword KReturn
  expr <- optional parseExpression
  delimiter DelimSemicolon
  pos <- getCurrentPos
  return $ Stmt (SReturn expr) pos

-- | Парсер goto-оператора
parseGotoStmt :: Parser Stmt
parseGotoStmt = do
  keyword KGoto
  label <- identifier
  delimiter DelimSemicolon
  pos <- getCurrentPos
  return $ Stmt (SGoto label) pos

-- | Парсер метки
parseLabelStmt :: Parser Stmt
parseLabelStmt = do
  label <- identifier
  operator OpColon
  stmt <- parseStatement
  pos <- getCurrentPos
  return $ Stmt (SLabel label stmt) pos

-- | Парсер декларации как оператора
parseDeclarationStmt :: Parser Stmt
parseDeclarationStmt = do
  decl <- parseDeclaration
  pos <- getCurrentPos
  return $ Stmt (SDecl decl) pos

-- ============================================================================
-- ПАРСЕРЫ ДЕКЛАРАЦИЙ
-- ============================================================================

-- | Парсер декларации
parseDeclaration :: Parser Declaration
parseDeclaration = do
  storageClass <- optional parseStorageClass
  qualifiers <- many parseTypeQualifier
  memoryModel <- optional parseMemoryModel
  typeSpec <- parseType
  name <- identifier
  init <- optional parseInitializer
  delimiter DelimSemicolon
  pos <- getCurrentPos
  return $ Declaration
    { declType = typeSpec
    , declName = name
    , declInit = init
    , declStorageClass = storageClass
    , declQualifiers = qualifiers
    , declMemoryModel = memoryModel
    , declPos = pos
    }

-- | Парсер инициализатора
parseInitializer :: Parser Initializer
parseInitializer = do
  operator OpAssign
  choice
    [ parseInitList
    , InitExpr <$> parseExpression
    ]
  where
    parseInitList = do
      inits <- braces (commaSep parseInitializer)
      return $ InitList inits

-- | Парсер определения функции
parseFunctionDef :: Parser FunctionDef
parseFunctionDef = do
  storageClass <- optional parseStorageClass
  memoryModel <- optional parseMemoryModel
  returnType <- parseType
  name <- identifier
  params <- parens (commaSep parseParameter)
  interrupt <- optional parseInterruptSpec
  body <- parseCompoundStmt
  pos <- getCurrentPos
  
  -- Извлекаем CompoundStmt из Stmt
  compoundBody <- case body of
    Stmt (SCompound compound) _ -> return compound
    _ -> fail "Expected compound statement for function body"
  
  return $ FunctionDef
    { funcName = name
    , funcReturnType = returnType
    , funcParams = params
    , funcBody = compoundBody
    , funcStorageClass = storageClass
    , funcInterrupt = interrupt
    , funcMemoryModel = memoryModel
    , funcPos = pos
    }

-- | Парсер параметра функции
parseParameter :: Parser Parameter
parseParameter = do
  qualifiers <- many parseTypeQualifier
  memoryModel <- optional parseMemoryModel
  typeSpec <- parseType
  name <- optional identifier
  pos <- getCurrentPos
  return $ Parameter
    { paramType = typeSpec
    , paramName = name
    , paramQualifiers = qualifiers
    , paramMemoryModel = memoryModel
    , paramPos = pos
    }

-- | Парсер спецификации прерывания (расширение для 8051)
parseInterruptSpec :: Parser InterruptSpec
parseInterruptSpec = do
  keyword KInterrupt
  number <- token $ \case
    TLiteral (IntLiteral n _) -> Just n
    _ -> Nothing
  using <- optional $ do
    keyword KUsing
    token $ \case
      TLiteral (IntLiteral n _) -> Just n
      _ -> Nothing
  reentrant <- isJust <$> optional (keyword KReentrant)
  pos <- getCurrentPos
  return $ InterruptSpec
    { intNumber = number
    , intUsing = using
    , intReentrant = reentrant
    , intPos = pos
    }

-- ============================================================================
-- ПАРСЕРЫ ВЕРХНЕГО УРОВНЯ
-- ============================================================================

-- | Парсер декларации верхнего уровня
parseTopLevelDecl :: Parser TopLevelDecl
parseTopLevelDecl = choice
  [ TLFunction <$> try parseFunctionDef
  , TLBitDecl <$> parseBitDeclaration
  , TLSfrDecl <$> parseSfrDeclaration
  , TLDeclaration <$> parseDeclaration
  ]

-- | Парсер битовой декларации (sbit)
parseBitDeclaration :: Parser BitDeclaration
parseBitDeclaration = do
  keyword KSbit
  name <- identifier
  operator OpAssign
  address <- parseExpression
  delimiter DelimSemicolon
  pos <- getCurrentPos
  return $ BitDeclaration
    { bitName = name
    , bitAddress = address
    , bitPos = pos
    }

-- | Парсер SFR декларации
parseSfrDeclaration :: Parser SfrDeclaration
parseSfrDeclaration = do
  keyword KSfr
  name <- identifier
  operator OpAssign
  address <- parseExpression
  delimiter DelimSemicolon
  pos <- getCurrentPos
  return $ SfrDeclaration
    { sfrName = name
    , sfrAddress = address
    , sfrType = TSUnsigned  -- По умолчанию unsigned char
    , sfrPos = pos
    }

-- | Парсер программы
parseProgram :: Parser Program
parseProgram = do
  decls <- many parseTopLevelDecl
  eof
  pos <- getCurrentPos
  return $ Program decls pos

-- ============================================================================
-- УТИЛИТЫ
-- ============================================================================

-- | Красивое отображение ошибки парсинга
prettyParseError :: ParseError -> Text
prettyParseError = \case
  MegaparsecError msg -> "Parse error: " <> msg
  UnexpectedToken expected actual pos ->
    "Unexpected token '" <> actual <> "', expected '" <> expected <> 
    "' at " <> T.pack (show pos)
  InvalidSyntax msg pos ->
    "Invalid syntax: " <> msg <> " at " <> T.pack (show pos) 