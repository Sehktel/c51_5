-- | Модуль системы обработки ошибок для HCL компилятора
--
-- Этот модуль предоставляет иерархию типов ошибок для всех фаз
-- компиляции, а также монады для обработки ошибок и предупреждений.
-- Использует современные подходы Haskell для обработки ошибок.
module HCL.Error
  ( -- * Типы ошибок
    CompilerError(..)
  , LexerError(..)
  , ParserError(..)
  , SemanticError(..)
  , CodeGenError(..)
  , ErrorSeverity(..)
  
    -- * Диагностические сообщения
  , Diagnostic(..)
  , DiagnosticLevel(..)
  , mkDiagnostic
  , mkError
  , mkWarning
  , mkInfo
  
    -- * Монады для обработки ошибок
  , CompilerM
  , runCompilerM
  , throwCompilerError
  , addDiagnostic
  , addWarning
  , addInfo
  , collectDiagnostics
  
    -- * Утилиты
  , prettyError
  , prettyDiagnostic
  , formatErrorWithContext
  ) where

import Control.Monad.Except (ExceptT, MonadError(..), runExceptT)
import Control.Monad.Writer (WriterT, MonadWriter(..), runWriterT)
import Control.Monad.IO.Class (MonadIO)
import Data.Text (Text)
import qualified Data.Text as T
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NE

import HCL.Types (SourcePos, SourceSpan, Identifier)

-- ============================================================================
-- ТИПЫ ОШИБОК
-- ============================================================================

-- | Основной тип ошибок компилятора
data CompilerError
  = LexError LexerError
  | ParseError ParserError  
  | SemanticError SemanticError
  | CodeGenError CodeGenError
  | IOError Text SourcePos  -- ^ Ошибки ввода-вывода
  | InternalError Text SourcePos  -- ^ Внутренние ошибки компилятора
  deriving (Eq, Show)

-- | Ошибки лексического анализа
data LexerError
  = UnexpectedCharacter Char SourcePos
    -- ^ Неожиданный символ в исходном коде
  | UnterminatedString SourcePos
    -- ^ Незакрытая строка
  | UnterminatedComment SourcePos
    -- ^ Незакрытый комментарий
  | InvalidNumber Text SourcePos
    -- ^ Некорректное число
  | InvalidCharLiteral Text SourcePos
    -- ^ Некорректный символьный литерал
  | InvalidEscapeSequence Text SourcePos
    -- ^ Некорректная escape-последовательность
  deriving (Eq, Show)

-- | Ошибки синтаксического анализа
data ParserError
  = UnexpectedToken Text Text SourcePos
    -- ^ Неожиданный токен (ожидался, получен, позиция)
  | UnexpectedEndOfFile SourcePos
    -- ^ Неожиданный конец файла
  | MissingToken Text SourcePos
    -- ^ Отсутствующий токен
  | InvalidSyntax Text SourcePos
    -- ^ Некорректный синтаксис
  | AmbiguousExpression Text SourcePos
    -- ^ Неоднозначное выражение
  deriving (Eq, Show)

-- | Ошибки семантического анализа
data SemanticError
  = UndefinedVariable Identifier SourcePos
    -- ^ Неопределенная переменная
  | UndefinedFunction Identifier SourcePos
    -- ^ Неопределенная функция
  | RedefinedVariable Identifier SourcePos SourcePos
    -- ^ Переопределение переменной (новая позиция, старая позиция)
  | RedefinedFunction Identifier SourcePos SourcePos
    -- ^ Переопределение функции
  | TypeMismatch Text Text SourcePos
    -- ^ Несоответствие типов (ожидался, получен, позиция)
  | InvalidOperation Text SourcePos
    -- ^ Некорректная операция
  | InvalidCast Text Text SourcePos
    -- ^ Некорректное приведение типов
  | InvalidArrayAccess Text SourcePos
    -- ^ Некорректный доступ к массиву
  | InvalidFunctionCall Identifier Int Int SourcePos
    -- ^ Некорректный вызов функции (имя, ожидается параметров, получено, позиция)
  | ReturnTypeMismatch Text Text SourcePos
    -- ^ Несоответствие типа возвращаемого значения
  | UnreachableCode SourcePos
    -- ^ Недостижимый код
  | MissingReturn Identifier SourcePos
    -- ^ Отсутствующий return в функции
  deriving (Eq, Show)

-- | Ошибки генерации кода
data CodeGenError
  = UnsupportedFeature Text SourcePos
    -- ^ Неподдерживаемая возможность
  | ResourceExhausted Text SourcePos
    -- ^ Исчерпание ресурсов (память, регистры)
  | InvalidTargetAddress Text SourcePos
    -- ^ Некорректный адрес назначения
  | OptimizationFailed Text SourcePos
    -- ^ Ошибка оптимизации
  deriving (Eq, Show)

-- | Уровень серьезности ошибки
data ErrorSeverity
  = Fatal    -- ^ Фатальная ошибка, компиляция невозможна
  | Error    -- ^ Ошибка, компиляция прервана
  | Warning  -- ^ Предупреждение, компиляция продолжается
  | Info     -- ^ Информационное сообщение
  deriving (Eq, Ord, Show, Enum, Bounded)

-- ============================================================================
-- ДИАГНОСТИЧЕСКИЕ СООБЩЕНИЯ
-- ============================================================================

-- | Уровень диагностического сообщения
data DiagnosticLevel
  = ErrorLevel
  | WarningLevel  
  | InfoLevel
  deriving (Eq, Ord, Show, Enum, Bounded)

-- | Диагностическое сообщение
data Diagnostic = Diagnostic
  { diagLevel :: !DiagnosticLevel
  , diagMessage :: !Text
  , diagPosition :: !(Maybe SourcePos)
  , diagCode :: !(Maybe Text)  -- ^ Код ошибки для документации
  , diagHints :: ![Text]       -- ^ Подсказки для исправления
  } deriving (Eq, Show)

-- | Создает диагностическое сообщение
mkDiagnostic :: DiagnosticLevel -> Text -> Maybe SourcePos -> Diagnostic
mkDiagnostic level msg pos = Diagnostic
  { diagLevel = level
  , diagMessage = msg
  , diagPosition = pos
  , diagCode = Nothing
  , diagHints = []
  }

-- | Создает ошибку
mkError :: Text -> Maybe SourcePos -> Diagnostic
mkError = mkDiagnostic ErrorLevel

-- | Создает предупреждение
mkWarning :: Text -> Maybe SourcePos -> Diagnostic
mkWarning = mkDiagnostic WarningLevel

-- | Создает информационное сообщение
mkInfo :: Text -> Maybe SourcePos -> Diagnostic
mkInfo = mkDiagnostic InfoLevel

-- ============================================================================
-- МОНАДЫ ДЛЯ ОБРАБОТКИ ОШИБОК
-- ============================================================================

-- | Монада компилятора с обработкой ошибок и сбором диагностики
type CompilerM = ExceptT CompilerError (WriterT [Diagnostic] IO)

-- | Запускает вычисление в монаде компилятора
runCompilerM :: CompilerM a -> IO (Either CompilerError a, [Diagnostic])
runCompilerM = runWriterT . runExceptT

-- | Выбрасывает ошибку компилятора
throwCompilerError :: CompilerError -> CompilerM a
throwCompilerError = throwError

-- | Добавляет диагностическое сообщение
addDiagnostic :: Diagnostic -> CompilerM ()
addDiagnostic diag = tell [diag]

-- | Добавляет предупреждение
addWarning :: Text -> Maybe SourcePos -> CompilerM ()
addWarning msg pos = addDiagnostic (mkWarning msg pos)

-- | Добавляет информационное сообщение
addInfo :: Text -> Maybe SourcePos -> CompilerM ()
addInfo msg pos = addDiagnostic (mkInfo msg pos)

-- | Собирает все диагностические сообщения из вычисления
collectDiagnostics :: CompilerM a -> CompilerM (a, [Diagnostic])
collectDiagnostics action = do
  (result, diags) <- liftIO $ runCompilerM action
  case result of
    Left err -> throwError err
    Right val -> return (val, diags)
  where
    liftIO = Control.Monad.IO.Class.liftIO

-- ============================================================================
-- УТИЛИТЫ ДЛЯ ФОРМАТИРОВАНИЯ
-- ============================================================================

-- | Красивый вывод ошибки
prettyError :: CompilerError -> Text
prettyError = \case
  LexError lexErr -> "Лексическая ошибка: " <> prettyLexerError lexErr
  ParseError parseErr -> "Синтаксическая ошибка: " <> prettyParserError parseErr
  SemanticError semErr -> "Семантическая ошибка: " <> prettySemanticError semErr
  CodeGenError codeErr -> "Ошибка генерации кода: " <> prettyCodeGenError codeErr
  IOError msg pos -> "Ошибка ввода-вывода: " <> msg <> " в " <> prettySourcePos pos
  InternalError msg pos -> "Внутренняя ошибка: " <> msg <> " в " <> prettySourcePos pos

-- | Красивый вывод ошибки лексера
prettyLexerError :: LexerError -> Text
prettyLexerError = \case
  UnexpectedCharacter ch pos ->
    "Неожиданный символ '" <> T.singleton ch <> "' в " <> prettySourcePos pos
  UnterminatedString pos ->
    "Незакрытая строка в " <> prettySourcePos pos
  UnterminatedComment pos ->
    "Незакрытый комментарий в " <> prettySourcePos pos
  InvalidNumber num pos ->
    "Некорректное число '" <> num <> "' в " <> prettySourcePos pos
  InvalidCharLiteral lit pos ->
    "Некорректный символьный литерал '" <> lit <> "' в " <> prettySourcePos pos
  InvalidEscapeSequence esc pos ->
    "Некорректная escape-последовательность '" <> esc <> "' в " <> prettySourcePos pos

-- | Красивый вывод ошибки парсера
prettyParserError :: ParserError -> Text
prettyParserError = \case
  UnexpectedToken expected got pos ->
    "Ожидался '" <> expected <> "', получен '" <> got <> "' в " <> prettySourcePos pos
  UnexpectedEndOfFile pos ->
    "Неожиданный конец файла в " <> prettySourcePos pos
  MissingToken token pos ->
    "Отсутствует '" <> token <> "' в " <> prettySourcePos pos
  InvalidSyntax desc pos ->
    "Некорректный синтаксис: " <> desc <> " в " <> prettySourcePos pos
  AmbiguousExpression desc pos ->
    "Неоднозначное выражение: " <> desc <> " в " <> prettySourcePos pos

-- | Красивый вывод семантической ошибки
prettySemanticError :: SemanticError -> Text
prettySemanticError = \case
  UndefinedVariable ident pos ->
    "Неопределенная переменная '" <> identText ident <> "' в " <> prettySourcePos pos
  UndefinedFunction ident pos ->
    "Неопределенная функция '" <> identText ident <> "' в " <> prettySourcePos pos
  RedefinedVariable ident newPos oldPos ->
    "Переопределение переменной '" <> identText ident <> "' в " <> prettySourcePos newPos <>
    " (первое определение в " <> prettySourcePos oldPos <> ")"
  RedefinedFunction ident newPos oldPos ->
    "Переопределение функции '" <> identText ident <> "' в " <> prettySourcePos newPos <>
    " (первое определение в " <> prettySourcePos oldPos <> ")"
  TypeMismatch expected got pos ->
    "Несоответствие типов: ожидался '" <> expected <> "', получен '" <> got <> "' в " <> prettySourcePos pos
  InvalidOperation op pos ->
    "Некорректная операция: " <> op <> " в " <> prettySourcePos pos
  InvalidCast from to pos ->
    "Некорректное приведение типов от '" <> from <> "' к '" <> to <> "' в " <> prettySourcePos pos
  InvalidArrayAccess desc pos ->
    "Некорректный доступ к массиву: " <> desc <> " в " <> prettySourcePos pos
  InvalidFunctionCall ident expected got pos ->
    "Некорректный вызов функции '" <> identText ident <> "': ожидается " <> 
    T.pack (show expected) <> " параметров, получено " <> T.pack (show got) <> " в " <> prettySourcePos pos
  ReturnTypeMismatch expected got pos ->
    "Несоответствие типа возвращаемого значения: ожидался '" <> expected <> "', получен '" <> got <> "' в " <> prettySourcePos pos
  UnreachableCode pos ->
    "Недостижимый код в " <> prettySourcePos pos
  MissingReturn ident pos ->
    "Отсутствует return в функции '" <> identText ident <> "' в " <> prettySourcePos pos
  where
    identText (HCL.Types.Identifier txt) = txt

-- | Красивый вывод ошибки генерации кода
prettyCodeGenError :: CodeGenError -> Text
prettyCodeGenError = \case
  UnsupportedFeature feature pos ->
    "Неподдерживаемая возможность: " <> feature <> " в " <> prettySourcePos pos
  ResourceExhausted resource pos ->
    "Исчерпание ресурсов: " <> resource <> " в " <> prettySourcePos pos
  InvalidTargetAddress addr pos ->
    "Некорректный адрес назначения: " <> addr <> " в " <> prettySourcePos pos
  OptimizationFailed desc pos ->
    "Ошибка оптимизации: " <> desc <> " в " <> prettySourcePos pos

-- | Красивый вывод позиции в исходном коде
prettySourcePos :: SourcePos -> Text
prettySourcePos pos = 
  HCL.Types.sourceName pos <> ":" <> 
  T.pack (show (HCL.Types.sourceLine pos)) <> ":" <> 
  T.pack (show (HCL.Types.sourceColumn pos))

-- | Красивый вывод диагностического сообщения
prettyDiagnostic :: Diagnostic -> Text
prettyDiagnostic Diagnostic{..} =
  levelPrefix <> diagMessage <> positionSuffix <> hintsSuffix
  where
    levelPrefix = case diagLevel of
      ErrorLevel -> "Ошибка: "
      WarningLevel -> "Предупреждение: "
      InfoLevel -> "Информация: "
    
    positionSuffix = case diagPosition of
      Nothing -> ""
      Just pos -> " в " <> prettySourcePos pos
    
    hintsSuffix = if null diagHints
      then ""
      else "\n  Подсказки:\n" <> T.unlines (map ("    " <>) diagHints)

-- | Форматирует ошибку с контекстом исходного кода
formatErrorWithContext :: CompilerError -> Text -> Text
formatErrorWithContext err sourceCode = 
  prettyError err <> "\n\n" <> "Контекст:\n" <> sourceCode 