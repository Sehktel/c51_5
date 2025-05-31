-- | Тесты для модуля синтаксического анализатора HCL
module HCL.ParserSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import HCL.Parser
import HCL.Lexer (tokenize, lrTokens, lrSuccess)
import HCL.AST
import HCL.Types (Identifier(..), Literal(..), LiteralType(..), SourcePos(..), 
                  BinaryOp(..), UnaryOp(..), AssignOp(..))

-- Вспомогательная функция для парсинга из текста
parseFromText :: Parser a -> Text -> Either String a
parseFromText parser input = do
  let lexResult = tokenize input
  if not (lrSuccess lexResult)
    then Left "Lexer error"
    else case runParser parser (lrTokens lexResult) of
      ParseResult (Left err) _ _ -> Left (T.unpack $ prettyParseError err)
      ParseResult (Right result) _ _ -> Right result

spec :: Spec
spec = describe "HCL.Parser" $ do
  
  describe "Парсинг выражений" $ do
    it "парсит простые литералы" $ do
      let result = parseFromText parseExpression "42"
      case result of
        Right (Expr (ELiteral (IntLiteral 42 _)) _) -> return ()
        _ -> expectationFailure $ "Expected integer literal, got: " ++ show result
    
    it "парсит переменные" $ do
      let result = parseFromText parseExpression "variable"
      case result of
        Right (Expr (EVariable (Identifier "variable")) _) -> return ()
        _ -> expectationFailure $ "Expected variable, got: " ++ show result
    
    it "парсит бинарные операции" $ do
      let result = parseFromText parseExpression "a + b"
      case result of
        Right (Expr (EBinary Add 
          (Expr (EVariable (Identifier "a")) _)
          (Expr (EVariable (Identifier "b")) _)) _) -> return ()
        _ -> expectationFailure $ "Expected binary operation, got: " ++ show result
    
    it "парсит сложные выражения с приоритетами" $ do
      let result = parseFromText parseExpression "a + b * c"
      case result of
        Right (Expr (EBinary Add 
          (Expr (EVariable (Identifier "a")) _)
          (Expr (EBinary Mul 
            (Expr (EVariable (Identifier "b")) _)
            (Expr (EVariable (Identifier "c")) _)) _)) _) -> return ()
        _ -> expectationFailure $ "Expected correct precedence, got: " ++ show result
    
    it "парсит выражения в скобках" $ do
      let result = parseFromText parseExpression "(a + b) * c"
      case result of
        Right (Expr (EBinary Mul 
          (Expr (EBinary Add _ _) _)
          (Expr (EVariable (Identifier "c")) _)) _) -> return ()
        _ -> expectationFailure $ "Expected parenthesized expression, got: " ++ show result
    
    it "парсит вызовы функций" $ do
      let result = parseFromText parseExpression "func(a, b)"
      case result of
        Right (Expr (ECall (Identifier "func") [_, _]) _) -> return ()
        _ -> expectationFailure $ "Expected function call, got: " ++ show result
    
    it "парсит доступ к массиву" $ do
      let result = parseFromText parseExpression "arr[index]"
      case result of
        Right (Expr (EArrayAccess 
          (Expr (EVariable (Identifier "arr")) _)
          (Expr (EVariable (Identifier "index")) _)) _) -> return ()
        _ -> expectationFailure $ "Expected array access, got: " ++ show result
    
    it "парсит присваивание" $ do
      let result = parseFromText parseExpression "x = 42"
      case result of
        Right (Expr (EAssign SimpleAssign 
          (Expr (EVariable (Identifier "x")) _)
          (Expr (ELiteral (IntLiteral 42 _)) _)) _) -> return ()
        _ -> expectationFailure $ "Expected assignment, got: " ++ show result
    
    it "парсит унарные операторы" $ do
      let result = parseFromText parseExpression "-x"
      case result of
        Right (Expr (EUnary UnaryMinus 
          (Expr (EVariable (Identifier "x")) _)) _) -> return ()
        _ -> expectationFailure $ "Expected unary operation, got: " ++ show result
    
    it "парсит тернарный оператор" $ do
      let result = parseFromText parseExpression "a ? b : c"
      case result of
        Right (Expr (ETernary 
          (Expr (EVariable (Identifier "a")) _)
          (Expr (EVariable (Identifier "b")) _)
          (Expr (EVariable (Identifier "c")) _)) _) -> return ()
        _ -> expectationFailure $ "Expected ternary operator, got: " ++ show result
  
  describe "Парсинг операторов" $ do
    it "парсит оператор-выражение" $ do
      let result = parseFromText parseStatement "x = 42;"
      case result of
        Right (Stmt (SExpr _) _) -> return ()
        _ -> expectationFailure $ "Expected expression statement, got: " ++ show result
    
    it "парсит if-оператор" $ do
      let result = parseFromText parseStatement "if (x > 0) return 1;"
      case result of
        Right (Stmt (SIf _ _ Nothing) _) -> return ()
        _ -> expectationFailure $ "Expected if statement, got: " ++ show result
    
    it "парсит if-else оператор" $ do
      let result = parseFromText parseStatement "if (x > 0) return 1; else return 0;"
      case result of
        Right (Stmt (SIf _ _ (Just _)) _) -> return ()
        _ -> expectationFailure $ "Expected if-else statement, got: " ++ show result
    
    it "парсит while-оператор" $ do
      let result = parseFromText parseStatement "while (x > 0) x--;"
      case result of
        Right (Stmt (SWhile _ _) _) -> return ()
        _ -> expectationFailure $ "Expected while statement, got: " ++ show result
    
    it "парсит for-оператор" $ do
      let result = parseFromText parseStatement "for (i = 0; i < 10; i++) sum += i;"
      case result of
        Right (Stmt (SFor (Just _) (Just _) (Just _) _) _) -> return ()
        _ -> expectationFailure $ "Expected for statement, got: " ++ show result
    
    it "парсит составной оператор" $ do
      let result = parseFromText parseStatement "{ x = 1; y = 2; }"
      case result of
        Right (Stmt (SCompound (CompoundStmt [_, _] _)) _) -> return ()
        _ -> expectationFailure $ "Expected compound statement, got: " ++ show result
    
    it "парсит return-оператор" $ do
      let result = parseFromText parseStatement "return 42;"
      case result of
        Right (Stmt (SReturn (Just _)) _) -> return ()
        _ -> expectationFailure $ "Expected return statement, got: " ++ show result
    
    it "парсит break и continue" $ do
      let breakResult = parseFromText parseStatement "break;"
      let continueResult = parseFromText parseStatement "continue;"
      
      case (breakResult, continueResult) of
        (Right (Stmt SBreak _), Right (Stmt SContinue _)) -> return ()
        _ -> expectationFailure "Expected break and continue statements"
  
  describe "Парсинг типов" $ do
    it "парсит базовые типы" $ do
      let intResult = parseFromText parseType "int"
      let charResult = parseFromText parseType "char"
      let voidResult = parseFromText parseType "void"
      
      case (intResult, charResult, voidResult) of
        (Right TSInt, Right TSChar, Right TSVoid) -> return ()
        _ -> expectationFailure "Expected basic types"
    
    it "парсит указатели" $ do
      let result = parseFromText parseType "int*"
      case result of
        Right (TSPointer TSInt) -> return ()
        _ -> expectationFailure $ "Expected pointer type, got: " ++ show result
    
    it "парсит массивы" $ do
      let result = parseFromText parseType "int[10]"
      case result of
        Right (TSArray TSInt (Just _)) -> return ()
        _ -> expectationFailure $ "Expected array type, got: " ++ show result
    
    it "парсит расширения для 8051" $ do
      let bitResult = parseFromText parseType "bit"
      case bitResult of
        Right TSBit -> return ()
        _ -> expectationFailure $ "Expected bit type, got: " ++ show bitResult
  
  describe "Парсинг деклараций" $ do
    it "парсит простую декларацию переменной" $ do
      let result = parseFromText parseDeclaration "int x;"
      case result of
        Right (Declaration TSInt (Identifier "x") Nothing _ _ _ _) -> return ()
        _ -> expectationFailure $ "Expected variable declaration, got: " ++ show result
    
    it "парсит декларацию с инициализацией" $ do
      let result = parseFromText parseDeclaration "int x = 42;"
      case result of
        Right (Declaration TSInt (Identifier "x") (Just _) _ _ _ _) -> return ()
        _ -> expectationFailure $ "Expected initialized declaration, got: " ++ show result
    
    it "парсит декларацию массива" $ do
      let result = parseFromText parseDeclaration "int arr[10];"
      case result of
        Right (Declaration (TSArray TSInt _) (Identifier "arr") Nothing _ _ _ _) -> return ()
        _ -> expectationFailure $ "Expected array declaration, got: " ++ show result
  
  describe "Парсинг функций" $ do
    it "парсит простую функцию" $ do
      let result = parseFromText parseFunctionDef "int main() { return 0; }"
      case result of
        Right (FunctionDef (Identifier "main") TSInt [] _ _ _ _ _) -> return ()
        _ -> expectationFailure $ "Expected function definition, got: " ++ show result
    
    it "парсит функцию с параметрами" $ do
      let result = parseFromText parseFunctionDef "int add(int a, int b) { return a + b; }"
      case result of
        Right (FunctionDef (Identifier "add") TSInt [_, _] _ _ _ _ _) -> return ()
        _ -> expectationFailure $ "Expected function with parameters, got: " ++ show result
    
    it "парсит функцию прерывания (8051)" $ do
      let result = parseFromText parseFunctionDef "void timer_isr() interrupt 1 { /* код */ }"
      case result of
        Right (FunctionDef (Identifier "timer_isr") TSVoid [] _ _ (Just _) _ _) -> return ()
        _ -> expectationFailure $ "Expected interrupt function, got: " ++ show result
  
  describe "Расширения для 8051" $ do
    it "парсит sbit декларацию" $ do
      let result = parseFromText parseBitDeclaration "sbit LED = P1^0;"
      case result of
        Right (BitDeclaration (Identifier "LED") _ _) -> return ()
        _ -> expectationFailure $ "Expected sbit declaration, got: " ++ show result
    
    it "парсит sfr декларацию" $ do
      let result = parseFromText parseSfrDeclaration "sfr P1 = 0x90;"
      case result of
        Right (SfrDeclaration (Identifier "P1") _ _ _) -> return ()
        _ -> expectationFailure $ "Expected sfr declaration, got: " ++ show result
  
  describe "Парсинг программы" $ do
    it "парсит простую программу" $ do
      let input = T.unlines
            [ "int global_var;"
            , "int main() {"
            , "    return 0;"
            , "}"
            ]
      let result = parseFromText parseProgram input
      case result of
        Right (Program [_, _] _) -> return ()
        _ -> expectationFailure $ "Expected program with declarations, got: " ++ show result
    
    it "парсит программу с расширениями 8051" $ do
      let input = T.unlines
            [ "sbit LED = P1^0;"
            , "sfr P1 = 0x90;"
            , "void timer_isr() interrupt 1 {"
            , "    LED = !LED;"
            , "}"
            , "int main() {"
            , "    while(1) {"
            , "        /* основной цикл */"
            , "    }"
            , "    return 0;"
            , "}"
            ]
      let result = parseFromText parseProgram input
      case result of
        Right (Program [_, _, _, _] _) -> return ()
        _ -> expectationFailure $ "Expected 8051 program, got: " ++ show result
  
  describe "Обработка ошибок" $ do
    it "обрабатывает синтаксические ошибки" $ do
      let result = parseFromText parseExpression "a +"  -- Неполное выражение
      case result of
        Left _ -> return ()  -- Ожидаем ошибку
        Right _ -> expectationFailure "Expected parse error"
    
    it "обрабатывает несоответствие скобок" $ do
      let result = parseFromText parseExpression "(a + b"  -- Незакрытая скобка
      case result of
        Left _ -> return ()  -- Ожидаем ошибку
        Right _ -> expectationFailure "Expected parse error"
    
    it "обрабатывает неожиданные токены" $ do
      let result = parseFromText parseStatement "if x > 0 return 1;"  -- Пропущены скобки
      case result of
        Left _ -> return ()  -- Ожидаем ошибку
        Right _ -> expectationFailure "Expected parse error" 