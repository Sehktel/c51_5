-- | Тесты для модуля лексера HCL
module HCL.LexerSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T

import HCL.Lexer
import HCL.Types (Identifier(..), Literal(..), LiteralType(..), SourcePos(..))

spec :: Spec
spec = describe "HCL.Lexer" $ do
  
  describe "Базовая токенизация" $ do
    it "токенизирует простые идентификаторы" $ do
      let input = "main variable_name _private"
      let result = tokenize input
      lrSuccess result `shouldBe` True
      length (lrTokens result) `shouldBe` 4  -- 3 токена + EOF
      
      let tokens = lrTokens result
      tokenType (tokens !! 0) `shouldSatisfy` \case
        TIdentifier (Identifier "main") -> True
        _ -> False
      tokenType (tokens !! 1) `shouldSatisfy` \case
        TIdentifier (Identifier "variable_name") -> True
        _ -> False
      tokenType (tokens !! 2) `shouldSatisfy` \case
        TIdentifier (Identifier "_private") -> True
        _ -> False
    
    it "токенизирует ключевые слова" $ do
      let result = tokenize "int void char"
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 3
      map tokenType tokens `shouldBe` [TKeyword KInt, TKeyword KVoid, TKeyword KChar]
    
    it "токенизирует числовые литералы" $ do
      let result = tokenize "42 0x1A 077"
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 3
      all isLiteral tokens `shouldBe` True
      where
        isLiteral Token{tokenType = TLiteral _} = True
        isLiteral _ = False
    
    it "токенизирует строковые литералы" $ do
      let result = tokenize "\"hello world\""
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 1
      case tokenType (head tokens) of
        TLiteral (StringLit str) -> str `shouldBe` "hello world"
        _ -> expectationFailure "Expected string literal"
    
    it "токенизирует символьные литералы" $ do
      let result = tokenize "'a' '\\n'"
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 2
      all isCharLiteral tokens `shouldBe` True
      where
        isCharLiteral Token{tokenType = TLiteral (CharLit _)} = True
        isCharLiteral _ = False
  
  describe "Операторы" $ do
    it "токенизирует арифметические операторы" $ do
      let result = tokenize "+ - * / %"
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 5
      map tokenType tokens `shouldBe` 
        [TOperator OpPlus, TOperator OpMinus, TOperator OpMul, 
         TOperator OpDiv, TOperator OpMod]
    
    it "токенизирует операторы сравнения" $ do
      let result = tokenize "== != < <= > >="
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 6
      map tokenType tokens `shouldBe`
        [TOperator OpEq, TOperator OpNe, TOperator OpLt,
         TOperator OpLe, TOperator OpGt, TOperator OpGe]
    
    it "токенизирует составные операторы присваивания" $ do
      let result = tokenize "+= -= *= /= %= &= |= ^="
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 8
      map tokenType tokens `shouldBe`
        [TOperator OpPlusAssign, TOperator OpMinusAssign, TOperator OpMulAssign,
         TOperator OpDivAssign, TOperator OpModAssign, TOperator OpAndAssign,
         TOperator OpOrAssign, TOperator OpXorAssign]
    
    it "токенизирует инкремент и декремент" $ do
      let result = tokenize "++ --"
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 2
      map tokenType tokens `shouldBe` [TOperator OpIncrement, TOperator OpDecrement]
  
  describe "Разделители" $ do
    it "токенизирует скобки и разделители" $ do
      let result = tokenize "( ) { } [ ] ; ,"
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 8
      map tokenType tokens `shouldBe`
        [TDelimiter DelimLParen, TDelimiter DelimRParen,
         TDelimiter DelimLBrace, TDelimiter DelimRBrace,
         TDelimiter DelimLBracket, TDelimiter DelimRBracket,
         TDelimiter DelimSemicolon, TDelimiter DelimComma]
  
  describe "Расширения для 8051" $ do
    it "токенизирует ключевые слова 8051" $ do
      let result = tokenize "sbit sfr interrupt using"
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 4
      map tokenType tokens `shouldBe`
        [TKeyword KSbit, TKeyword KSfr, TKeyword KInterrupt, TKeyword KUsing]
    
    it "токенизирует типы памяти 8051" $ do
      let result = tokenize "data idata xdata code bit"
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 5
      map tokenType tokens `shouldBe`
        [TKeyword KData, TKeyword KIdata, TKeyword KXdata, 
         TKeyword KCode, TKeyword KBit]
  
  describe "Комментарии" $ do
    it "пропускает однострочные комментарии" $ do
      let result = tokenize "int x; // это комментарий\nint y;"
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 6  -- int x ; int y ;
      map tokenType tokens `shouldBe`
        [TKeyword KInt, TIdentifier (Identifier "x"), TDelimiter DelimSemicolon,
         TKeyword KInt, TIdentifier (Identifier "y"), TDelimiter DelimSemicolon]
    
    it "пропускает многострочные комментарии" $ do
      let result = tokenize "int x; /* многострочный\nкомментарий */ int y;"
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 6  -- int x ; int y ;
  
  describe "Сложные примеры" $ do
    it "токенизирует простую функцию" $ do
      let code = "int main() { return 0; }"
      let result = tokenize code
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 8
      map tokenType tokens `shouldBe`
        [TKeyword KInt, TIdentifier (Identifier "main"),
         TDelimiter DelimLParen, TDelimiter DelimRParen,
         TDelimiter DelimLBrace, TKeyword KReturn,
         TLiteral (IntLiteral 0 NumberLiteral), TDelimiter DelimSemicolon,
         TDelimiter DelimRBrace]
    
    it "токенизирует объявление переменной с инициализацией" $ do
      let code = "int x = 42;"
      let result = tokenize code
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 5
      map tokenType tokens `shouldBe`
        [TKeyword KInt, TIdentifier (Identifier "x"),
         TOperator OpAssign, TLiteral (IntLiteral 42 NumberLiteral),
         TDelimiter DelimSemicolon]
    
    it "токенизирует условную конструкцию" $ do
      let code = "if (x > 0) { x++; }"
      let result = tokenize code
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 10
    
    it "токенизирует объявление sbit для 8051" $ do
      let code = "sbit LED = P1^0;"
      let result = tokenize code
      lrSuccess result `shouldBe` True
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      length tokens `shouldBe` 6
      head (map tokenType tokens) `shouldBe` TKeyword KSbit
  
  describe "Обработка ошибок" $ do
    it "обрабатывает некорректные символы" $ do
      let result = tokenize "int x @ y;"  -- @ не является валидным символом
      lrSuccess result `shouldBe` False
      length (lrErrors result) `shouldBe` 1
    
    it "обрабатывает незакрытые строки" $ do
      let result = tokenize "\"незакрытая строка"
      lrSuccess result `shouldBe` False
      length (lrErrors result) `shouldBe` 1
  
  describe "Утилиты" $ do
    it "правильно определяет ключевые слова" $ do
      let result = tokenize "int main"
      let tokens = lrTokens result
      isKeyword (head tokens) `shouldBe` True
      isKeyword (tokens !! 1) `shouldBe` False
    
    it "правильно определяет операторы" $ do
      let result = tokenize "x + y"
      let tokens = filter (\t -> tokenType t /= TEOF) (lrTokens result)
      isOperator (tokens !! 1) `shouldBe` True
      isOperator (head tokens) `shouldBe` False
    
    it "возвращает позиции токенов" $ do
      let input = "int x"
      let result = tokenizeWithPos "test.c" input
      let tokens = lrTokens result
      
      let pos1 = tokenPosition (tokens !! 0)
      sourceLine pos1 `shouldBe` 1
      sourceColumn pos1 `shouldBe` 1
      
      let pos2 = tokenPosition (tokens !! 1)
      sourceLine pos2 `shouldBe` 1
      sourceColumn pos2 `shouldBe` 5  -- После "int " 