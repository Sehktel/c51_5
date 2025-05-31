-- | Тесты для модуля препроцессора HCL
module HCL.PreprocessorSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map

import HCL.Preprocessor
import HCL.Error (DiagnosticLevel(..))

spec :: Spec
spec = describe "HCL.Preprocessor" $ do
  
  describe "Базовая функциональность" $ do
    it "обрабатывает простой текст без директив" $ do
      let input = "int main() {\n    return 0;\n}"
      result <- preprocess defaultOptions "test.c" input
      prSuccess result `shouldBe` True
      T.strip (prResult result) `shouldBe` T.strip input
    
    it "обрабатывает простые макросы #define" $ do
      let input = "#define MAX 100\nint x = MAX;"
      result <- preprocess defaultOptions "test.c" input
      prSuccess result `shouldBe` True
      prResult result `shouldContain` "int x = 100;"
    
    it "обрабатывает условную компиляцию #ifdef" $ do
      let input = "#define DEBUG\n#ifdef DEBUG\nint debug = 1;\n#endif\nint main() { return 0; }"
      result <- preprocess defaultOptions "test.c" input
      prSuccess result `shouldBe` True
      prResult result `shouldContain` "int debug = 1;"
      prResult result `shouldContain` "int main() { return 0; }"
  
  describe "Предопределенные макросы" $ do
    it "содержит макросы компилятора" $ do
      let input = "__C51__"
      result <- preprocess defaultOptions "test.c" input
      prSuccess result `shouldBe` True
      prResult result `shouldContain` "1"
    
    it "содержит макрос целевого микроконтроллера" $ do
      let input = "__AT89S4051__"
      result <- preprocess defaultOptions "test.c" input
      prSuccess result `shouldBe` True
      prResult result `shouldContain` "1"
  
  describe "Обработка ошибок" $ do
    it "генерирует ошибку для некорректной директивы #define" $ do
      let input = "#define\nint x;"
      result <- preprocess defaultOptions "test.c" input
      let errors = filter (\d -> diagLevel d == ErrorLevel) (prDiagnostics result)
      length errors `shouldBe` 1
    
    it "обрабатывает #undef для несуществующего макроса" $ do
      let input = "#undef NONEXISTENT\nint x;"
      result <- preprocess defaultOptions "test.c" input
      prSuccess result `shouldBe` True  -- Это не ошибка
  
  describe "Расширение макросов" $ do
    it "правильно расширяет простые макросы" $ do
      let defines = Map.fromList [("PI", SimpleMacro "3.14"), ("MAX", SimpleMacro "100")]
          input = "float pi = PI; int max = MAX;"
          result = expandMacros defines input
      result `shouldBe` "float pi = 3.14; int max = 100;"
    
    it "не изменяет текст без макросов" $ do
      let defines = Map.fromList [("PI", SimpleMacro "3.14")]
          input = "int x = 42;"
          result = expandMacros defines input
      result `shouldBe` input
  
  describe "Вычисление условий" $ do
    it "правильно определяет существование макроса" $ do
      let defines = Map.fromList [("DEBUG", SimpleMacro "1")]
      evaluateCondition "DEBUG" defines `shouldBe` True
      evaluateCondition "RELEASE" defines `shouldBe` False
    
    it "обрабатывает defined() оператор" $ do
      let defines = Map.fromList [("DEBUG", SimpleMacro "1")]
      evaluateCondition "defined DEBUG" defines `shouldBe` True
      evaluateCondition "defined RELEASE" defines `shouldBe` False 