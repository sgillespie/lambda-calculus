module Language.Lambda.ParserSpec (spec) where

import Data.Either

import Test.Hspec

import Language.Lambda.Expression
import Language.Lambda.Parser

spec :: Spec
spec = do
  describe "parseExpr" $ do
    it "parses simple variables" $ do
      parseExpr "x" `shouldBe` Right (Var "x")

    it "parses parenthesized variables" $ do
      parseExpr "(x)" `shouldBe` Right (Var "x")

    it "parses parenthesized variables with whitespace" $ do
      parseExpr "( x )" `shouldBe` Right (Var "x")

    it "does not parse trailing errors" $ do
      parseExpr "x +" `shouldSatisfy` isLeft

    it "parses simple abstractions" $ do
      parseExpr "\\x. x" `shouldBe` Right (Abs "x" (Var "x"))

    it "parses nested abstractions" $ do
      parseExpr "\\f a. a" `shouldBe` Right (Abs "f" (Abs "a" (Var "a")))
  
    it "ignores whitespace" $ do
      let exprs = [
            " x ",
            " \\ x . x ",
            " ( x ) "
            ]
      
      mapM_ ((flip shouldSatisfy) isRight . parseExpr) exprs
            

