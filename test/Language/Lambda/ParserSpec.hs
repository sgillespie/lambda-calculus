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

    it "parses simple abstractions" $ do
      parseExpr "\\x. x" `shouldBe` Right (Abs "x" (Var "x"))

    it "parses nested abstractions" $ do
      parseExpr "\\f a. a" `shouldBe` Right (Abs "f" (Abs "a" (Var "a")))

    it "parses simple applications" $ do
      parseExpr "f x" `shouldBe` Right (App (Var "f") (Var "x"))

    it "parses chained applications" $ do
      parseExpr "f x y" `shouldBe` Right (App (App (Var "f") (Var "x")) (Var "y"))

    it "parses complex expressions" $ do
      let exprs = [
            "\\f x. f x",
            "(\\p x y. y) (\\p x y. x)",
            "f (\\x. x)",
            "(\\x . f x) g y",
            "(\\f . (\\ x y. f x y) f x y) w x y"
            ]
      
      mapM_ ((flip shouldSatisfy) isRight . parseExpr) exprs

    it "does not parse trailing errors" $ do
      parseExpr "x +" `shouldSatisfy` isLeft
  
    it "ignores whitespace" $ do
      let exprs = [
            " x ",
            " \\ x . x ",
            " ( x ) "
            ]
      
      mapM_ ((flip shouldSatisfy) isRight . parseExpr) exprs
            

