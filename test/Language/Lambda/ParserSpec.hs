module Language.Lambda.ParserSpec (spec) where

import Data.Either

import Test.Hspec

import Language.Lambda.Expression
import Language.Lambda.Parser

spec :: Spec
spec = describe "parseExpr" $ do
  it "parses simple variables" $
    parseExpr "x" `shouldBe` Right (Var "x")

  it "parses parenthesized variables" $
    parseExpr "(x)" `shouldBe` Right (Var "x")

  it "parses simple abstractions" $
    parseExpr "\\x. x" `shouldBe` Right (Abs "x" (Var "x"))

  it "parses nested abstractions" $
    parseExpr "\\f a. a" `shouldBe` Right (Abs "f" (Abs "a" (Var "a")))

  it "parses simple applications" $
    parseExpr "f x" `shouldBe` Right (App (Var "f") (Var "x"))

  it "parses chained applications" $
    parseExpr "f x y" `shouldBe` Right (App (App (Var "f") (Var "x")) (Var "y"))

  it "parses complex expressions" $ do
    let exprs = [
          "\\f x. f x",
          "(\\p x y. y) (\\p x y. x)",
          "f (\\x. x)",
          "(\\x . f x) g y",
          "(\\f . (\\ x y. f x y) f x y) w x y"
          ]
    
    mapM_ (flip shouldSatisfy isRight . parseExpr) exprs

  it "does not parse trailing errors" $
    parseExpr "x +" `shouldSatisfy` isLeft

  it "ignores whitespace" $ do
    let exprs = [
          " x ",
          " \\ x . x ",
          " ( x ) "
          ]
    
    mapM_ (flip shouldSatisfy isRight . parseExpr) exprs
            

