module Language.SystemF.ParserSpec (spec) where

import Data.Either

import Test.Hspec

import Language.SystemF.Expression
import Language.SystemF.Parser

spec :: Spec
spec = describe "parseExpr" $ do
  it "parses simple variables" $
    parseExpr "x" `shouldBe` Right (Var "x")

  it "parses parenthesized variables" $
    parseExpr "(x)" `shouldBe` Right (Var "x")

  it "parses simple abstractions" $
    pendingWith "Not Implemented"

  it "parses simple applications" $
    parseExpr "f x" `shouldBe` Right (App (Var "f") (Var "x"))

  it "parses chained applications" $
    parseExpr "a b c" `shouldBe` Right (App (App (Var "a") (Var "b")) (Var "c"))

  it "parses complex expressions"  $
    pendingWith "Abstraction Not Implemented"

  it "does not parse trailing errors" $
    parseExpr "x +" `shouldSatisfy` isLeft

  it "ignores whitespace" $
    pendingWith "Abstraction Not Implemented"
