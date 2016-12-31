module Language.SystemF.ParserSpec (spec) where

import Data.Either

import Test.Hspec

import Language.SystemF.Expression
import Language.SystemF.Parser

spec :: Spec
spec = do
  describe "parseExpr" $ do
    it "parses simple variables" $
      parseExpr "x" `shouldBe` Right (Var "x")

    it "parses parenthesized variables" $
      parseExpr "(x)" `shouldBe` Right (Var "x")

    it "parses simple abstractions" $
      parseExpr "\\x:T. x" `shouldBe` Right (Abs "x" (TyVar "T") (Var "x"))

    it "parses simple type abstractions" $
      parseExpr "\\X. x" `shouldBe` Right (TyAbs "X" (Var "x"))

    it "parses simple type applications" $ 
      parseExpr "x [T]" `shouldBe` Right (TyApp (Var "x") (TyVar "T"))

    it "parses nested abstractions" $
      parseExpr "\\a:A b:B. b" 
        `shouldBe` Right (Abs "a" (TyVar "A") (Abs "b" (TyVar "B") (Var "b")))

    it "parses abstractions with arrow types" $
      parseExpr "\\f:(T->U). f"
        `shouldBe` Right (Abs "f" (TyArrow (TyVar "T") (TyVar "U")) (Var "f"))

    it "parses simple applications" $
      parseExpr "f x" `shouldBe` Right (App (Var "f") (Var "x"))

    it "parses chained applications" $
      parseExpr "a b c" `shouldBe` Right (App (App (Var "a") (Var "b")) (Var "c"))

    it "parses complex expressions"  $ do
      let exprs = [
            "\\f:(A->B) x:B. f x",
            "(\\p:(X->Y->Z) x:X y:Y. y) (\\p:(A->B->C) x:B y:C. x)",
            "f (\\x:T. x)",
            "(\\ x:X . f x) g y",
            "(\\f:(X->Y) . (\\ x:X y:Y. f x y) f x y) w x y",
            "(\\x:T. x) [U]"
            ]

      mapM_ (flip shouldSatisfy isRight . parseExpr) exprs

    it "does not parse trailing errors" $
      parseExpr "x +" `shouldSatisfy` isLeft

    it "ignores whitespace" $ do
      let exprs = [
            " x ",
            " \\ x : X. x ",
            " ( x ) "
            ]

      mapM_ (flip shouldSatisfy isRight . parseExpr) exprs
  
  describe "parseType" $ do
    it "parses simple variables" $
      parseType "X" `shouldBe` Right (TyVar "X")

    it "parses parenthesized variables" $
      parseType "(T)" `shouldBe` Right (TyVar "T")

    it "parses simple arrow types" $
      parseType "A -> B" `shouldBe` Right (TyArrow (TyVar "A") (TyVar "B")) 

    it "parses parenthesized arrow types" $
      parseType "((X)->(Y))" `shouldBe` Right (TyArrow (TyVar "X") (TyVar "Y"))

    it "parses nested arrow types" $ do
      parseType "T -> U -> V" 
        `shouldBe` Right (TyArrow (TyVar "T") (TyArrow (TyVar "U") (TyVar "V")))

      parseType "(W -> V) -> U"
        `shouldBe` Right (TyArrow (TyArrow (TyVar "W") (TyVar "V")) (TyVar "U"))
