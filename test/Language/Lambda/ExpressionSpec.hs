module Language.Lambda.ExpressionSpec where

import Test.Hspec

import Language.Lambda.Expression
import Language.Lambda.PrettyPrint

spec :: Spec
spec = do
  describe "prettyPrint" $ do
    it "prints simple variables" $ do
      prettyPrint (Var "x") `shouldBe` "x"

    it "prints simple abstractions" $ do
      prettyPrint (Abs "x" (Var "x")) `shouldBe` "\\x. x"

    it "prints simple applications" $ do
      prettyPrint (App (Var "a") (Var "b"))
        `shouldBe` "a b"

    it "prints nested applications" $ do
      prettyPrint (Abs "f" (Abs "x" (Var"x")))
        `shouldBe` "\\f x. x"

    it "prints nested applications" $ do
      prettyPrint (App (App (Var "f") (Var "x")) (Var "y"))
        `shouldBe` "f x y"

    it "prints parenthesized applications" $ do
      prettyPrint (App (Var "f") (App (Var "x") (Var "y")))
        `shouldBe` "f (x y)"

      prettyPrint (App (Abs "x" (Var "x")) (Var "y"))
        `shouldBe` "(\\x. x) y"

      prettyPrint (App (Var "x") (Abs "f" (Var "f")))
        `shouldBe` "x (\\f. f)"
      
