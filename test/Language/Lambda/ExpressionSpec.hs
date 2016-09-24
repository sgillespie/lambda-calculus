module Language.Lambda.ExpressionSpec where

import Test.Hspec

import Language.Lambda.Expression

spec :: Spec
spec = do
  describe "ppr" $ do
    it "prints simple variables" $ do
      ppr (Var "x") `shouldBe` "x"

    it "prints simple abstractions" $ do
      ppr (Abs "x" (Var "x")) `shouldBe` "\\x. x"

    it "prints simple applications" $ do
      ppr (App (Var "f") (Var "x")) `shouldBe` "f x"

    it "prints nested abstractions" $ do
      ppr (Abs "f" (Abs "x" (Var "x")))
        `shouldBe` "\\f x. x"

    it "prints nested applications" $ do
      ppr (App (App (Var "f") (Var "x")) (Var "y"))
        `shouldBe` "f x y"

    it "prints parenthesized applications" $ do
      ppr (App (Var "f") (App (Var "x") (Var "y")))
        `shouldBe` "f (x y)"

      ppr (App (Abs "x" (Var "x")) (Var "y"))
        `shouldBe` "(\\x. x) y"

      ppr (App (Var "x") (Abs "f" (Var "f")))
        `shouldBe` "x (\\f. f)"
