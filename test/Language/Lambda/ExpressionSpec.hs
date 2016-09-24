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
