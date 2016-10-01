module Language.Lambda.EvalSpec where

import Test.Hspec

import Language.Lambda.Eval
import Language.Lambda.Expression

spec :: Spec
spec = do
  describe "evalExpr" $ do
    it "Beta reduces" $ do
      let expr = App (Abs "x" (Var "x")) (Var "z")
      evalExpr expr `shouldBe` Var "z"

  describe "betaReduce" $ do
    it "reduces simple applications" $ do
      let e1 = Abs "x" (Var "x")
          e2 = (Var "y")
      betaReduce e1 e2 `shouldBe` Var "y"

    it "reduces nested abstractions" $ do
      let e1 = Abs "x" (Abs "y" (Var "x"))
          e2 = Var "z"
      betaReduce e1 e2 `shouldBe` Abs "y" (Var "z")

    it "does not reduce unreducible expression" $ do
      let e1 = Var "x"
          e2 = Var "y"
      betaReduce e1 e2 `shouldBe` App (Var "x") (Var "y")
      
