module Language.Lambda.EvalSpec where

import Data.Map (fromList, empty, insert)
import Test.Hspec

import Language.Lambda
import Language.Lambda.Eval
import Language.Lambda.Expression

spec :: Spec
spec = do
  describe "evalExpr" $ do
    let evalExpr' = fst <$> evalExpr empty uniques
    
    it "beta reduces" $ do
      let expr = App (Abs "x" (Var "x")) (Var "z")
      evalExpr' expr `shouldBe` Var "z"

    it "reduces multiple applications" $ do
      let expr = App (App (Abs "f" (Abs "x" (App (Var "f") (Var "x")))) (Var "g")) (Var "y")
      evalExpr' expr `shouldBe` App (Var "g") (Var "y")

    it "reduces inner redexes" $ do
      let expr = Abs "x" (App (Abs "y" (Var "y")) (Var "x"))
      evalExpr' expr `shouldBe` Abs "x" (Var "x")

    it "reduces with name captures" $ do
      let expr = App (Abs "f" (Abs "x" (App (Var "f") (Var "x"))))
                     (Abs "f" (Var "x"))
      evalExpr' expr `shouldBe` Abs "z" (Var "x")

    it "reduces let bodies" $ do
      let expr = Let "x" $ App (Abs "y" (Var "y")) (Var "z")
      evalExpr' expr `shouldBe` Let "x" (Var "z")

    it "let expressions update state" $ do
      let expr = Let "w" (Var "x")
          (_, globals) = evalExpr empty uniques expr

      globals `shouldBe` insert "w" (Var "x") empty

    it "subs global variables" $ do
      let globals = insert "w" (Var "x") empty
          expr = Var "w"

      fst (evalExpr globals uniques expr)
        `shouldBe` Var "x"

  describe "subGlobals" $ do
    let globals = fromList [("w", Var "x")]
        subGlobals' = subGlobals globals ["a"]
    
    it "subs simple variables" $
      subGlobals' (Var "w") `shouldBe` Var "x"

    it "does not sub shadowed bindings" $ do
      let expr = Abs "w" (Var "w")
      subGlobals' expr `shouldBe` expr

    xit "does not capture globals" $ do
      let expr = Abs "x" (Var "w")
      subGlobals' expr `shouldBe` Abs "a" (Var "x")

  describe "betaReduce" $ do
    let betaReduce' = betaReduce []
    
    it "reduces simple applications" $ do
      let e1 = Abs "x" (Var "x")
          e2 = Var "y"
      betaReduce' e1 e2 `shouldBe` Var "y"

    it "reduces nested abstractions" $ do
      let e1 = Abs "x" (Abs "y" (Var "x"))
          e2 = Var "z"
      betaReduce' e1 e2 `shouldBe` Abs "y" (Var "z")

    it "reduces inner applications" $ do
      let e1 = Abs "f" (App (Var "f") (Var "x"))
          e2 = Var "g"
      betaReduce' e1 e2 `shouldBe` App (Var "g") (Var "x")

    it "does not reduce unreducible expression" $ do
      let e1 = Var "x"
          e2 = Var "y"
      betaReduce' e1 e2 `shouldBe` App (Var "x") (Var "y")

    it "does not reduce irreducible chained applications" $ do
      let e1 = App (Var "x") (Var "y")
          e2 = Var "z"
      betaReduce' e1 e2 `shouldBe` App (App (Var "x") (Var "y")) (Var "z")

    it "does not sub shadowed bindings" $ do
      let e1 = Abs "x" (Abs "x" (Var "x"))
          e2 = Var "z"
      betaReduce' e1 e2 `shouldBe` Abs "x" (Var "x")

  describe "alphaConvert" $ do
    it "alpha converts simple expressions" $ do
      let freeVars = ["x"]
          expr = Abs "x" (Var "x")
          uniques = ["y"]
      alphaConvert uniques freeVars expr `shouldBe` Abs "y" (Var "y")
  
    it "avoids captures" $ do
      let freeVars = ["x"]
          expr = Abs "x" (Var "x")
          uniques = ["x", "y"]
      alphaConvert uniques freeVars expr `shouldBe` Abs "y" (Var "y")

  describe "etaConvert" $ do
    it "eta converts simple expressions" $ do
      let expr = Abs "x" $ App (Var "f") (Var "x")
      etaConvert expr `shouldBe` Var "f" 

    it "eta converts nested applications" $ do
      let expr = Abs "y" $ App (App (Var "f") (Var "x")) (Var "y")
      etaConvert expr `shouldBe` App (Var "f") (Var "x")

      let expr' = Abs "x" $ Abs "y" (App (App (Var "f") (Var "x")) (Var "y"))
      etaConvert expr' `shouldBe` Var "f" 

      let expr'' = Abs "x" (Abs "y" (App (Var "y") (Var "x")))
      etaConvert expr'' `shouldBe` expr''

      let expr''' = Abs "f" (Abs "x" (Var "x"))
      etaConvert expr''' `shouldBe` expr'''

    it "ignores non-eta convertable expressions" $ do
      let expr = Abs "x" $ Var "x"
      etaConvert expr `shouldBe` expr

  describe "freeVarsOf" $ do
    it "Returns simple vars" $
      freeVarsOf (Var "x") `shouldBe` ["x"]
  
    it "Does not return bound vars" $
      freeVarsOf (Abs "x" (Var "x")) `shouldBe` []

    it "Returns nested simple vars" $
      freeVarsOf (Abs "x" (Var "y")) `shouldBe` ["y"]

    it "Returns applied simple vars" $
      freeVarsOf (App (Var "x") (Var "y")) `shouldBe` ["x", "y"]
