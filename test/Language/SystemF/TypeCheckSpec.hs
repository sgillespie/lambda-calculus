module Language.SystemF.TypeCheckSpec (spec) where

import Data.Map

import Test.Hspec

import Language.SystemF.Expression
import Language.SystemF.TypeCheck

tc uniqs ctx = typecheck uniqs (fromList ctx)

spec :: Spec
spec = describe "typecheck" $ do
  it "typechecks simple variables in context" $
    tc [] [("x", TyVar "X")] (Var "x") `shouldBe` Right (TyVar "X")

  it "typechecks simple variables not in context" $ 
    tc ["A"] [] (Var "x") `shouldBe` Right (TyVar "A")

  it "typechecks simple abstractions" $
    tc [] [] (Abs "x" (TyVar "A") (Var "x")) 
      `shouldBe` Right (TyArrow (TyVar "A") (TyVar "A"))
