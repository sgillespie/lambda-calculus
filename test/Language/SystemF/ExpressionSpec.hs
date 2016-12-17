{-# LANGUAGE FlexibleInstances #-}
module Language.SystemF.ExpressionSpec where

import Test.Hspec

import Language.Lambda.PrettyPrint
import Language.SystemF.Expression

spec :: Spec
spec = describe "prettyPrint" $ do
  it "prints simple variables" $
    prettyPrint' (Var "x") `shouldBe` "x"

  it "prints simple applications" $
    prettyPrint' (App (Var "a") (Var "b")) `shouldBe` "a b"

  it "prints simple abstracctions" $ 
    prettyPrint (Abs "x" "T" (Var "x")) `shouldBe` "λ x:T. x"


