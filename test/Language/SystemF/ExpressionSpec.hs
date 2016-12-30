{-# LANGUAGE FlexibleInstances #-}
module Language.SystemF.ExpressionSpec where

import Test.Hspec

import Language.Lambda.Util.PrettyPrint
import Language.SystemF.Expression

spec :: Spec
spec = describe "prettyPrint" $ do
  it "prints simple variables" $
    prettyPrint' (Var "x") `shouldBe` "x"

  it "prints simple applications" $
    prettyPrint' (App (Var "a") (Var "b")) `shouldBe` "a b"

  it "prints simple abstractions" $ 
    prettyPrint (Abs "x" (TyVar "T") (Var "x")) `shouldBe` "λ x:T. x"

  it "prints simple type abstractions" $
    prettyPrint (TyAbs (TyVar "X") (Var "x")) `shouldBe` "Λ X. x"

  it "prints simple type applications" $ 
    prettyPrint' (TyApp (Var "t") (TyVar "T")) `shouldBe` "t [T]"

  it "prints nested abstractions" $
    prettyPrint (Abs "f" (TyVar "F") (Abs "x" (TyVar "X") (Var "x")))
      `shouldBe` "λ f:F x:X. x"

  it "prints abstractions with composite types" $ 
    prettyPrint (Abs "f" (TyArrow (TyVar "X") (TyVar "Y")) (Var "f"))
      `shouldBe ` "λ f:(X->Y). f"

  it "prints nested type abstractions" $
    prettyPrint (TyAbs (TyVar "A") (TyAbs (TyVar "B") (Var "x")))
      `shouldBe` "Λ A B. x"

  it "prints nested applications" $
    prettyPrint' (App (App (Var "f") (Var "x")) (Var "y"))
      `shouldBe` "f x y"

  it "prints parenthesized applications" $ do
    prettyPrint' (App (Var "w") (App (Var "x") (Var "y")))
      `shouldBe` "w (x y)"

    prettyPrint (App (Abs "t" (TyVar "T") (Var "t")) (Var "x"))
      `shouldBe` "(λ t:T. t) x"

    prettyPrint (App (Abs "f" (TyVar "F") (Var "f")) (Abs "g" (TyVar "G") (Var "g")))
      `shouldBe` "(λ f:F. f) (λ g:G. g)"

  it "prints simple types" $
    prettyPrint (TyVar "X") `shouldBe` "X"

  it "print simple arrow types" $
    prettyPrint (TyArrow (TyVar "A") (TyVar "B")) `shouldBe` "A -> B"

  it "prints chained arrow types" $
    prettyPrint (TyArrow (TyVar "X") (TyArrow (TyVar "Y") (TyVar "Z")))
      `shouldBe` "X -> Y -> Z"

  it "prints nested arrow types" $
    prettyPrint (TyArrow (TyArrow (TyVar "T") (TyVar "U")) (TyVar "V"))
      `shouldBe` "(T -> U) -> V"
