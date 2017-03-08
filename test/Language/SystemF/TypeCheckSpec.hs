module Language.SystemF.TypeCheckSpec (spec) where

import Data.Either
import Data.Map

import Test.Hspec

import Language.Lambda.Util.PrettyPrint
import Language.SystemF.Expression
import Language.SystemF.TypeCheck

tc :: (Ord n, Eq n, PrettyPrint n)
          => UniqueSupply n 
          -> [(n, Ty n)]
          -> SystemFExpr n n 
          -> Either String (Ty n)
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

  it "typechecks simple applications" $ do
    let ctx = [
          ("f", TyArrow (TyVar "T") (TyVar "U")),
          ("a", TyVar "T")
          ]

    tc [] ctx (App (Var "f") (Var "a")) `shouldBe` Right (TyVar "U")

  it "apply variable to variable fails" $ do
    let ctx = [
          ("a", TyVar "A"),
          ("b", TyVar "B")
          ]

    tc ["C"] ctx (App (Var "a") (Var "b")) 
      `shouldSatisfy` isLeft

  it "apply arrow to variable of wrong type fails" $ do
    let ctx = [
          ("f", TyArrow (TyVar "F") (TyVar "G")),
          ("b", TyVar "B")
          ]

    tc [] ctx (App (Var "f") (Var "b")) `shouldSatisfy` isLeft

  it "typechecks simple type abstractions" $
    tc ["A"] [] (TyAbs "X" (Var "x")) `shouldBe` Right (TyForAll "X" (TyVar "A"))

  it "typechecks type abstractions with simple abstraction" $
    tc [] [] (TyAbs "X" (Abs "x" (TyVar "X") (Var "x"))) 
      `shouldBe` Right (TyForAll "X" (TyArrow (TyVar "X") (TyVar "X")))

  it "typechecks type abstractions with application" $
    tc [] [("y", TyVar "Y")] 
      (App (TyApp (TyAbs "X" (Abs "x" (TyVar "X") (Var "x"))) (TyVar "Y")) 
           (Var "y"))
      `shouldBe` Right (TyVar "Y")

  it "typechecks simple type applications" $
    tc [] [("x", TyVar "A")] (TyApp (TyAbs "X" (Var "x")) (TyVar "X"))
      `shouldBe` Right (TyVar "A")

  it "typechecks type applications with simple abstraction" $
    tc [] [] (TyApp (TyAbs "X" (Abs "x" (TyVar "X") (Var "x"))) (TyVar "Y"))
      `shouldBe` Right (TyArrow (TyVar "Y") (TyVar "Y"))
