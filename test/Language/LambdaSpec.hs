module Language.LambdaSpec where

import Test.Hspec

import Language.Lambda

spec :: Spec
spec = do
  describe "evalString" $ do
    it "evaluates simple strings" $ do
      evalString "x" `shouldBe` Right (Var "x")
      evalString "\\x. x" `shouldBe` Right (Abs "x" (Var "x"))
      evalString "f y" `shouldBe` Right (App (Var "f") (Var "y"))

    it "reduces simple applications" $
      evalString "(\\x .x) y" `shouldBe` Right (Var "y")

    it "reduces applications with nested redexes" $
      evalString "(\\f x. f x) (\\y. y)" `shouldBe` Right (Abs "x" (Var "x"))

  describe "uniques" $ do
    let alphabet = reverse ['a'..'z']
        len = length alphabet
    
    it "starts with plain alphabet" $
      take len uniques `shouldBe` map (:[]) alphabet

    it "adds index afterwards" $
      take len (drop len uniques) `shouldBe` map (:['0']) alphabet

