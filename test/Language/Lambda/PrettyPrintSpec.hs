module Language.Lambda.PrettyPrintSpec where

import Test.Hspec

import Language.Lambda.PrettyPrint
  
spec :: Spec
spec = describe "PDoc" $ do
  it "pretty prints empty" $
    prettyPrint' empty `shouldBe` ""

  it "pretty prints added components" $ do
    let pdoc = add "f" (add "x" empty)
    prettyPrint' pdoc `shouldBe` "fx"

  it "pretty prints appended components" $ do
    let pdoc = append ["f", "x", "y"] empty
    prettyPrint' pdoc `shouldBe` "fxy"

  it "pretty prints between parens" $ do
    let pdoc = between (PDoc ["f"]) "(" ")" empty
    prettyPrint' pdoc `shouldBe` "(f)"

    let pdoc' = betweenParens (PDoc ["f"]) empty
    prettyPrint' pdoc' `shouldBe` "(f)"

  it "pretty prints intercalated spaces" $ do
    let pdoc = intercalate ["f", "x", "y"] [space] empty
    prettyPrint' pdoc `shouldBe` "f x y"

  it "pretty prints lambda" $ do
    let pdoc = between (PDoc ["x"]) "\\" ". " (add "x" empty)
    prettyPrint' pdoc `shouldBe` "\\x. x"

prettyPrint' :: PDoc String -> String
prettyPrint' = prettyPrint
