module Language.Lambda.HspecUtils where

import Test.Hspec

import Language.Lambda

shouldEvalTo :: String -> String -> Expectation
shouldEvalTo s1 = shouldBe (evalString s1) . evalString
