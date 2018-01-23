module Language.Lambda.HspecUtils where

import Data.Map (empty)
import Test.Hspec

import Language.Lambda

shouldEvalTo :: String -> String -> Expectation
shouldEvalTo s1 = shouldBe (eval s1) . eval

eval :: String -> Either ParseError (LambdaExpr String)
eval = fmap fst . evalString empty
