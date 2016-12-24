module Language.Lambda.HspecUtils where

import Test.Hspec

import Language.Lambda

shouldEvalTo :: String -> String -> Expectation
shouldEvalTo s1 = shouldBe (eval s1) . eval

eval :: String -> Either ParseError (LambdaExpr String)
eval = evalString
