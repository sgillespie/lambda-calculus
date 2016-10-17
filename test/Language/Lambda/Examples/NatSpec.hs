module Language.Lambda.Examples.NatSpec where

import Test.Hspec

import Language.Lambda

shouldEvalTo :: String -> String -> Expectation
shouldEvalTo s1 = shouldBe (evalString s1) . evalString

spec :: Spec
spec = do
  describe "Nat" $ do
    -- Nat is the definition of natural numbers. More precisely, Nat
    -- is the set of nonnegative integers.  We represent nats using
    -- Church Encodings:
    --
    -- 0: \f x. x
    -- 1: \f x. f x
    -- 2: \f x. f (f x)
    -- ...and so on

    describe "successor" $ do
      -- successor is a function that adds 1
      -- succ(0) = 1
      -- succ(1) = 2
      -- ... and so forth
      --
      -- successor is defined by
      -- succ = \n f x. f (n f x)

      it "succ 0 = 1" $ do
        "(\\n f x. f (n f x)) (\\f x. x)" `shouldEvalTo` "\\f x. f x"

      it "succ 1 = 2" $ do
        "(\\n f x. f (n f x)) (\\f x. f x)" `shouldEvalTo` "\\f x. f (f x)"
