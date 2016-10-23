module Language.Lambda.Examples.PairSpec where

import Language.Lambda.HspecUtils

import Test.Hspec

spec :: Spec
spec = do
  describe "Pair" $ do
    -- Pair is the definition of tuples with two items. Pairs,
    -- again are represented using Church Encodings:
    --
    -- pair = \x y f. f x y
    describe "first" $ do
      -- The function first returns the first item in a pair
      -- first(x, y) = x
      --
      -- first is defined by
      -- first = \p. p (\t f. t)
      it "first 0 1 = 0" $ do
        "(\\p. p (\\t f. t)) ((\\x y f. f x y) (\\f x. x) (\\f x. f x))"
          `shouldEvalTo` "\\f x. x"

      it "first x y = x" $ do
        "(\\p. p (\\t f. t)) ((\\x y f. f x y) x y)" `shouldEvalTo` "x"

    describe "second" $ do
      -- The function second returns the second item in a pair
      -- second(x, y) = y
      --
      -- second is defined by
      -- second = \p. p (\t f. f)
      it "second 0 1 = 1" $ do
        "(\\p. p (\\t f. f)) ((\\x y f. f x y) (\\f x. x) (\\f x. f x))"
          `shouldEvalTo` "\\f x. f x"

      it "second x y = y" $ do
        "(\\p. p (\\t f. f)) ((\\x y f. f x y) x y)" `shouldEvalTo` "y"
        "(\\p. p (\\x y z. x)) ((\\x y z f. f x y z) x y z)" `shouldEvalTo` "x"
