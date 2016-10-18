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

    describe "add" $ do
      -- add(m, n) = m + n
      --
      -- It is defined by applying successor m times on n:
      -- add = \m n f x. m f (n f x)
      it "add 0 2 = 2" $ do
        "(\\m n f x. m f (n f x)) (\\f x. x) (\\f x. f (f x))"
          `shouldEvalTo` "\\f x. f (f x)"

      it "add 3 2 = 5" $ do
        "(\\m n f x. m f (n f x)) (\\f x. f (f (f x))) (\\f x. f (f x))"
          `shouldEvalTo` "\\f x. f (f (f (f (f x))))"

      -- Here, we use `\f x. n f x` instead of `n`. This is because
      -- I haven't implemented eta conversion
      it "add 0 n = n" $ do
        "(\\m n f x. m f (n f x)) (\\f x. x) n"
          `shouldEvalTo` "\\f x. n f x"

    describe "multiply" $ do
      -- multiply(m, n) = m * n
      --
      -- multiply is defined by applying add m times
      -- multiply = \m n f x. m (n f x) x)
      --
      -- Using eta conversion, we can omit the parameter x
      -- multiply = \m n f. m (n f)
      it "multiply 0 2 = 0" $ do
        "(\\m n f. m (n f)) (\\f x. x) (\\f x. f (f x))"
          `shouldEvalTo` "\\f x. x"

      it "multiply 2 3 = 6" $ do
        "(\\m n f. m (n f)) (\\f x. f (f x)) (\\f x. f (f (f x)))"
          `shouldEvalTo` "\\f x. f (f (f (f (f (f x)))))"

      it "multiply 0 n = 0" $ do
        "(\\m n f. m (n f)) (\\f x. x) n"
          `shouldEvalTo` "\\f x. x"

      it "multiply 1 n = n" $ do
        "(\\m n f. m (n f)) (\\f x. f x) n"
          `shouldEvalTo` "\\f x. n f x"

    describe "power" $ do
      -- The function power raises m to the power of n.
      -- power(m, n) = m^n
      --
      -- power is defined by applying multiply n times
      -- power = \m n f x. (n m) f x
      --
      -- Using eta conversion again, we can omit the parameter f
      -- power = \m n = n m

      -- NOTE: Here we use the first form to get more predictable
      -- variable names. Otherwise, alpha conversion will choose a random
      -- unique variable.
      it "power 0 1 = 0" $ do
        "(\\m n f x. (n m) f x) (\\f x. x) (\\f x. f x)"
          `shouldEvalTo` "\\f x. x"

      it "power 2 3 = 8" $ do
        "(\\m n f x. (n m) f x) (\\f x. f (f x)) (\\f x. f (f (f x)))"
          `shouldEvalTo` "\\f x. f (f (f (f (f (f (f (f x)))))))"
