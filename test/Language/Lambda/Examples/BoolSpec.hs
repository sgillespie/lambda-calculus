module Language.Lambda.Examples.BoolSpec where

import Test.Hspec

import Language.Lambda.HspecUtils

spec :: Spec
spec = describe "Bool" $ do
  -- Bool is the definition of Booleans. We represent bools
  -- using Church Encodings:
  --
  -- true:  \t f. t
  -- false: \t f. f
  describe "and" $ do
    -- The function and takes two Bools and returns true
    -- iff both arguments are true
    -- 
    -- and(true,  true)  = true
    -- and(false, true)  = false
    -- and(true,  false) = false
    -- and(false, false) = false
    --
    -- and is defined by
    -- and = \x y. x y x
    it "true and true = true" $
      "(\\x y. x y x) (\\t f. t) (\\t f. t)" `shouldEvalTo` "\\t f. t"

    it "true and false = false" $
      "(\\x y. x y x) (\\t f. t) (\\t f. f)" `shouldEvalTo` "\\t f. f"
      
    it "false and true = false" $
      "(\\x y. x y x) (\\t f. f) (\\t f. t)" `shouldEvalTo` "\\t f. f"

    it "false and false = false" $
      "(\\x y. x y x) (\\t f. f) (\\t f. f)" `shouldEvalTo` "\\t f. f"

    it "false and p = false" $
      "(\\x y. x y x) (\\t f. f) p" `shouldEvalTo` "\\t f. f"

    it "true and p = false" $
      "(\\x y. x y x) (\\t f. t) p" `shouldEvalTo` "p"

  describe "or" $ do
    -- or takes two Bools and returns true iff either argument is true
    -- 
    -- or(true,  true)  = true
    -- or(true,  false) = true
    -- or(false, true)  = true
    -- or(false, false) = false
    --
    -- or is defined by
    -- or = \x y. x x y
    it "true or true = true" $
      "(\\x y. x x y) (\\t f. t) (\\t f. t)" `shouldEvalTo` "\\t f. t"
    
    it "true or false = true" $
      "(\\x y. x x y) (\\t f. t) (\\t f. f)" `shouldEvalTo` "\\t f. t"
      
    it "false or true = true" $
      "(\\x y. x x y) (\\t f. f) (\\t f. t)" `shouldEvalTo` "\\t f. t"

    it "false or false = false" $
      "(\\x y. x x y) (\\t f. f) (\\t f. f)" `shouldEvalTo` "\\t f. f"

    it "true or p = true" $
      "(\\x y. x x y) (\\t f. t) p" `shouldEvalTo` "\\t f. t"

    it "false or p = p" $
      "(\\x y. x x y) (\\t f. f) p" `shouldEvalTo` "p"
      

  describe "not" $ do
    -- not takes a Bool and returns its opposite value
    --
    -- not(true)  = false
    -- not(false) = true
    --
    -- not is defined by
    -- not = \x. x (\t f. f) (\t f. t)
    it "not true = false" $
      "(\\x. x (\\t f. f) (\\t f. t)) \\t f. t" `shouldEvalTo` "\\t f. f"

    it "not false = true" $
      "(\\x. x (\\t f. f) (\\t f. t)) \\t f. f" `shouldEvalTo` "\\t f. t"
      
  describe "if" $ do
    -- if takes a Bool and two values. If returns the first value
    -- if the Bool is true, and the second otherwise. In other words,
    -- if p x y = if p then x else y
    --
    -- if(true,  x, y) = x
    -- if(false, x, y) = y
    -- 
    -- if is defined by
    -- if = \p x y. p x y
    it "if true 0 1 = 0" $
      "(\\p x y. p x y) (\\t f. t) (\\f x. x) (\\f x. f x)"
        `shouldEvalTo` "\\f x. x"

    it "if false 0 1 = 1" $
      "(\\p x y. p x y) (\\t f. f) (\\f x. x) (\\f x. f x)"
        `shouldEvalTo` "\\f x. f x"

    it "it true p q = p" $
      "(\\p x y. p x y) (\\t f. t) p q" `shouldEvalTo` "p"

    it "it false p q = q" $
      "(\\p x y. p x y) (\\t f. f) p q" `shouldEvalTo` "q"
