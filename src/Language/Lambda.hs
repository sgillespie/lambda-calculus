module Language.Lambda (
  LambdaExpr(..),
  PrettyPrint(..),
  evalExpr,
  evalString,
  parseExpr,
  uniques,
  ) where

import Control.Monad
import Text.Parsec

import Language.Lambda.Eval
import Language.Lambda.Expression
import Language.Lambda.Parser
import Language.Lambda.PrettyPrint

evalString :: String -> Either ParseError (LambdaExpr String)
evalString = liftM (evalExpr uniques) . parseExpr

-- TODO[sgillespie]: Uniques should be [a..z, a0..z0, a1..z1] etc
-- concatMap (\x -> map (\y -> y:x) ['a'..'z']) ([""] ++ map show [0..])
  
uniques :: [String]
uniques = concatMap (\p -> map (:p) . reverse $ ['a'..'z']) suffix
  where suffix = [""] ++ map show [(0::Int)..]

