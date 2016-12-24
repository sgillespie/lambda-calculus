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
import Language.Lambda.Util.PrettyPrint

evalString :: String -> Either ParseError (LambdaExpr String)
evalString = fmap (evalExpr uniques) . parseExpr

uniques :: [String]
uniques = concatMap (\p -> map (:p) . reverse $ ['a'..'z']) suffix
  where suffix = "" : map show [(0::Int)..]

