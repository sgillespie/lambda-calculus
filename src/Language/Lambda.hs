module Language.Lambda (
  LambdaExpr(..),
  PrettyPrint(..),
  evalExpr,
  evalString,
  parseExpr
  ) where

import Control.Monad
import Text.Parsec

import Language.Lambda.Eval
import Language.Lambda.Expression
import Language.Lambda.Parser
import Language.Lambda.PrettyPrint

evalString :: String -> Either ParseError (LambdaExpr String)
evalString = liftM (evalExpr uniques) . parseExpr

uniques :: [String]
uniques = map (:[]) ['z'..'a']
