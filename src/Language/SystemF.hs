module Language.SystemF (
  PrettyPrint(..),
  SystemFExpr(..),
  evalString,
  parseExpr
  ) where

import Text.Parsec

import Language.Lambda.Util.PrettyPrint
import Language.SystemF.Expression
import Language.SystemF.Parser

evalString :: String -> Either ParseError (SystemFExpr String String)
evalString = parseExpr

