module Language.SystemF (
  PrettyPrint(..),
  SystemFExpr(..),
  evalString
  ) where

import Text.Parsec

import Language.Lambda.Util.PrettyPrint
import Language.SystemF.Expression

-- TODO
evalString :: String -> Either ParseError (SystemFExpr String String)
evalString = undefined

