module Language.SystemF (
  Globals(..),
  PrettyPrint(..),
  SystemFExpr(..),
  evalString,
  parseExpr
  ) where

import Text.Parsec

import qualified Data.Map as Map

import Language.Lambda.Util.PrettyPrint
import Language.SystemF.Expression
import Language.SystemF.Parser

type Globals = Map.Map String (SystemFExpr String String)

evalString :: Globals
           -> String
           -> Either ParseError (SystemFExpr String String, Globals)
evalString globals = fmap (flip (,) globals) . parseExpr

