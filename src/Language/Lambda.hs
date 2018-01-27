{-# LANGUAGE FlexibleInstances #-}
module Language.Lambda (
  Globals(..),
  LambdaExpr(..),
  ParseError(..),
  PrettyPrint(..),
  evalExpr,
  evalString,
  parseExpr,
  uniques,
  ) where

import Control.Monad
import Text.Parsec

import qualified Data.Map as Map

import Language.Lambda.Eval
import Language.Lambda.Expression
import Language.Lambda.Parser
import Language.Lambda.Util.PrettyPrint

type Globals = Map.Map String (LambdaExpr String)

evalString :: Globals
           -> String
           -> Either ParseError (LambdaExpr String, Globals)
evalString globals str = evalExpr globals uniques <$> parseExpr str

uniques :: [String]
uniques = concatMap (\p -> map (:p) . reverse $ ['a'..'z']) suffix
  where suffix = "" : map show [(0::Int)..]

