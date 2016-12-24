module Language.Lambda.Util.Eval where

import Text.Parsec

class Eval e where
  evalString :: String -> Either ParseError e
