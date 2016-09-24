{-# LANGUAGE FlexibleInstances #-}
module Language.Lambda.Expression where

data LambdaExpr name
  = Var name
  | App (LambdaExpr name) (LambdaExpr name)
  | Abs name (LambdaExpr name)
  deriving (Eq, Show)

class Outputtable a where
  ppr :: a -> String

instance Outputtable [Char] where
  ppr = id

instance Outputtable a => Outputtable (LambdaExpr a) where
  ppr = pprExpr

pprExpr :: Outputtable a => LambdaExpr a -> String 
pprExpr (Var n) = ppr n
pprExpr (Abs n expr) = "\\" ++ ppr n ++ ". " ++ ppr expr
pprExpr (App e1 e2) = ppr e1 ++ " " ++ ppr e2
