module Language.Lambda.Expression (LambdaExpr(..)) where

data LambdaExpr name
  = Var name
  | App (LambdaExpr name) (LambdaExpr name)
  | Abs name (LambdaExpr name)
  deriving (Eq, Show)
