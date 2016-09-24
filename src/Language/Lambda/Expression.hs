{-# LANGUAGE FlexibleInstances #-}
module Language.Lambda.Expression where

import Prelude hiding (abs, uncurry)
import Data.List

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
pprExpr (Var n)       = ppr n
pprExpr abs@(Abs _ _) = pprAbs abs
pprExpr app@(App _ _) = pprApp app

pprAbs :: Outputtable a => LambdaExpr a -> String
pprAbs (Abs n expr) = "\\" ++ pprVarList vars ++ ". " ++ ppr expr'
  where (vars, expr') = uncurry n expr
pprAbs _ = ""

pprApp :: Outputtable a => LambdaExpr a -> String
pprApp (App e1 e2@(App _ _)) = ppr e1 ++ " (" ++ ppr e2 ++ ")"
pprApp (App e1 e2@(Abs _ _)) = ppr e1 ++ " (" ++ ppr e2 ++ ")"
pprApp (App e1@(Abs _ _) e2) = "(" ++ ppr e1 ++ ") " ++ ppr e2
pprApp (App e1 e2) = ppr e1 ++ " " ++ ppr e2
pprApp _ = ""

pprVarList :: Outputtable a => [a] -> String
pprVarList = intercalate " " . map ppr

uncurry :: n -> LambdaExpr n -> ([n], LambdaExpr n)
uncurry n body = uncurry' [n] body
  where uncurry' ns (Abs n' body') = uncurry' (n':ns) body'
        uncurry' ns body'          = (reverse ns, body')
