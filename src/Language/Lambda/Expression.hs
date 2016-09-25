{-# LANGUAGE FlexibleInstances #-}
module Language.Lambda.Expression where

import Prelude hiding (abs, uncurry)

import Language.Lambda.PrettyPrint

data LambdaExpr name
  = Var name
  | App (LambdaExpr name) (LambdaExpr name)
  | Abs name (LambdaExpr name)
  deriving (Eq, Show)

-- Pretty printing
instance PrettyPrint a => PrettyPrint (LambdaExpr a) where
  prettyPrint = prettyPrint . pprExpr empty

-- Pretty print a lambda expression
pprExpr :: PrettyPrint n => PDoc String -> LambdaExpr n -> PDoc String
pprExpr pdoc (Var n)      = prettyPrint n `add` pdoc
pprExpr pdoc (Abs n body) = pprAbs pdoc n body
pprExpr pdoc (App e1 e2)  = pprApp pdoc e1 e2

-- Pretty print an abstraction 
pprAbs :: PrettyPrint n => PDoc String -> n -> LambdaExpr n -> PDoc String
pprAbs pdoc n body
  = between vars' "\\" ". " (pprExpr pdoc body')
  where (vars, body') = uncurry n body
        vars' = intercalate (map prettyPrint vars) " " empty

-- Pretty print an application
pprApp :: PrettyPrint n
        => PDoc String
        -> LambdaExpr n
        -> LambdaExpr n
        -> PDoc String
pprApp pdoc e1 e2@(App _ _) = pprExpr pdoc e1
  `mappend` addSpace (betweenParens (pprExpr pdoc e2) pdoc)
pprApp pdoc e1 e2@(Abs _ _) = pprExpr pdoc e1
  `mappend` addSpace (betweenParens (pprExpr pdoc e2) pdoc)
pprApp pdoc e1@(Abs _ _) e2 = betweenParens (pprExpr pdoc e1) pdoc
  `mappend` addSpace (pprExpr pdoc e2)
pprApp pdoc e1 e2
  = pprExpr pdoc e1 `mappend` addSpace (pprExpr pdoc e2)

uncurry :: n -> LambdaExpr n -> ([n], LambdaExpr n)
uncurry n body = uncurry' [n] body
  where uncurry' ns (Abs n' body') = uncurry' (n':ns) body'
        uncurry' ns body'          = (reverse ns, body')
