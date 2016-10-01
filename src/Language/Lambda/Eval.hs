module Language.Lambda.Eval where

import Language.Lambda.Expression

evalExpr :: Eq n => LambdaExpr n -> LambdaExpr n
evalExpr (App e1 e2) = betaReduce e1 e2
evalExpr e           = e

betaReduce :: Eq n => LambdaExpr n -> LambdaExpr n -> LambdaExpr n
betaReduce (Abs n e1) e2 = sub n e1 e2
betaReduce e1 e2 = App e1 e2

sub :: Eq n => n -> LambdaExpr n -> LambdaExpr n -> LambdaExpr n
sub name b@(Var name') expr | name == name' = expr
                            | otherwise = b
sub name (Abs name' expr') expr = Abs name' (sub name expr' expr)
sub _ body _ = body
