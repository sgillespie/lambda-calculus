module Language.Lambda.Eval where

import Data.List
import Data.Maybe

import Language.Lambda.Expression

evalExpr :: Eq n => LambdaExpr n -> LambdaExpr n
evalExpr (App e1@(Var _) e2) = betaReduce e1 (evalExpr e2)
evalExpr (App e1         e2) = betaReduce (evalExpr e1)  e2
evalExpr (Abs name expr)     = Abs name (evalExpr expr)
evalExpr e = e

betaReduce :: Eq n => LambdaExpr n -> LambdaExpr n -> LambdaExpr n
betaReduce (Abs n expr) = evalExpr . sub n expr
betaReduce (App e1 e1') = App . betaReduce e1 $ e1'
betaReduce expr@(Var _) = App expr

alphaConvert :: Eq n => [n] -> [n] -> LambdaExpr n -> LambdaExpr n
alphaConvert uniqs freeVars expr@(Abs name body)
  | name `elem` freeVars = Abs uniq (sub name body (Var uniq))
  | otherwise = expr
  where uniq = fromMaybe name (find (`notElem` freeVars) uniqs)
alphaConvert _ _ e = e

sub :: Eq n => n -> LambdaExpr n -> LambdaExpr n -> LambdaExpr n
sub name b@(Var name') expr | name == name' = expr
                            | otherwise = b

sub name b@(Abs name' expr') expr | name == name' = b
  | otherwise = Abs name' (sub name expr' expr)

sub name (App e1 e2) expr = App (sub name e1 expr)
                                (sub name e2 expr)
