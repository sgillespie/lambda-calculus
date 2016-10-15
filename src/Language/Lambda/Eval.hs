module Language.Lambda.Eval where

import Data.List
import Data.Maybe

import Language.Lambda.Expression

evalExpr :: Eq n => LambdaExpr n -> LambdaExpr n
evalExpr expr = evalExpr' [] [] expr

evalExpr' :: Eq n => [n] -> [n] -> LambdaExpr n -> LambdaExpr n
evalExpr' uniqs fvs (App e1@(Var _) e2)
  = betaReduce uniqs fvs e1 (evalExpr' uniqs fvs e2)
evalExpr' uniqs fvs (App e1         e2)
  = betaReduce uniqs fvs (evalExpr' uniqs fvs e1)  e2
evalExpr' uniqs fvs (Abs name expr)
  = Abs name (evalExpr' uniqs fvs expr)
evalExpr' _ _ e = e

betaReduce :: Eq n => [n] -> [n]
           -> LambdaExpr n -> LambdaExpr n -> LambdaExpr n
betaReduce uniqs fvs (Abs n expr) = evalExpr' uniqs fvs . sub n expr
betaReduce uniqs fvs (App e1 e1') = App . betaReduce uniqs fvs e1 $ e1'
betaReduce _ _ expr@(Var _) = App expr

alphaConvert :: Eq n => [n] -> [n] -> LambdaExpr n -> LambdaExpr n
alphaConvert uniqs freeVars expr@(Abs name body)
  | name `elem` freeVars = Abs uniq (sub name body (Var uniq))
  | otherwise = expr
  where uniq = fromMaybe name (find (`notElem` freeVars) uniqs)
alphaConvert _ _ e = e

sub :: Eq n => n -> LambdaExpr n -> LambdaExpr n -> LambdaExpr n
sub name b@(Var name') expr | name == name' = expr
                            | otherwise = b

sub name b@(Abs name' expr') expr
  | name == name' = b
  | otherwise     = Abs name' (sub name expr' expr)

sub name (App e1 e2) expr = App (sub name e1 expr)
                                (sub name e2 expr)

freeVarsOf :: Eq n => LambdaExpr n -> [n]
freeVarsOf (Abs n expr) = filter (/=n) . freeVarsOf $ expr
freeVarsOf (App e1 e2)  = freeVarsOf e1 ++ freeVarsOf e2
freeVarsOf (Var n)      = [n]
