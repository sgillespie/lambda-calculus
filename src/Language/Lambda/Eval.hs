module Language.Lambda.Eval where

import Data.List
import Data.Maybe

import qualified Data.Map as Map

import Language.Lambda.Expression

-- | Evaluate an expression
evalExpr :: (Eq n, Ord n)
         => Map.Map n (LambdaExpr n) -- ^ globals
         -> [n]                      -- ^ unique supply
         -> LambdaExpr n             -- ^ the expression to evaluate
         -> (LambdaExpr n, Map.Map n (LambdaExpr n))
evalExpr globals uniqs (Let name expr)
  = (Let name expr', Map.insert name expr' globals)
  where expr' = evalExpr' uniqs (subGlobals globals uniqs expr)
evalExpr globals uniqs expr = (evalExpr' uniqs expr', globals)
  where expr' = subGlobals globals uniqs expr

subGlobals :: (Eq n, Ord n)
           => Map.Map n (LambdaExpr n) -- ^ globals
           -> [n]                      -- ^ unique supply
           -> LambdaExpr n             -- ^ the expression
           -> LambdaExpr n
subGlobals globals uniqs expr@(Var x) = Map.findWithDefault expr x globals
subGlobals globals uniqs (App e1 e2)  = App (subGlobals globals uniqs e1)
                                            (subGlobals globals uniqs e2)
subGlobals globals uniqs (Abs n expr) = Abs n expr'
  where expr' | Map.member n globals = expr
              | otherwise            = subGlobals globals uniqs expr
        fvs = freeVarsOf expr
subGlobals _ _ expr = expr

-- | Evaluate an expression; does not support `let`
evalExpr' :: Eq n
          => [n]                      -- ^ unique supply
          -> LambdaExpr n             -- ^ the expression to evaluate
          -> LambdaExpr n
evalExpr' _     expr@(Var _) = expr
evalExpr' uniqs (Abs name expr) = Abs name . evalExpr' uniqs $ expr
evalExpr' uniqs (Let name expr) = Let name . evalExpr' uniqs $ expr
evalExpr' uniqs (App e1 e2)     = betaReduce uniqs (evalExpr' uniqs e1)
                                                   (evalExpr' uniqs e2)

betaReduce :: Eq n
           => [n]
           -> LambdaExpr n
           -> LambdaExpr n
           -> LambdaExpr n
betaReduce _     expr@(Var _) e2 = App expr e2
betaReduce uniqs (App e1 e1') e2 = App (betaReduce uniqs e1 e1') e2
betaReduce uniqs (Abs n  e1)  e2 = evalExpr' uniqs . sub n e1' $ e2
  where fvs = freeVarsOf e2
        e1' = alphaConvert uniqs fvs e1

alphaConvert :: Eq n => [n] -> [n] -> LambdaExpr n -> LambdaExpr n
alphaConvert uniqs freeVars (Abs name body)
  | name `elem` freeVars = Abs uniq . sub name body . Var $ uniq
  | otherwise            = Abs name . alphaConvert uniqs freeVars $ body
  where uniq = fromMaybe name (find (`notElem` freeVars) uniqs)
alphaConvert _ _ e = e

etaConvert :: Eq n => LambdaExpr n -> LambdaExpr n
etaConvert (Abs n (App e1 (Var n')))
  | n == n'   = etaConvert e1
  | otherwise = Abs n (App (etaConvert e1) (Var n'))
etaConvert (Abs n e@(Abs _ _)) 
  -- If `etaConvert e == e` then etaConverting it will create an infinite loop
  | e == e'   = Abs n e'
  | otherwise = etaConvert (Abs n e')
  where e' = etaConvert e
etaConvert (Abs n expr) = Abs n (etaConvert expr)
etaConvert (App e1 e2)  = App (etaConvert e1) (etaConvert e2)
etaConvert expr@(Var _) = expr

sub :: Eq n => n -> LambdaExpr n -> LambdaExpr n -> LambdaExpr n
sub name b@(Var name') expr
  | name == name' = expr
  | otherwise     = b

sub name b@(Abs name' expr') expr
  | name == name' = b
  | otherwise     = Abs name' (sub name expr' expr)

sub name (App e1 e2) expr = App (sub name e1 expr)
                                (sub name e2 expr)

freeVarsOf :: Eq n => LambdaExpr n -> [n]
freeVarsOf (Abs n expr) = filter (/=n) . freeVarsOf $ expr
freeVarsOf (App e1 e2)  = freeVarsOf e1 ++ freeVarsOf e2
freeVarsOf (Var n)      = [n]
