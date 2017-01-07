module Language.SystemF.TypeCheck where

import Data.Map
import Prelude hiding (lookup)

import Language.SystemF.Expression

type UniqueSupply n = [n]
type Context n t = Map n t

typecheck :: (Ord n, Eq t)
          => UniqueSupply t 
          -> Context n (Ty t)
          -> SystemFExpr n t 
          -> Either String (Ty t)
typecheck uniqs ctx (Var v)        = tcVar uniqs ctx v
typecheck uniqs ctx (Abs n t body) = tcAbs uniqs ctx n t body
typecheck _     _   _       = undefined

tcVar :: (Ord n, Eq t)
      => UniqueSupply t
      -> Context n (Ty t)
      -> n
      -> Either String (Ty t)
tcVar uniqs ctx var = maybe (TyVar <$> unique uniqs) return (lookup var ctx)

tcAbs :: (Ord n, Eq t)
      => UniqueSupply t
      -> Context n (Ty t)
      -> n
      -> Ty t
      -> SystemFExpr n t
      -> Either String (Ty t)
tcAbs uniqs ctx name ty body = TyArrow ty <$> typecheck uniqs ctx' body
  where ctx' = insert name ty ctx

-- Utilities
unique :: UniqueSupply t
       -> Either String t
unique (u:_) = return u
unique _     = fail "Unique supply ran out"
