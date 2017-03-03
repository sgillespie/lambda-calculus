module Language.SystemF.TypeCheck where

import Data.Map
import Prelude hiding (lookup)

import Language.Lambda.Util.PrettyPrint
import Language.SystemF.Expression

type UniqueSupply n = [n]
type Context n t = Map n t

typecheck :: (Ord n, Eq n, PrettyPrint n)
          => UniqueSupply n 
          -> Context n (Ty n)
          -> SystemFExpr n n 
          -> Either String (Ty n)
typecheck uniqs ctx (Var v)        = tcVar uniqs ctx v
typecheck uniqs ctx (Abs n t body) = tcAbs uniqs ctx n t body
typecheck uniqs ctx (App e1 e2)    = tcApp uniqs ctx e1 e2
typecheck uniqs ctx (TyAbs t body) = tcTyAbs uniqs ctx t body
typecheck _     _   _       = undefined

tcVar :: (Ord n, Eq n, PrettyPrint n)
      => UniqueSupply n
      -> Context n (Ty n)
      -> n
      -> Either String (Ty n)
tcVar uniqs ctx var = maybe (TyVar <$> unique uniqs) return (lookup var ctx)

tcAbs :: (Ord n, Eq n, PrettyPrint n)
      => UniqueSupply n
      -> Context n (Ty n)
      -> n
      -> Ty n
      -> SystemFExpr n n
      -> Either String (Ty n)
tcAbs uniqs ctx name ty body = TyArrow ty <$> typecheck uniqs ctx' body
  where ctx' = insert name ty ctx

tcApp :: (Ord n, Eq n, PrettyPrint n)
      => UniqueSupply n
      -> Context n (Ty n)
      -> SystemFExpr n n
      -> SystemFExpr n n
      -> Either String (Ty n)
tcApp uniqs ctx e1 e2 = do
    t1 <- typecheck uniqs ctx e1
    t2 <- typecheck uniqs ctx e2

    -- Unwrap t1; Should be (t2 -> *)
    (t2', t3) <- either genMismatchVar return (arrow t1)

    if t2' == t2
      then return t3
      else Left $ tyMismatchMsg (TyArrow t2 t3) (TyArrow t1 t3)

  where genMismatchVar expected = tyMismatchMsg expected <$> unique uniqs >>= Left
        arrow (TyArrow t1 t2) = return (t1, t2)
        arrow t               = Left t

tcTyAbs :: (Ord n, Eq n, PrettyPrint n)
        => UniqueSupply n
        -> Context n (Ty n)
        -> n
        -> SystemFExpr n n
        -> Either String (Ty n)
tcTyAbs uniqs ctx ty body = TyForAll ty <$> typecheck uniqs ctx' body
  where ctx' = insert ty (TyVar ty) ctx

-- Utilities
unique :: UniqueSupply t
       -> Either String t
unique (u:_) = return u
unique _     = fail "Unique supply ran out"

tyMismatchMsg :: (PrettyPrint t, PrettyPrint t')
              => t
              -> t'
              -> String
tyMismatchMsg expected actual = "Couldn't match expected type " ++
                                prettyPrint expected ++
                                " with actual type " ++
                                prettyPrint actual
