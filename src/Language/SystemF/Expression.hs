{-# LANGUAGE FlexibleInstances #-}
module Language.SystemF.Expression where

import Language.Lambda.Util.PrettyPrint

data SystemFExpr name ty
  = Var name                                        -- Variable
  | App (SystemFExpr name ty) (SystemFExpr name ty) -- Application
  | Abs name (Ty ty) (SystemFExpr name ty)          -- Abstraction
  | TyAbs (Ty ty) (SystemFExpr name ty)             -- Type Abstraction
                                                    -- \X. body

  | TyApp (SystemFExpr name ty) (Ty ty)             -- Type Application
                                                    -- x [X]
  deriving (Eq, Show)

data Ty name
  = TyVar name                  -- Type variable (T)
  | TyArrow (Ty name) (Ty name) -- Type arrow    (T -> U)
  deriving (Eq, Show)

-- Pretty printing
instance (PrettyPrint n, PrettyPrint t) => PrettyPrint (SystemFExpr n t) where
  prettyPrint = prettyPrint . pprExpr empty

instance PrettyPrint n => PrettyPrint (Ty n) where
  prettyPrint = prettyPrint . pprTy empty

-- Same as prettyPrint, but we assume the same type for names and types. Useful
-- for testing.
prettyPrint' :: PrettyPrint n => SystemFExpr n n -> String
prettyPrint' = prettyPrint

-- Pretty print a system f expression
pprExpr :: (PrettyPrint n, PrettyPrint t) 
        => PDoc String 
        -> SystemFExpr n t
        -> PDoc String
pprExpr pdoc (Var n)        = prettyPrint n `add` pdoc
pprExpr pdoc (App e1 e2)    = pprApp pdoc e1 e2
pprExpr pdoc (Abs n t body) = pprAbs pdoc n t body
pprExpr pdoc (TyAbs t body) = pprTAbs pdoc t body
pprExpr pdoc (TyApp e ty)   = pprTApp pdoc e ty

-- Pretty print an application
pprApp :: (PrettyPrint n, PrettyPrint t)
       => PDoc String
       -> SystemFExpr n t
       -> SystemFExpr n t
       -> PDoc String
pprApp pdoc e1@Abs{} e2@Abs{} = betweenParens (pprExpr pdoc e1) pdoc
  `mappend` addSpace (betweenParens (pprExpr pdoc e2) pdoc)
pprApp pdoc e1 e2@App{} = pprExpr pdoc e1
  `mappend` addSpace (betweenParens (pprExpr pdoc e2) pdoc)
pprApp pdoc e1 e2@Abs{} = pprExpr pdoc e1
  `mappend` addSpace (betweenParens (pprExpr pdoc e2) pdoc)
pprApp pdoc e1@Abs{} e2 = betweenParens (pprExpr pdoc e1) pdoc
  `mappend` addSpace (pprExpr pdoc e2)
pprApp pdoc e1 e2
  = pprExpr pdoc e1 `mappend` addSpace (pprExpr pdoc e2)

pprTApp :: (PrettyPrint n, PrettyPrint t)
        => PDoc String
        -> SystemFExpr n t
        -> Ty t
        -> PDoc String
pprTApp pdoc expr ty = expr' `mappend` addSpace (between ty' "[" "]" empty)
  where expr' = pprExpr pdoc expr
        ty' = add (prettyPrint ty) empty

-- Pretty print an abstraction
pprAbs :: (PrettyPrint n, PrettyPrint t)
       => PDoc String
       -> n
       -> Ty t
       -> SystemFExpr n t
       -> PDoc String
pprAbs pdoc name ty body = between vars' lambda' ". " (pprExpr pdoc body')
  where (vars, body') = uncurryAbs name ty body
        vars' = intercalate (map (uncurry pprArg) vars) " " empty
        pprArg n t = prettyPrint n ++ ":" ++ prettyPrint t
        lambda' = [lambda, space]

-- Pretty print types
pprTy :: PrettyPrint n
      => PDoc String
      -> Ty n
      -> PDoc String
pprTy pdoc (TyVar n) = prettyPrint n `add` pdoc
pprTy pdoc (TyArrow a b) = pprTyArrow pdoc a b

pprTyArrow :: PrettyPrint n
           => PDoc String
           -> Ty n
           -> Ty n
           -> PDoc String
pprTyArrow pdoc a@(TyVar _) b = pprTyArrow' pdoc a b
pprTyArrow pdoc a@(TyArrow _ _) b = pprTyArrow' pdoc a' b
  where a' = betweenParens (pprTy empty a) empty

pprTyArrow' pdoc a b = between arrow (prettyPrint a) (prettyPrint b) pdoc
  where arrow = " -> " `add` empty

-- Pretty print a type abstraction
pprTAbs :: (PrettyPrint n, PrettyPrint t)
        => PDoc String
        -> Ty t
        -> SystemFExpr n t
        -> PDoc String
pprTAbs pdoc ty body = between vars' lambda' ". " (pprExpr pdoc body')
  where (vars, body') = uncurryTAbs ty body
        vars' = intercalate (map prettyPrint vars) " " empty
        lambda' = [upperLambda, space]

uncurryAbs :: n -> Ty t -> SystemFExpr n t -> ([(n, Ty t)], SystemFExpr n t)
uncurryAbs name ty = uncurry' [(name, ty)] 
  where uncurry' ns (Abs n' t' body') = uncurry' ((n', t'):ns) body'
        uncurry' ns body'             = (reverse ns, body')

uncurryTAbs :: Ty t -> SystemFExpr n t -> ([Ty t], SystemFExpr n t)
uncurryTAbs ty = uncurry' [ty]
  where uncurry' ts (TyAbs t' body') = uncurry' (t':ts) body'
        uncurry' ts body'            = (reverse ts, body')
