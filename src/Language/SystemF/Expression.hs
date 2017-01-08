module Language.SystemF.Expression where

import Data.Monoid

import Language.Lambda.Util.PrettyPrint

data SystemFExpr name ty
  = Var name                                        -- Variable
  | App (SystemFExpr name ty) (SystemFExpr name ty) -- Application
  | Abs name (Ty ty) (SystemFExpr name ty)          -- Abstraction
  | TyAbs ty (SystemFExpr name ty)                  -- Type Abstraction
                                                    -- \X. body

  | TyApp (SystemFExpr name ty) (Ty ty)             -- Type Application
                                                    -- x [X]
  deriving (Eq, Show)

data Ty name
  = TyVar name                  -- Type variable (T)
  | TyArrow (Ty name) (Ty name) -- Type arrow    (T -> U)
  | TyForAll name (Ty name)     -- Universal type (forall T. X)
  deriving (Eq, Show)

-- Pretty printing
instance (PrettyPrint n, PrettyPrint t) => PrettyPrint (SystemFExpr n t) where
  prettyPrint = prettyPrint . pprExpr empty

instance PrettyPrint n => PrettyPrint (Ty n) where
  prettyPrint = prettyPrint . pprTy empty True

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
        lambda' = [lambda, space]

        pprArg n t = prettyPrint n ++ (':':pprArg' t)
        pprArg' t@(TyVar _)     = prettyPrint t
        pprArg' t@(TyArrow _ _) = prettyPrint $ betweenParens (pprTy empty False t) empty

-- Pretty print types
pprTy :: PrettyPrint n
      => PDoc String
      -> Bool -- Add a space between arrows?
      -> Ty n
      -> PDoc String
pprTy pdoc space (TyVar n) = prettyPrint n `add` pdoc
pprTy pdoc space (TyArrow a b) = pprTyArrow pdoc space a b
pprTy pdoc _     (TyForAll n t) =  pprTyForAll pdoc n t

pprTyArrow :: PrettyPrint n
           => PDoc String
           -> Bool -- Add a space between arrows?
           -> Ty n
           -> Ty n
           -> PDoc String
pprTyArrow pdoc space a@(TyVar _) b = pprTyArrow' space (pprTy pdoc space a) 
                                                        (pprTy pdoc space b)
pprTyArrow pdoc space (TyArrow a1 a2) b = pprTyArrow' space a' (pprTy pdoc space b)
  where a' = betweenParens (pprTyArrow pdoc space a1 a2) empty

pprTyArrow' :: Bool -- Add a space between arrows?
            -> PDoc String
            -> PDoc String
            -> PDoc String
pprTyArrow' space a b = a <> arrow <> b
  where arrow | space     = " -> " `add` empty
              | otherwise = "->" `add` empty

pprTyForAll :: PrettyPrint n
            => PDoc String
            -> n
            -> Ty n
            -> PDoc String
pprTyForAll pdoc n t = prefix <> prettyPrint t `add` pdoc
  where prefix = between (prettyPrint n `add` empty) "forall " ". " empty

-- Pretty print a type abstraction
pprTAbs :: (PrettyPrint n, PrettyPrint t)
        => PDoc String
        -> t
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

uncurryTAbs :: t -> SystemFExpr n t -> ([t], SystemFExpr n t)
uncurryTAbs ty = uncurry' [ty]
  where uncurry' ts (TyAbs t' body') = uncurry' (t':ts) body'
        uncurry' ts body'            = (reverse ts, body')
