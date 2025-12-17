module Eval where

import Syntax 

{-

data Term = 
                Ann Term Type 
            |   VarT Var 
            |   App Term Term 
            |   Abs [ Var ] Term 
            deriving (Show, Eq)

-}

type Env = [ (Var, Term) ]

-- convert all abstractions of the form
-- \a b c . e 
-- to 
-- \a . \b . \c. e 
-- everywhere

deSugar :: Term -> Term 
deSugar (Ann t ty) = Ann (deSugar t) ty
deSugar (VarT v)   = VarT v 
deSugar (App t1 t2) = App (deSugar t1) (deSugar t2)
deSugar (Abs [v] t)  = Abs [v] (deSugar t)
deSugar (Abs (v:vs) t) = Abs [v] (deSugar (Abs vs t))

evalTerm :: Env -> Term -> Term 
evalTerm en (Ann t ty)    = undefined 
evalTerm en (VarT v)      = undefined
evalTerm en (App (Abs [v]  t) t2)   = undefined
evalTerm en (App (Abs [v1] t) (Abs [v2] t2))   = undefined
evalTerm en (App t1 t2)   = App (evalTerm en t1) t2

