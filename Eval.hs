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

evalTerm : Env -> Term -> Term 
evalTerm en (Ann t ty)    = undefined 
evalTerm en (VarT v)      = undefined
evalTerm en (App (Abs vs t) t2)   = undefined
evalTerm en (App (Abs vs t) (Abs (vs2 t2)))   = undefined
evalTerm en (App t1 t2)   = App (eval t1) t2

evalTerm en (Abs [] t1)   = evalTerm en t1
evalTerm en a@(Abs [v] t1)  = a
evalTerm en a@(Abs (v:vs) t1) = a