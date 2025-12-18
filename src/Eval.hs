module Eval where

import Syntax 

import Control.Monad.IO.Class

import Control.Monad.Except
    ( MonadError(..), ExceptT, runExceptT )
import Unbound.Generics.LocallyNameless as Unbound

type Eval = Unbound.FreshMT (ExceptT Err IO)

runEvalMonad :: Eval a -> IO (Either Err a)
runEvalMonad m = runExceptT $ Unbound.runFreshMT m

getTermFromDec :: Var -> [Decl] -> Term 
getTermFromDec v [] = error $ (show v) ++ " not found!"
getTermFromDec v (Decl n t:ds) | varString v == n = t 
                               | otherwise  = getTermFromDec v ds 
getTermFromDec v (Assume v2 t:ds) | v == v2 = VarT v
getTermFromDec v (_:ds) = getTermFromDec v ds

evalTerm :: [Decl] -> Term -> Eval Term 
evalTerm en (Ann t ty)    = evalTerm en t 
evalTerm en (VarT v)      = do
    case getTermFromDec v en of 
        VarT v -> return (VarT v)
        t      -> evalTerm en t 
evalTerm en e@(Abs bnd)   = do 
    (x, body) <- unbind bnd 
    body' <- evalTerm (Decl (varString x) (VarT x):en) body
    return (Abs (bind x body'))
evalTerm en e@(App (VarT v) t2) = do
    case getTermFromDec v en of 
        VarT v -> do t2' <- evalTerm en t2 
                     return (App (VarT v) t2')
        t      -> do 
            t1 <- evalTerm en t 
            t2' <- evalTerm en t2
            case t1 of 
                Abs bnd -> do 
                    (x, body) <- unbind bnd 
                    let body' = subst x t2' body 
                    evalTerm en body' 
                x -> return (App x t2')
evalTerm en (App t1 t2)   = do 
    t1' <- evalTerm en t1 
    t2' <- evalTerm en t2 
    case t1' of 
        Abs bnd -> do 
            (x, body) <- unbind bnd 
            let body' = subst x t2' body 
            evalTerm en body' 
        x -> return (App x t2')
