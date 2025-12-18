module Eval where

import Syntax 

import Control.Monad.Except
    ( MonadError(..), ExceptT, runExceptT )
import Unbound.Generics.LocallyNameless as Unbound

type Eval = Unbound.FreshMT (ExceptT Err IO)

runEvalMonad :: Eval a -> IO (Either Err a)
runEvalMonad m = runExceptT $ Unbound.runFreshMT m

getTermFromDec :: Var -> [Decl] -> Term 
getTermFromDec v [] = error $ (show v) ++ "not found!"
getTermFromDec v (Decl n t:ds) | varString v == n = t 
                            | otherwise  = getTermFromDec v ds 
getTermFromDec v (Assume v2 t:ds) | v == v2 = VarT v
getTermFromDec v (_:ds) = getTermFromDec v ds

evalTerm :: [Decl] -> Term -> Eval Term 
evalTerm en (Ann t ty)    = evalTerm en t 
evalTerm en (VarT v)      = evalTerm en (getTermFromDec v en)
evalTerm en e@(Abs bnd)   = return e
evalTerm en (App t1 t2)   = do 
    t1' <- evalTerm en t1 
    t2' <- evalTerm en t2 
    case t1' of 
        Abs bnd -> do 
            (x, body) <- unbind bnd 
            let body' = subst x t2' body 
            evalTerm en body' 
        _ -> error "application of non-lambda"

