module TypeChecker where

import Syntax
import Pretty

import Control.Monad.Reader
import Control.Monad.Except


-- Define the type checking Monad which includes a reader 
-- (to read the environment)
-- Exceptions, for error reporting
-- and IO to allow the printing of warning messages
-- and to allow for debugging
type TcMonad = ReaderT Context (ExceptT Err IO)

-- Errors are just strings
-- Change this later (also add source locations)
data Err = Err String deriving (Show)

-- Entry point for the type checking monad.
-- Given a context, and a typechecker
-- run the typechecker in the context.
-- the result is either an error or the result of the typechecking computation.

runTcMonad :: Context -> TcMonad a -> IO (Either Err a)
runTcMonad con m = runExceptT $ runReaderT m con 

err :: (MonadError Err m, MonadReader Context m) => String -> m b
err msg = throwError $ Err msg

lookupTy :: (Monad m, MonadError Err m, MonadReader Context m) => Var -> m Type
lookupTy var = do 
					con <- ask
					case lookup var con of 
						Just ty1 -> return ty1
						Nothing  -> err "Nothing in context!"

extendCtx entry m = local (\con -> [entry]++con) m 

inferType :: Term -> TcMonad Derivation
inferType (VarT v) = do 
						ty <- lookupTy v 
						con <- ask
						return $ MkDerivation [] (MkConclusion con (VarT v) ty)
inferType a@(Abs v t (Fun ty1 ty2)) = do
		-- extend the context to contain the new lambda expression
		p@(MkDerivation pre (MkConclusion con' t' ty)) <- extendCtx (v, ty1) (inferType t)
		con <- ask
		if ty == ty2 then return $  MkDerivation [p] (MkConclusion con a (Fun ty1 ty2))
			 	     else err $ "Type " ++ (printType ty) ++ " in abstraction doesn't match expected type " ++ (printType ty2)

inferType a@(App t1 t2) = do 
	p1@(MkDerivation pre (MkConclusion con' t1 (Fun ty1 ty2))) <- inferType t1 
	p2@(MkDerivation pre2 (MkConclusion con t2 ty1')) <- inferType t2 
	con <- ask 
	if ty1 == ty1' then return $ MkDerivation [p1,p2] (MkConclusion con a (Fun ty1 ty2)) 
				   else err "Application type error!"

