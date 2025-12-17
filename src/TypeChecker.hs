module TypeChecker where

import Syntax

import Pretty

import Control.Monad.Except
    ( MonadError(..), ExceptT, runExceptT )
import Control.Monad.Reader
    ( MonadReader(local), asks, ReaderT(runReaderT) )
import Unbound.Generics.LocallyNameless as Unbound
import Unbound.Generics.LocallyNameless.Fresh

-- Define the type checking Monad which includes a reader 
-- (to read the environment)
-- Exceptions, for error reporting
-- and IO to allow the printing of warning messages
-- and to allow for debugging
type TcMonad = Unbound.FreshMT (ReaderT Context (ExceptT Err IO))

-- Errors are just strings
-- Change this later (also add source locations)
data Err = Err String -- deriving (Show)

-- Entry point for the type checking monad.
-- Given a context, and a typechecker
-- run the typechecker in the context.
-- the result is either an error or the result of the typechecking computation.

runTcMonad :: Context -> TcMonad a -> IO (Either Err a)
runTcMonad con m = runExceptT $ runReaderT (Unbound.runFreshMT m) con 

-- err :: (MonadError Err m, MonadReader Context m) => String -> m a
err msg = throwError $ Err msg

-- lookupTy :: (MonadError Err m, MonadReader Context m) => VarInfo -> m Info
lookupTy :: VarInfo -> TcMonad Info
lookupTy var = do 
					con <- asks ctx
					case lookup var con of 
						Just ty1 -> return ty1
						Nothing  -> error ("Nothing in context for " ++ (printVar var) ++ " in context " ++ (printContext (Context con)))

extendCtx :: (MonadReader Context m) => (VarInfo, Info) -> m a -> m a
extendCtx entry = local (\m@Context{ctx = cs} -> m {ctx = entry : cs}) 


-- kindCheck :: Type -> Kind -> TcMonad TypeDerivation
-- kindCheck :: (MonadError Err m, MonadReader Context m) => Type -> Kind -> m TypeDerivation
kindCheck :: Type -> Kind -> TcMonad TypeDerivation
--  :: (MonadError Err m, MonadReader Context m) =>
--     Type -> Kind -> m TyPrecondition
kindCheck (TypeVar v) Star = do 
							ty <- lookupTy (TypeV v)
							case ty of
								HasKind Star -> do
													con <- asks ctx
													return (MkTyDerivation [] (MkTyConclusion (Context con) (TypeVar v) Star))
								k -> err ("Type :" ++ (printType (TypeVar v)) ++ " does not have kind * instead has " ++ (printInfo k))

kindCheck (Fun t1 t2) Star = do 
								p1 <- kindCheck t1 Star 
								p2 <- kindCheck t2 Star
								con <- asks ctx
								return (MkTyDerivation [p1,p2] (MkTyConclusion (Context con) (Fun t1 t2) Star))




typeCheckM :: Module -> TcMonad [ (String, Derivation) ]
typeCheckM (Module name decls) = typeCheckDecls decls 


typeCheckDecls :: [ Decl ] -> TcMonad [ (String, Derivation) ]
typeCheckDecls [] = return [] 
typeCheckDecls (Assume v t : decs ) = extendCtx (TermV v, HasType t) (typeCheckDecls decs)	
typeCheckDecls (Type v k : decs) = extendCtx (TypeV v, HasKind k) (typeCheckDecls decs)
typeCheckDecls (Decl n t : ds) = do
									-- kD <- kindCheck t Star
									p@(MkDerivation pre (MkConclusion con' t' (MkTyDerivation _ (MkTyConclusion _ ty ki)))) <- inferType t 
									ps <- extendCtx (TermV (varN n), HasType ty) (typeCheckDecls ds)
									return ((n,p):ps)

getType :: Derivation -> Type
getType (MkDerivation pre (MkConclusion con' t' (MkTyDerivation _ (MkTyConclusion _ ty ki)))) = ty 

getTypeDerivation :: Derivation -> TypeDerivation
getTypeDerivation (MkDerivation pre (MkConclusion con' t' a@(MkTyDerivation _ (MkTyConclusion _ ty ki)))) = a


typeCheck :: Term -> Type -> TcMonad Derivation
typeCheck a@(Abs bnd) (Fun ty1 ty2) = do 
		kD <- kindCheck (Fun ty1 ty2) Star
		(x, body) <- unbind bnd 
		p <- extendCtx (TermV x, HasType ty1) (typeCheck body ty2)
		con <- asks ctx
		if getType p == ty2 then return $  MkDerivation [p] (MkConclusion (Context con) a kD)
			 	     	    else err ("Type  in abstraction " ++ (printTerm a) ++ " doesn't match expected type " ++ (printType ty2))
{-
typeCheck a@(Abs (v:vars) t) (Fun ty1 ty2) = do
		-- extend the context to contain the new lambda expression
		let (Abs [v2] t2) = transformAbs a
		kD <- kindCheck (Fun ty1 ty2) Star
		p <- extendCtx (TermV v2, HasType ty1) (typeCheck t2 ty2)
		con <- ask
		if getType p == ty2 then return $  MkDerivation [p] (MkConclusion con a kD)
			 	     else err ("Type  in abstraction " ++ (printTerm a) ++ " doesn't match expected type " ++ (printType ty2) ++ " : " ++ (printType (getType p))) -- ++ (printType ty2)
-}					
typeCheck t ty = do
					p <- inferType t 
				 	if (ty == getType p) then return p 
				 			       else err ("Type Mismatch in Term " ++ (printTerm t) ++ " with type " ++ (printType ty))

inferType :: Term -> TcMonad Derivation
inferType (Ann t ty) = do 
							kD <- kindCheck ty Star
							tD <- typeCheck t ty
							con <- asks ctx
							return $ MkDerivation [tD] (MkConclusion (Context con) t kD)

inferType (VarT v) = do 
						HasType ty <- lookupTy (TermV v) 
						kD <- kindCheck ty Star
						con <- asks ctx
						return $ MkDerivation [] (MkConclusion (Context con) (VarT v) kD)
inferType a@(App t1 t2) = do 
	p1@(MkDerivation pre (MkConclusion con' t1 (MkTyDerivation _ (MkTyConclusion _ (Fun ty1 ty2) ki)))) <- inferType t1 
	p2 <- typeCheck t2 ty1 
	con <- asks ctx
	if ty1 == getType p2 then return $ MkDerivation [p1,p2] (MkConclusion (Context con) a (getTypeDerivation p2))
				         else err "Application type error!"

inferType t = do 
				err ("unable to infer type for term " ++ (printTerm t))


