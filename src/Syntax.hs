{-# LANGUAGE DeriveDataTypeable, DeriveGeneric, MultiParamTypeClasses #-}
module Syntax where
import Unbound.Generics.LocallyNameless
import GHC.Generics (Generic)
import Data.Typeable (Typeable)

type Var = Name Term

data Term = 
                Ann Term Type 
            |   VarT Var 
            |   App Term Term 
            |   Abs (Bind (Name Term) Term) 
            deriving (Show, Generic, Typeable)

data Context = Context {
                         ctx :: [ (VarInfo, Info) ]
                        }
                    deriving (Show)

data VarInfo = TypeV String
             | TermV String
        deriving (Generic, Show, Eq)

data Info = HasType Type
          | HasKind Kind 
        deriving (Show)

data Type = 
              Fun Type Type 
            | TypeVar Var
        deriving (Show, Generic, Typeable, Eq) 

data Kind = Star 
        deriving (Show)

-- a Decl has form
-- name : Type 
-- name = Term 
data Decl = Decl    String Term
          | Assume  Var Type
          | Type Var Kind
    deriving (Show, Generic, Typeable)



data Module = Module String [ Decl ]
    deriving (Show)


type Precondition = Derivation 

data Conclusion = MkConclusion Context Term TypeDerivation
    deriving (Show)


data Derivation = 
        MkDerivation [ Precondition ] Conclusion
    deriving (Show)

type TyPrecondition = TypeDerivation

data TyConclusion = MkTyConclusion Context Type Kind 
    deriving (Show)

data TypeDerivation = 
        MkTyDerivation [ TyPrecondition ] TyConclusion
    deriving (Show)

-- Derives alpha equivalence
instance Alpha Type

instance Alpha Term

-- capture avoiding substitution
instance Subst Term Term where 
    isvar (VarT x) = Just (SubstName x)
    isvar _ = Nothing 

-- we cannot substitute in types 
instance Subst Term Type where 
    isvar (TypeVar x) = Nothing 
    isvar _ = Nothing

-- helper functions
lam :: String -> Term -> Term 
lam x t = Abs $ bind (string2Name x) t

var :: String -> Term 
var = VarT . string2Name

-- varN :: String -> Term 
varN = string2Name

varString :: Name Term -> String 
varString x = name2String x