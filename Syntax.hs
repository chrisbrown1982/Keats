module Syntax where


data Var = Var String 
    deriving (Show, Eq)

data Term = 
                Ann Term Type 
            |   VarT Var 
            |   App Term Term 
            |   Abs [ Var ] Term 
            deriving (Show, Eq)
            
type Context = [ (VarInfo, Info) ]


data VarInfo = TypeV Var
             | TermV Var
        deriving (Show, Eq)

data Info = HasType Type
          | HasKind Kind 
        deriving (Show, Eq)

data Type = 
              Fun Type Type 
            | TypeVar Var
        deriving (Show, Eq) 

data Kind = Star 
        deriving (Show, Eq)

-- a Decl has form
-- name : Type 
-- name = Term 
data Decl = Decl    String Term
          | Assume  Var Type
          | Type Var Kind
    deriving (Show, Eq)

data Module = Module String [ Decl ]
    deriving (Show, Eq)


type Precondition = Derivation 

data Conclusion = MkConclusion Context Term TypeDerivation
    deriving (Show, Eq)


data Derivation = 
        MkDerivation [ Precondition ] Conclusion
    deriving (Show, Eq)

type TyPrecondition = TypeDerivation

data TyConclusion = MkTyConclusion Context Type Kind 
    deriving (Show, Eq)

data TypeDerivation = 
        MkTyDerivation [ TyPrecondition ] TyConclusion
    deriving (Show, Eq)

{-
ex1 :: Term 
ex1 = App (Abs (Var "X") (VarT (Var "X")) (Fun (Fun TypeVar TypeVar) (Fun TypeVar TypeVar)))  (Abs (Var "Y") (VarT (Var "Y")) (Fun TypeVar TypeVar)) 
-}