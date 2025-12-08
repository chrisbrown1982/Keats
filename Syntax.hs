module Syntax where


data Var = Var String 
    deriving (Show, Eq)

data Term = 
                VarT Var 
            |   App Term Term 
            |   Abs Var Term Type 
            deriving (Show, Eq)
            
type Context = [ (Var, Type) ]

data Type = 
              Fun Type Type 
            | TypeVar 
        deriving (Show, Eq) 


type Precondition = Derivation 

data Conclusion = MkConclusion Context Term Type 
    deriving (Show, Eq)


data Derivation = 
        MkDerivation [ Precondition ] Conclusion
    deriving (Show, Eq)


ex1 :: Term 
ex1 = App (Abs (Var "X") (VarT (Var "X")) (Fun (Fun TypeVar TypeVar) (Fun TypeVar TypeVar)))  (Abs (Var "Y") (VarT (Var "Y")) (Fun TypeVar TypeVar)) 
