module Syntax where

data Var = Var String 
    deriving (Show, Eq)

data Term = 
                VarT Var 
            |   App Term Term 
            |   Abs Var Term Type 
            deriving (Show, Eq)


printTerm :: Term -> String 
printTerm (VarT (Var x)) = x 
printTerm (App t1 t2) = printTerm t1 ++ " " ++ printTerm t2
printTerm (Abs (Var x) t1 ty) = "(\\" ++ x ++ ". " ++ printTerm t1 ++ ")"
            
type Context = [ (Var, Type) ]


data Type = 
              Fun Type Type 
            | TypeVar 
        deriving (Show, Eq) 

printType :: Type -> String 
printType (Fun ty1 ty2) = "(" ++ printType ty1 ++ " -> " ++ printType ty2 ++ ")"
printType (TypeVar) = "*"
 

type Precondition = Derivation 

data Conclusion = MkConclusion Context Term Type 
    deriving (Show, Eq)


data Derivation = 
        MkDerivation [ Precondition ] Conclusion
    deriving (Show, Eq)

printDerivation :: Derivation -> String 
printDerivation (MkDerivation pres con) = printPreconditions pres ++ "\n=================================\n" ++ printConclusion con ++ "\n"

printConclusion :: Conclusion -> String 
printConclusion (MkConclusion con term ty) = show con ++ " |- " ++ printTerm term ++ " : " ++ printType ty

printPreconditions :: [ Precondition ] -> [String] 
printPreconditions [] = "" 
printPreconditions (p:ps) = printDerivation p : printPreconditions ps


typeI :: Context -> Term -> Derivation 
typeI con (VarT v) = case lookup v con of 
                                Just ty1 -> MkDerivation [] (MkConclusion con (VarT v) ty1)
                                Nothing -> error ("Variable: " ++ show v ++ " is not available in the context" )
typeI con a@(Abs v t (Fun ty1 ty2)) = 
        case typeI ((v,ty1):con) t of 
             p@(MkDerivation pre (MkConclusion con' t' ty)) -> 
                    if ty == ty2 then MkDerivation [p] (MkConclusion con a (Fun ty1 ty2))
                                 else error ("Abstraction does not have correct type! " ++ (show ty) ++ ", but expecting type: " ++ (show ty2))
typeI con a@(App t1 t2) = 
    case typeI con t1 of 
        p1@(MkDerivation pre (MkConclusion con' t1 (Fun ty1 ty2))) 
                 -> case typeI con t2 of 
                           p2@(MkDerivation pre2 (MkConclusion con t2 ty1')) -> 
                                    if ty1 == ty1' then MkDerivation [p1,p2] (MkConclusion con a (Fun ty1 ty2)) 
                                                   else error ("application does not match function type: " ++ (show ty1'))

ex1 :: Term 
ex1 = App (Abs (Var "X") (VarT (Var "X")) (Fun (Fun TypeVar TypeVar) (Fun TypeVar TypeVar)))  (Abs (Var "Y") (VarT (Var "Y")) (Fun TypeVar TypeVar)) 
