module Syntax where

import Data.Tree
import Text.PrettyPrint.Boxes
import GHC.Conc (childHandler)


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

printContext :: Context -> String 
printContext [] = ""
printContext [(Var x, ty)] = x ++ " : " ++ printType ty
printContext ((Var x,ty):r) = x ++ " : " ++ printType ty ++ " ,"  ++ printContext r


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

printConclusion :: Conclusion -> String 
printConclusion (MkConclusion con te ty) = "{" ++ printContext con ++ "} |- " ++ printTerm te ++ " : " ++ printType ty

data Derivation = 
        MkDerivation [ Precondition ] Conclusion
    deriving (Show, Eq)


pp :: Tree String -> Box
pp (Node here []      ) = text here
pp (Node here children) = vcat center1 [premises, separator, conclusion]
    where
    premises   = hsep 4 bottom (map pp children)
    conclusion = text here
    width      = max (cols premises) (cols conclusion)
    separator  = text (replicate width '-')
{-
sampleTree :: Tree String
sampleTree = Node "<z:=x; x:=y; y:=z, s> -> s'''"
    [Node "<z:=x; x:=y,s> -> s''"
        [Node "<z:=x, s> -> s'" []
        ,Node "<x:=,s1> -> s''" []
        ]
    ,Node "<y:=z, s''> -> s'''" []
    ]
-}

derivationTree :: Derivation -> Tree String 
derivationTree (MkDerivation child con) = Node (printConclusion con) (mkChildren child)
    where mkChildren [] = [] 
          mkChildren (p:ps) = derivationTree p : mkChildren ps

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
