module Pretty where

import Data.Tree
import Text.PrettyPrint.Boxes
import GHC.Conc (childHandler)

import Syntax

printTerm :: Term -> String 
printTerm (VarT (Var x)) = x 
printTerm (App t1 t2) = printTerm t1 ++ " " ++ printTerm t2
printTerm (Abs (Var x) t1 ty) = "(\\" ++ x ++ ". " ++ printTerm t1 ++ ")"


printContext :: Context -> String 
printContext [] = ""
printContext [(Var x, ty)] = x ++ " : " ++ printType ty
printContext ((Var x,ty):r) = x ++ " : " ++ printType ty ++ " ,"  ++ printContext r

printType :: Type -> String 
printType (Fun ty1 ty2) = "(" ++ printType ty1 ++ " -> " ++ printType ty2 ++ ")"
printType (TypeVar) = "*"

printConclusion :: Conclusion -> String 
printConclusion (MkConclusion con te ty) = "{" ++ printContext con ++ "} |- " ++ printTerm te ++ " : " ++ printType ty

derivationTree :: Derivation -> Tree String 
derivationTree (MkDerivation child con) = Node (printConclusion con) (mkChildren child)
    where mkChildren [] = [] 
          mkChildren (p:ps) = derivationTree p : mkChildren ps

printTypeConclusion :: Derivation -> String 
printTypeConclusion (MkDerivation child (MkConclusion con te ty)) = printTerm te ++ " : " ++ printType ty

{-
pp :: Tree String -> Box
pp (Node here []      ) = text here
pp (Node here children) = vcat center1 [premises, separator, conclusion]
    where
    premises   = hsep 4 bottom (map pp children)
    conclusion = text here
    width      = max (cols premises) (cols conclusion)
    separator  = text (replicate width '-')

sampleTree :: Tree String
sampleTree = Node "<z:=x; x:=y; y:=z, s> -> s'''"
    [Node "<z:=x; x:=y,s> -> s''"
        [Node "<z:=x, s> -> s'" []
        ,Node "<x:=,s1> -> s''" []
        ]
    ,Node "<y:=z, s''> -> s'''" []
    ]
-}