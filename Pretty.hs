module Pretty where

import Data.Tree
import Text.PrettyPrint.Boxes
import GHC.Conc (childHandler)

import Syntax


printDerivations :: [ Derivation ] -> String 
printDerivations [] = ""
printDerivations (d:ds) = printTypeConclusion d ++ "\n" ++ printDerivations ds

printTerm :: Term -> String 
printTerm (VarT (Var x)) = x 
printTerm (App t1 t2) = printTerm t1 ++ " " ++ printTerm t2
printTerm (Abs [] t1) = printTerm t1
printTerm (Abs (Var v:vars) t1) = "(\\" ++ v ++ ". " ++ printTerm (Abs vars t1) ++ ")"
printTerm (Ann t ty) = printTerm t ++ " : " ++ printType ty

printVar :: VarInfo -> String 
printVar (TypeV (Var v)) = v
printVar (TermV (Var v)) = v

printContext :: Context -> String 
printContext [] = ""
printContext [(x,ty)] = printVar x ++ " : " ++ printInfo ty
printContext ((x,ty):r) = printVar x ++ " : " ++ printInfo ty ++ " ,"  ++ printContext r


printKind :: Kind -> String 
printKind Star = "*"

printInfo :: Info -> String 
printInfo (HasType ty) = printType ty
printInfo (HasKind k)  = printKind k 

printType :: Type -> String 
printType (Fun ty1 ty2) = "(" ++ printType ty1 ++ " -> " ++ printType ty2 ++ ")"
printType (TypeVar (Var n)) = n

printConclusion :: Conclusion -> String 
printConclusion (MkConclusion con te ty) = "{" ++ printContext con ++ "} |- " ++ printTerm te ++ " : " ++ printTypeDerivation ty

derivationTree :: Derivation -> Tree String 
derivationTree (MkDerivation child con) = Node (printConclusion con) (mkChildren child)
    where mkChildren [] = [] 
          mkChildren (p:ps) = derivationTree p : mkChildren ps

printTypeConclusion :: Derivation -> String 
printTypeConclusion (MkDerivation child (MkConclusion con te ty)) = printTerm te ++ " : " ++ printTypeDerivation ty

printTypeDerivation :: TypeDerivation -> String 
printTypeDerivation (MkTyDerivation child (MkTyConclusion _ ty _) ) = printType ty 


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