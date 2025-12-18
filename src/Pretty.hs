module Pretty where

import Data.Tree
import Text.PrettyPrint.Boxes
import GHC.Conc (childHandler)
import Unbound.Generics.LocallyNameless qualified as Unbound

import Syntax

-- printBinder :: Term -> (String, String)
printBinder b = 
    Unbound.lunbind b $ \(n, body) -> do 
        return (varString n, printTerm body)
        

printDerivationDef :: String -> [ (String, Derivation) ] -> Box 
printDerivationDef d [] = error "Cannot find definition in context to print!"
printDerivationDef d ((n,de):ds) | d == n     = pp (derivationTree de)
                        		 | otherwise  = printDerivationDef d ds

printTypeDef :: String -> [ (String, Derivation) ] -> String 
printTypeDef d [] = "Cannot find definition in context to print!"
printTypeDef d ((n,de):ds) | d == n     = n ++ " = " ++ printTypeConclusion de ++ "\n"
                        | otherwise  = printTypeDef d ds

printDerivations :: [ (String, Derivation) ] -> String 
printDerivations [] = ""
printDerivations ((n,d):ds) = n ++ " = " ++ printTypeConclusion d ++ "\n" ++ printDerivations ds

printTerm :: Term -> String 
printTerm (VarT x) = varString x 
printTerm (App t1 t@(App t2 t3)) = printTerm t1 ++ " (" ++ printTerm t ++ ")"
printTerm (App t1 t2) = printTerm t1 ++ " " ++ printTerm t2
printTerm (Abs bnd) = "ABS HERE" -- let (n, body) = printBinder bnd in "(\\" ++ n ++ ". " ++ body ++ ")"
printTerm (Ann t ty) = printTerm t ++ " : " ++ printType ty

printVar :: VarInfo -> String 
printVar (TypeV x) =  tvarString x
printVar (TermV x) =  varString x

printContext :: Context -> String 
printContext (Context []) = ""
printContext (Context [(x,ty)]) = printVar x ++ " : " ++ printInfo ty
printContext (Context ((x,ty):r)) = printVar x ++ " : " ++ printInfo ty ++ " ,"  ++ printContext (Context r)


printKind :: Kind -> String 
printKind Star = "*"

printInfo :: Info -> String 
printInfo (HasType ty) = printType ty
printInfo (HasKind k)  = printKind k 

printType :: Type -> String 
printType (Fun ty1 ty2) = "(" ++ printType ty1 ++ " -> " ++ printType ty2 ++ ")"
printType (TypeVar x) = tvarString x

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
