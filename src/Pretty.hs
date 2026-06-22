module Pretty where

import Data.Tree
import Text.PrettyPrint.Boxes
import GHC.Conc (childHandler)
import Unbound.Generics.LocallyNameless qualified as Unbound
import Text.PrettyPrint (Doc)
import qualified Text.PrettyPrint as PP
import Syntax

{-
instance Unbound.LFresh ((->) DispInfo) where
  lfresh nm = do
    let s = Unbound.name2String nm
    di <- ask
    return $
      head
        ( filter
            (\x -> Unbound.AnyName x `S.notMember` dispAvoid di)
            (map (Unbound.makeName s) [0 ..])
        )
  getAvoids = asks dispAvoid
  avoid names = local upd
    where
      upd di =
        di
          { dispAvoid =
              S.fromList names `S.union` dispAvoid di
          }
-}

instance Unbound.LFresh Tree 

-- printBinder :: Unbound.LFresh m =>
--                 Unbound.Bind (Unbound.Name Term) Term -> m (String, String)
printBinder b = 
    Unbound.lunbind b $ \(n, body) -> do 
        body' <- printTerm body
        return (varString n, body')
        

printDerivationDef :: String -> [ (String, Derivation) ] -> Box 
printDerivationDef d [] = error "Cannot find definition in context to print!"
printDerivationDef d ((n,de):ds) | d == n     = pp (derivationTree de)
                        		 | otherwise  = printDerivationDef d ds

-- printTypeDef :: Unbound.LFresh m => String -> [ (String, Derivation) ] -> m String 
printTypeDef d [] = return "Cannot find definition in context to print!"
printTypeDef d ((n,de):ds) | d == n = do 
                                        tc <- printTypeConclusion de  
                                        return $ n ++ " = " ++ tc ++ "\n"
                        | otherwise  = printTypeDef d ds

-- printDerivations :: Unbound.LFresh m => [ (String, Derivation) ] -> m String 
printDerivations [] = return ""
printDerivations ((n,d):ds) = 
    do 
      tc <- printTypeConclusion d
      ds' <- printDerivations ds
      return $ n ++ " = " ++ tc ++ "\n" ++ ds'

-- printTerm :: Unbound.LFresh m => Term -> m String 
printTerm (VarT x) = return $ varString x 
printTerm (App t1 t@(App t2 t3)) = do 
    t1' <- printTerm t1
    t2' <- printTerm t2
    t' <- printTerm t 
    return $ t1' ++ " (" ++ t' ++ ")"
printTerm (App t1 t2) = do
    t1' <- printTerm t1 
    t2' <- printTerm t2 
    return $ t1' ++ " " ++ t2'
printTerm (Abs bnd) = do 
      (n, body) <- printBinder bnd 
      return $ "(\\" ++ n ++ ". " ++ body ++ ")"
printTerm (Ann t ty) = do
    t' <- printTerm t 
    return $ t' ++ " : " ++ printType ty

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

-- printConclusion :: Unbound.LFresh m => Conclusion -> m String 
printConclusion (MkConclusion con te ty) =
    do
    te' <- printTerm te    
    return $ "{" ++ printContext con ++ "} |- " ++ te' ++ " : " ++ printTypeDerivation ty

derivationTree :: Derivation -> Tree String 
derivationTree (MkDerivation child con) = 
    do 
      con' <- printConclusion con
      Node con' (mkChildren child)
    where mkChildren [] = [] 
          mkChildren (p:ps) = derivationTree p : mkChildren ps

-- printTypeConclusion :: Unbound.LFresh m => Derivation -> m String 
printTypeConclusion (MkDerivation child (MkConclusion con te ty)) =
    do
      t <- printTerm te
      
      return $ t ++ " : " ++ printTypeDerivation ty

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
