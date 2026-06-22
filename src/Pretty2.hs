module Pretty2 (Display (..), PP.render, disp) where

import Control.Monad.Reader (MonadReader (ask, local), asks)
import Data.Set qualified as S

import Text.PrettyPrint (Doc, ($$), (<+>))
import qualified Text.PrettyPrint as PP
import Unbound.Generics.LocallyNameless qualified as Unbound
import Unbound.Generics.LocallyNameless.Internal.Fold (toListOf)

import Syntax

disp d = display d initDI

class (Unbound.Alpha t) => Display t where
  -- | Convert a value to a 'Doc'.
  display :: t -> DispInfo -> Doc

-- | The data structure for information about the display
data DispInfo = DI
  { -- | should we show type annotations?
    showAnnots :: Bool,
    -- | names that have been used
    dispAvoid :: S.Set Unbound.AnyName,
    -- | current precedence level
    prec :: Int,
    -- | should we print internally-generated names, or user-friendly versions
    showLongNames :: Bool
  }

initDI :: DispInfo
initDI = DI {showAnnots = False,
                          dispAvoid = S.empty,
                          prec = 0,
                          showLongNames = False
                          }

-- Term Syntax instances ---
instance Display (Unbound.Name Term) where
  display = return . PP.text . Unbound.name2String

instance Display String where
  display = return . PP.text

instance Display Term where 
  display (VarT x) = return $ PP.text $ varString x
  display (App t1 t@(App t2 t3)) = do
       t1' <- display t1 
       t'  <- display t 
       pure $ t1' <+> PP.text "(" <+> t' <+> PP.text ")"
  display (App t1 t2) = do
    t1' <- display t1 
    t2' <- display t2 
    pure $ t1' <+> PP.text " " <+> t2'    
  display (Abs bnd) = do 
    Unbound.lunbind bnd $ \(n, body) -> do 
        n'    <- display n 
        dbody <- display body 
        pure $ PP.text "(\\" <+> n' <+> PP.text ". " <+> dbody <+> PP.text ")"     
  display (Ann t ty) = do 
    t' <- display t 
    ty' <- display ty 
    pure $ t' <+> PP.text " : " <+> ty'

instance Display Type where
  display (Fun ty1 ty2) = do
    ty1' <- display ty1 
    ty2' <- display ty2 
    pure $ PP.text "(" <+> ty1' <+> PP.text " -> " <+> ty2' <+> PP.text ")"
  display (TypeVar x) = return $ PP.text $ tvarString x

instance Display Syntax.Conclusion where 
  display (MkConclusion con te ty) = do
    te' <- display te    
    con' <- display con
    ty' <- display ty
    pure $ PP.text "{" <+> con' <+> PP.text "} |- " <+> te' <+> PP.text " : " <+> ty'

instance Display Derivation where 
    display (MkDerivation child (MkConclusion con te ty)) = do
      t' <- display te
      ty' <- display ty
      pure $ t' <+> PP.text " : " <+>  ty'

instance Display TypeDerivation where 
    display (MkTyDerivation child (MkTyConclusion _ ty _) ) = display ty 

instance Display Context where 
 display (Context []) = pure $ PP.text ""
 display (Context [(x,ty)]) = do 
    x' <- display x
    ty' <- display ty 
    pure $ x' <+> PP.text " : " <+> ty'
 display (Context ((x,ty):r)) = do 
    x' <- display x
    ty' <- display ty 
    con' <- display (Context r)
    pure $ x' <+> PP.text " : " <+> ty' <+> PP.text " ,"  <+> con'

instance Display Info where 
  display (HasType ty) = display ty
  display (HasKind k)  = display k

instance Display Kind where 
    display Star = pure $ PP.text "*"

instance Display VarInfo where 
    display (TypeV x) =  pure $ PP.text $ tvarString x
    display (TermV x) =  pure $ PP.text $ varString x 

instance Display [(String, Derivation)] where 
  display [] = pure $ PP.text ""
  display ((n,d):ds) = 
    do 
      tc <- display d
      ds' <- display ds
      pure $ PP.text n <+> PP.text " = " <+> tc <+> PP.text "\n" <+> ds'
-------------------------------------------------------------------------

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