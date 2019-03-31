
-- Investigation of methods used to map between representations of the AST.
-- In this method, the Datatypes a la Carte methods are used to create instances
-- of typeclasses which describe how to fold over the AST to create the new AST.
--
-- An advantage of this method is the ease of specifying what the new and old
-- AST contains, i.e. using (:<:).
--
-- A disadvantage is the mapping needs to be specified for every datatype
-- that will be mapped over into the tree. Even if no renaming occurs, such as
-- is the case for AExp.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Transform.Rename.AST where

import Front.AST
import Transform.Rename.RenameEff
import Helper.Alg
import Helper.Co
import Helper.Eff.Void

type Handler f g = CarrierId (Prog (Rename :+: Void) (Local :+: Void) (Prog f g ()))

instance VarExp Fresh :<: f => OpAlg (VarExp Ident) (Handler f g) where
    alg (GetVar v) = Id $ do
        v' <- varName v
        return (getVar v')

instance (VarStm Fresh :<: f, Functor g) => OpAlg (VarStm Ident) (Handler f g) where
    alg (SetVar v (Id x) (Id k)) = Id $ do
        v' <- varName v
        x' <- x
        k' <- k
        return (do setVar v' x'; k')
