
-- Using Datatypes a la Carte methods to describe how to rename an AST.
-- Converts variables in AST to be represented by Words instead of Strings.
-- Done by converting into an AST in a renaming effect handler context.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Transform.Rename.AST where

import Front.AST
import Transform.Rename.RenameEff
import Helper.Alg
import Helper.Co
import Helper.Eff.Void

type Handler f g a = CarrierId (Prog (Rename :+: Void) (Local :+: Void) (Prog f g a))

instance VarExp Fresh :<: f => OpAlg (VarExp Ident) (Handler f g a) where
    alg (GetVar v) = Id $ do
        v' <- varName v
        return (getVar v')
