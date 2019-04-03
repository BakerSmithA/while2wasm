
-- Investigation of methods used to map between representations of the AST.
-- In this method, the Datatypes a la Carte methods are used to create instances
-- of typeclasses which describe how to fold over the AST to create the new AST.
--
-- An advantage of this method is it is easy to add new mappings from syntax
-- by creating a new typeclass instance.
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
import Helper.Eff.Exception

-- To allow two different instance of Rename in the handler, use newtypes.
-- This allows procedures to be renamed separately from variables.
newtype VarName  = VarName Ident
newtype ProcName = ProcName Ident

type Op                = Throw :+: Rename VarName    :+: Rename ProcName :+: Void
type Sc                = Catch :+: LocalName VarName :+: Rename ProcName :+: Void
type RenameHandler f g = Prog Op Sc (Prog f g ())
type Carrier       f g = CarrierId (RenameHandler f g)

instance VarExp FreshName :<: f => OpAlg (VarExp Ident) (Carrier f g) where
    alg (GetVar v) = Id $ do
        v' <- name (VarName v)
        return (getVar v')
