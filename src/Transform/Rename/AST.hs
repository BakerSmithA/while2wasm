
-- Supplies AST nodes needed to describe AST after renaming.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}

module Transform.Rename.AST where

import Data.Word (Word)
import Front.AST
import Helper.Co

type Fresh = Word

-- Smart constructors
-- Constructs syntax where variables are represented as numbers.

getFVar :: VarExp Fresh :<: f => Fresh -> Prog f g a
getFVar v = inject (GetVar v)

setFVar :: VarStm Fresh :<: f => Fresh -> Prog f g () -> Prog f g ()
setFVar v x = inject (SetVar v x (Var ()))
