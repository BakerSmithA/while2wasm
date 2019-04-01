
-- Investigation of methods used to map between representations of the AST.
-- This method uses extensible effects methods to

{-# LANGUAGE TypeOperators, DataKinds #-}

module Trans.Rename.AST_02 where

import Front.AST
import Transform.Rename.RenameEff
import Helper.Prog
import Helper.Eff.Void
import Helper.Co

type RenameHandler f g = Prog (Rename :+: Void) (Local :+: Void) (Prog f g ())
type Carrier       f g = CarrierId (RenameHandler f g)

-- Types appended onto f and g will be removed, as these are types which
-- contain variables represented using strings.
type RN  f = VarExp Ident :+: VarStm Ident :+: ProcStm Ident :+: f
type SRN g = BlockStm Ident Ident :+: g

genMk :: () -> Carrier h i 'Z
genMk = undefined

algMk :: Alg (RN f) (SRN g) (Carrier h i)
algMk = undefined

handleMake :: (Functor f, Functor g) => Prog (RN f) (SRN g) () -> RenameHandler h i
handleMake prog = case run genMk algMk prog of
    (Id hdl) -> hdl
