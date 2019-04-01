
-- Investigation of methods used to map between representations of the AST.
-- This method uses extensible effects methods to

{-# LANGUAGE TypeOperators, DataKinds, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Trans.Rename.AST_02 where

import Front.AST
import Transform.Rename.RenameEff
import Helper.Prog
import Helper.Eff
import Helper.Eff.Void
import Helper.Co

type RenameHandler f g = Prog (Rename :+: Void) (Local :+: Void) (Prog f g ())
type Carrier       f g = CarrierId (RenameHandler f g)

-- Types appended onto f and g will be removed, as these are types which
-- contain variables represented using strings.
type RN  f = VarExp Ident :+: VarStm Ident :+: ProcStm Ident :+: f
type SRN g = BlockStm Ident Ident :+: g

-- Define patterns to make alg more readable.
pattern GetVar' v       <- (prj -> Just (GetVar v))
pattern SetVar' v  x  k <- (prj -> Just (SetVar v x k))
pattern Call'   p  k    <- (prj -> Just (Call p k))
pattern Block'  vs ps b <- (prj -> Just (Block vs ps b))

genMk :: (Functor h, Functor i) => () -> Carrier h i 'Z
genMk x = Id (return (return x))

algMk :: (Functor f, Functor g)
      => (VarExp Fresh :<: h, VarStm Fresh :<: h, ProcStm Fresh :<: h)
      => (BlockStm Ident Ident :<: i)
      => Alg (RN f) (SRN g) (Carrier h i)
algMk = A a d p where
    a (GetVar' v) = Id $ do
        v' <- varName v
        return (getVar v')
    a (Other op) = Id (Op (fmap _ op))

    d :: SRN g (Carrier h i ('S n)) -> Carrier h i n
    d = undefined

    p :: Carrier h i n -> Carrier h i ('S n)
    p = undefined

handleMake :: (Functor f, Functor g, Functor h, Functor i)
           => (VarExp Fresh :<: h, VarStm Fresh :<: h, ProcStm Fresh :<: h)
           => (BlockStm Ident Ident :<: i)
           => Prog (RN f) (SRN g) () -> RenameHandler h i
handleMake prog = case run genMk algMk prog of
    (Id hdl) -> hdl
