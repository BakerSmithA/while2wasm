
-- Datatypes a la Carte methods are used to create instances of typeclasses
-- which describe how to fold over the AST to create the new AST.
--
-- An advantage of this method is it is easy to add new mappings from syntax
-- by creating a new typeclass instance. Also, the domain being mapped to will
-- not change, meaning there are no problems with needing to implement these
-- typeclasses for slightly different domains.
--
-- A disadvantage is the mapping needs to be specified for every datatype
-- that will be mapped over into the tree. Even if no renaming occurs, such as
-- is the case for AExp.

{-# LANGUAGE TypeOperators, DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Transform.Rename.AST where

import Control.Monad (liftM2)
import Front.AST
import Transform.Rename.RenameEff
import Helper.Alg
import Helper.Co
import Helper.Eff.Void
import Helper.Eff.Exception
import Helper.ProgT as T hiding (Nat(..))

-- TODO: Remove
import Front.Pretty

type Op  = Rename    Ident :+: Void
type Sc  = LocalName Ident :+: Void
type Ctx = T.ProgT Op Sc

-- The renaming handler Ctx, should be a global scope around the While AST.
-- Therefore making renaming consistent across different scopes. Therefore,
-- Ctx wraps the AST.
data Carrier f g a n
    = Rn { runRn :: Ctx (Prog f g) (Carrier' f g a n) }

-- Also, because the renaming handler should be global, it is not given to
-- CS when increasing the level of nesting. This ensures there are not renaming
-- handlers nested inside other renaming handlers, and instead CZ and CS refer
-- only to the levels of nesting of the AST.
data Carrier' f g a :: Nat -> * where
    CZ :: a -> Carrier' f g a 'Z
    CS :: Ctx (Prog f g) (Carrier' f g a n) -> Carrier' f g a ('S n)

instance (Functor f, Functor g, BlockStm FreshName Ident :<: g)
    => ScopeAlg (BlockStm Ident Ident) (Carrier f g a) where

    dem (Block vs ps (Rn body)) = Rn $ do
        CS k <- body
        undefined

    -- dem (Block vs ps (Rn body)) = Rn $ do
    --     rnBody <- body
    --     return (do
    --         CS k <- rnBody
    --         _)

    -- dem (Block vs ps (Rn body)) = Rn (do
    --     -- TODO
    --     -- Continuation needs to occur outside `localNames` so names in
    --     -- continuation are not given incorrect names.
    --
    --     (vs', ps', b') <- localNames (fsts vs) (do
    --         vs' <- mapM fv vs
    --         ps' <- mapM fp ps
    --         body' <- body
    --         return (vs', ps', body'))
    --
    --     return (do (CS k) <- block vs' ps' b'; k))
