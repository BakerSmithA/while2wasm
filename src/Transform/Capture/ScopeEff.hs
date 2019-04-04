
-- Composite scoped effect handler to annotate variables with whether they are
-- written to across different scopes. This affects whether variables need
-- to be stored as values or pointers in outputted WASM.
--
-- This requires two passes over the AST. In the first pass, a mapping from
-- variable names to Clean (written to in a single scope), or Dirty
-- (written to over multiple scopes). Next, the AST can be updated to annotate
-- variables with their type. This separation is required because all occurrences
-- of a variable must be annotated with the same type.

{-# LANGUAGE ViewPatterns, PatternSynonyms, TypeOperators, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, DataKinds, KindSignatures, GADTs #-}

module Transform.Capture.ScopeEff where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Helper.Prog
import Helper.Co
import Helper.Eff.State
import Helper.Eff.Fresh
import Helper.Eff.Writer

data ScopeMod v
    -- Variable is not modified across different scopes.
    = Clean v
    -- Variable is modified across different scopes.
    | Dirty v
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Modified v k
    -- Tells environment that variable v was modified.
    = Modified' v k
    deriving Functor

data Scope k
    -- Tells environment that scope was entered.
    -- If a variable was modified in above scope, and lower scope, then it
    -- is dirty.
    = Scope' k
    deriving Functor

pattern Modified v k <- (prj -> Just (Modified' v k))
modified :: (Functor f, Functor g, Modified v :<: f) => v -> Prog f g ()
modified v = inject (Modified' v (Var ()))

pattern Scope k <- (prj -> Just (Scope' k))
scope :: (Functor f, Functor g, Scope :<: g) => Prog f g a -> Prog f g a
scope inner = injectS (fmap (fmap return) (Scope' inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- Use index to keep track of current scope
-- Keep track of index (scope) variable was modified in
-- If another modification occurs in a different scope, then variable is
-- dirty and added to dirty set.
--
-- Requires:
--  - Fresh supply of indices
--  - Statefully keeping track of current scope index
--  - Stateful mapping from variable names to scope last modified in
--  - Set of dirty variables that is written out to

-- Each scope is given a unique index.
type ScopeIdx    = Word
-- Mapping from variables to the scope they were modified in. If try to write
-- a different scope index than already exists then the variable is dirty.
type LastScope v = Map v ScopeIdx
-- All dirty variables found.
type DirtyVars v = Set v

-- Ordering ensures writer and fresh are global. However this is not strictly
-- necessary as the local scoping is not used.
type Op  f v     = State   (LastScope v) :+: State   ScopeIdx :+: Tell (DirtyVars v) :+: Fresh :+: f
type Sc  g v     = LocalSt (LastScope v) :+: LocalSt ScopeIdx :+: g
type Hdl f g v a = Prog (Op f v) (Sc g v) a

data CarrierSc f g v a n
    = Sc { runSc :: Hdl f g v (CarrierSc' f g v a n) }

data CarrierSc' f g v a :: Nat -> * where
    CZ :: a -> CarrierSc' f g v a 'Z
    CS :: (Hdl f g v (CarrierSc' f g v a n)) -> CarrierSc' f g v a ('S n)
