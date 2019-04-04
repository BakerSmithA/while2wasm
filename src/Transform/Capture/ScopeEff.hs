
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
{-# LANGUAGE FlexibleContexts #-}

module Transform.Capture.ScopeEff where

import Helper.Prog
import Helper.Co

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
