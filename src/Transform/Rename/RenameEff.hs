
-- Composite scoped effect handler to perform renaming, implemented using
-- composite effects methods. Allows other effects to be added, e.g. exceptions
-- to ensure procedures have been defined before being used.

{-# LANGUAGE ViewPatterns, PatternSynonyms, TypeOperators, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}

module Transform.Rename.RenameEff
( FreshName(..)
, Rename
, LocalName
, name
, exists
, localNames
, handleRename
) where

import Data.Map (Map)
import qualified Data.Map as Map
import Front.AST (Ident)
import Helper.Prog
import Helper.Pretty
import Helper.Co
import Helper.Eff.State
import Helper.Eff.Fresh

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

type Prefix = String
data FreshName  = FreshName Prefix Word deriving Eq

instance Pretty FreshName where
    pretty (FreshName pre i) = do text pre; showable i

data Rename v k
    -- Returns fresh name corresponding to renamed identifier. If the name
    -- is unseen then a fresh mapping is created.
    = Name' v (FreshName -> k)
    -- Returns whether a mapping exists.
    | Exists' v (Bool -> k)
    deriving Functor

data LocalName v k
    -- Assigns fresh names to all supplied names, and uses these new names
    -- inside continuation. After local, original names are restored.
    = Local' [v] k
    deriving Functor

-- Smart constructors

-- Use pattern synonyms and view patterns suggested in Effect Handlers in Scope.
-- These help make pattern matching in effect handler more readable.
pattern Name v fk <- (prj -> Just (Name' v fk))
name :: (Functor f, Functor g) => Rename v :<: f => v -> Prog f g FreshName
name v = inject (Name' v Var)

pattern Exists p fk <- (prj -> Just (Exists' p fk))
exists :: (Functor f, Functor g) => Rename v :<: f => v -> Prog f g Bool
exists p = inject (Exists' p Var)

pattern Local vs k <- (prj -> Just (Local' vs k))
localNames :: (Functor f, Functor g) => LocalName v :<: g => [v] -> Prog f g a -> Prog f g a
localNames vs inner = injectS (fmap (fmap return) (Local' vs inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

type Names v = Map v FreshName

-- Describe renaming in terms of State and FreshName effect handlers.

mkRename :: (Functor f, Functor g)
         => Prog (Rename v :+: f) (LocalName v :+: g) a
         -> Prog (State (Names v) :+: Fresh :+: f) (LocalSt (Names v) :+: g) a
mkRename = undefined

handleRename :: (Functor f, Functor g) => Prog (Rename v :+: f) (LocalName v :+: g) a -> Prog f g a
handleRename = undefined
