
-- Composite scoped effect handler to perform renaming, implemented using
-- composite effects methods. Allows other effects to be added, e.g. exceptions
-- to ensure procedures have been defined before being used.

{-# LANGUAGE ViewPatterns, PatternSynonyms, TypeOperators, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, DataKinds #-}

module Transform.Rename.RenameEff
( FreshName(..)
, Rename
, LocalName
, name
, exists
, localNames
, handleRename
) where

import Control.Monad (mapM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Front.AST (Ident)
import Helper.Prog
import Helper.Pretty
import Helper.Co
import Helper.Eff.State
import Helper.Eff.Fresh
import Helper.Eff

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

type Prefix = String
data FreshName  = FreshName Prefix Word deriving Eq

instance Pretty FreshName where
    pretty (FreshName pre i) = do text pre; showable i

data Rename k
    -- Returns fresh name corresponding to renamed identifier. If the name
    -- is unseen then a fresh mapping is created.
    = Name' String (FreshName -> k)
    -- Returns whether a mapping exists.
    | Exists' String (Bool -> k)
    deriving Functor

data LocalName k
    -- Assigns fresh names to all supplied names, and uses these new names
    -- inside continuation. After local, original names are restored.
    = Local' [String] k
    deriving Functor

-- Smart constructors

-- Use pattern synonyms and view patterns suggested in Effect Handlers in Scope.
-- These help make pattern matching in effect handler more readable.
pattern Name v fk <- (prj -> Just (Name' v fk))
name :: (Functor f, Functor g) => Rename :<: f => String -> Prog f g FreshName
name v = inject (Name' v Var)

pattern Exists p fk <- (prj -> Just (Exists' p fk))
exists :: (Functor f, Functor g) => Rename :<: f => String -> Prog f g Bool
exists p = inject (Exists' p Var)

pattern Local vs k <- (prj -> Just (Local' vs k))
localNames :: (Functor f, Functor g) => LocalName :<: g => [String] -> Prog f g a -> Prog f g a
localNames vs inner = injectS (fmap (fmap return) (Local' vs inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

type Names = Map String FreshName

emptyNames :: Names
emptyNames = Map.empty

-- Creates a mapping from v to a fresh variable name, and updates state.
insFresh :: (Functor f, Functor g)
         => String -> Prog (State Names :+: Fresh :+: f) (LocalSt Names :+: g) FreshName
insFresh v = do
    env <- get
    next <- fresh
    let f = FreshName "v" next
    put (Map.insert v f env)
    return f

-- Creates a mapping from each variable to a fresh name, and updates state,
-- returning updated state.
insManyFresh :: (Functor f, Functor g)
             => [String] -> Prog (State Names :+: Fresh :+: f) (LocalSt Names :+: g) Names
insManyFresh vs = do mapM_ insFresh vs; get

-- Overwrites duplicate entries with old variable names.
restoreNames :: Names -> Names -> Names
restoreNames old new = Map.union old new

-- Describe renaming in terms of State and FreshName effect handlers.

type Carrier f g a = CarrierId (Prog (State Names :+: Fresh :+: f) (LocalSt Names :+: g) a)

algRn :: (Functor f, Functor g) => Alg (Rename :+: f) (LocalName :+: g) (Carrier f g a)
algRn = A a d p where
    a :: (Functor f, Functor g) => (Rename :+: f) (Carrier f g a n) -> Carrier f g a n
    a (Name v fk) = Id $ do
        env <- get
        case Map.lookup v env of
            -- Mapping already exists, so just return it.
            Just fresh -> unId (fk fresh)
            -- No mapping exists, so create a new one.
            Nothing -> do
                f <- insFresh v
                unId (fk f)

    a (Other op) = Id (Op (fmap unId (R (R op))))

    d :: (Functor f, Functor g) => (LocalName :+: g) (Carrier f g a ('S n)) -> Carrier f g a n
    d (Local vs k) = Id $ do
        saved <- get
        ins   <- insManyFresh vs

        -- Want to propagate out any newly added mappings of names to fresh names.
        (r, localEnv) <- local ins (do
            r <- unId k
            e <- get
            return (r, e))

        put (restoreNames saved localEnv)
        return r

    p :: (Functor f, Functor g) => Carrier f g a n -> Carrier f g a ('S n)
    p (Id prog) = Id prog

mkRename :: (Functor f, Functor g)
         => Prog (Rename :+: f) (LocalName :+: g) a
         -> Prog (State Names :+: Fresh :+: f) (LocalSt Names :+: g) a
mkRename = runId (Id . return) algRn

handleRename :: (Functor f, Functor g) => Prog (Rename :+: f) (LocalName :+: g) a -> Prog f g a
handleRename prog = do
    -- Discard resulting Names, and next Fresh
    ((x, _), _) <- (handleFresh . handleState emptyNames . mkRename) prog
    return x
