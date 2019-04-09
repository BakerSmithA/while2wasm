
-- Composite scoped effect handler to perform renaming, implemented using
-- composite effects methods. Allows other effects to be added, e.g. exceptions
-- to ensure procedures have been defined before being used.

{-# LANGUAGE ViewPatterns, PatternSynonyms, TypeOperators, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Transform.Rename.RenameEff
( FreshName
, Fresh
, Rename
, fresh
, exists
, rename
, handleRename
) where

import Control.Monad (mapM_)
import Data.Map (Map)
import qualified Data.Map as Map
import Helper.Scope.Prog
import Helper.Pretty
import Helper.Co
import Helper.Eff.State
import qualified Helper.Eff.Fresh as F
import Helper.Eff

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

type FreshName = Word

instance Pretty FreshName where
    pretty n = do text "$"; showable n

-- v is the type of variables to be renamed.
data Fresh v k
    -- Returns fresh name corresponding to renamed identifier. If the name
    -- is unseen then a fresh mapping is created.
    = Fresh' v (FreshName -> k)
    -- Returns whether a mapping exists.
    | Exists' v (Bool -> k)
    deriving Functor

data Rename v k
    -- Assigns fresh names to all supplied names, and uses these new names
    -- inside continuation. After local, original names are restored.
    = Rename' [v] k
    deriving Functor

-- Smart constructors

-- Use pattern synonyms and view patterns suggested in Effect Handlers in Scope.
-- These help make pattern matching in effect handler more readable.
pattern Fresh v fk <- (prj -> Just (Fresh' v fk))
fresh :: (Functor f, Functor g) => Fresh v :<: f => v -> Prog f g FreshName
fresh v = injectP (Fresh' v Var)

pattern Exists p fk <- (prj -> Just (Exists' p fk))
exists :: (Functor f, Functor g) => Fresh v :<: f => v -> Prog f g Bool
exists p = injectP (Exists' p Var)

pattern Rename vs k <- (prj -> Just (Rename' vs k))
rename :: (Functor f, Functor g, Rename v :<: g) => [v] -> Prog f g a -> Prog f g a
rename vs inner = injectPSc (fmap (fmap return) (Rename' vs inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

type Names v = Map v FreshName

emptyNames :: Names v
emptyNames = Map.empty

-- Creates a mapping from v to a fresh variable name, and updates state.
insFresh :: (Functor f, Functor g, Ord v)
         => v -> Prog (State (Names v) :+: F.Fresh :+: f) (LocalSt (Names v) :+: g) FreshName
insFresh v = do
    env <- get
    next <- F.fresh
    put (Map.insert v next env)
    return next

-- Creates a mapping from each variable to a fresh name, and updates state,
-- returning updated state.
insManyFresh :: (Functor f, Functor g, Ord v)
             => [v] -> Prog (State (Names v) :+: F.Fresh :+: f) (LocalSt (Names v) :+: g) (Names v)
insManyFresh vs = do mapM_ insFresh vs; get

-- Overwrites duplicate entries with old variable names.
restoreNames :: Ord v => Names v -> Names v -> Names v
restoreNames old new = Map.union old new

-- Describe renaming in terms of State and FreshName effect handlers.

-- This ordering of effect handlers ensures the fresh is global, and so
-- even inside local scope of a state, globally fresh values will be produced.
-- Also see `handleRename` function.
type Ctx f g v a = Prog (State (Names v) :+: F.Fresh :+: f) (LocalSt (Names v) :+: g) a

-- Need to use carriers using CZ and CS because the state needs to be updated
-- after running local continuation, before running remaining continuation.
data CarrierRn f g v a n
    = Rn { runRn :: Ctx f g v (CarrierRn' f g v a n) }

data CarrierRn' f g v a :: Nat -> * where
    CZ :: a -> CarrierRn' f g v a 'Z
    CS :: (Ctx f g v (CarrierRn' f g v a n)) -> CarrierRn' f g v a ('S n)

getNames :: (Functor f, Functor g) => Ctx f g v (Names v)
getNames = get

genRn :: (Functor f, Functor g) => a -> CarrierRn f g v a 'Z
genRn x = Rn (return (CZ x))

algRn :: (Functor f, Functor g, Ord v) => Alg (Fresh v :+: f) (Rename v :+: g) (CarrierRn f g v a)
algRn = A a d p where
    a :: (Functor f, Functor g, Ord v) => (Fresh v :+: f) (CarrierRn f g v a n) -> CarrierRn f g v a n
    a (Fresh v fk) = Rn $ do
        env <- get
        case Map.lookup v env of
           -- Mapping already exists, so just return it.
           Just fresh -> runRn (fk fresh)
           -- No mapping exists, so create a new one.
           Nothing -> do
               f <- insFresh v
               runRn (fk f)

    a (Exists v fk) = Rn $ do
        env <- getNames
        let exists = v `Map.member` env
        runRn (fk exists)

    a (Other op) = Rn (Op (fmap runRn (R (R op))))

    d :: (Functor f, Functor g, Ord v) => (Rename v :+: g) (CarrierRn f g  v a ('S n)) -> CarrierRn f g v a n
    d (Rename vs k) = Rn $ do
        saved <- getNames
        ins   <- insManyFresh vs

        -- Run nested continuation with local state.
        -- run' is the continuation remaining after the local continuation.
        -- Before running this the state is restored.
        (CS run', localEnv) <- localSt ins (do
            r <- runRn k
            e <- getNames
            return (r, e))

        -- Run remaining continuation with restored state.
        put (restoreNames saved localEnv)
        run'

    d (Other op) = Rn (Scope (fmap (\(Rn prog) -> fmap f prog) (R op))) where
        f :: (Functor f, Functor g) => CarrierRn' f g v a ('S n) -> Ctx f g v (CarrierRn' f g v a n)
        f (CS prog) = prog

    p :: (Functor f, Functor g) => CarrierRn f g v a n -> CarrierRn f g v a ('S n)
    p (Rn runRn) = Rn (return (CS runRn))

mkCtx :: (Functor f, Functor g, Ord v) => Prog (Fresh v :+: f) (Rename v :+: g) a -> Ctx f g v a
mkCtx prog = case run genRn algRn prog of
    (Rn prog') -> do
        (CZ x) <- prog'
        return x

handleRename :: (Functor f, Functor g, Ord v) => Prog (Fresh v :+: f) (Rename v :+: g) a -> Prog f g a
handleRename prog = do
    -- Discard resulting Names, and next fresh.
    -- The fresh is global, indicated by wrapping around the state. Therefore,
    -- getting a new fresh inside scoped state still gives a globally fresh
    -- value.
    ((x, st), fresh) <- (F.handleFresh 0 . handleState emptyNames . mkCtx) prog
    return x
