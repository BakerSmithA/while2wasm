
-- Composite scoped effect handler to perform renaming, implemented using
-- composite effects methods. Allows other effects to be added, e.g. exceptions
-- to ensure procedures have been defined before being used.

{-# LANGUAGE ViewPatterns, PatternSynonyms, TypeOperators, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Transform.Rename.RenameEff
( FreshName
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

type FreshName = Word

instance Pretty FreshName where
    pretty n = do text "$"; showable n

-- v is the type of variables to be renamed.
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

emptyNames :: Names v
emptyNames = Map.empty

-- Creates a mapping from v to a fresh variable name, and updates state.
insFresh :: (Functor f, Functor g, Ord v)
         => v -> Prog (State (Names v) :+: Fresh :+: f) (LocalSt (Names v) :+: g) FreshName
insFresh v = do
    env <- get
    next <- fresh
    put (Map.insert v next env)
    return next

-- Creates a mapping from each variable to a fresh name, and updates state,
-- returning updated state.
insManyFresh :: (Functor f, Functor g, Ord v)
             => [v] -> Prog (State (Names v) :+: Fresh :+: f) (LocalSt (Names v) :+: g) (Names v)
insManyFresh vs = do mapM_ insFresh vs; get

-- Overwrites duplicate entries with old variable names.
restoreNames :: Ord v => Names v -> Names v -> Names v
restoreNames old new = Map.union old new

getNames :: (Functor f, Functor g) => Prog (State (Names v) :+: f) (LocalSt (Names v) :+: g) (Names v)
getNames = get

-- Describe renaming in terms of State and FreshName effect handlers.

type Hdl f g v a = Prog (State (Names v) :+: Fresh :+: f) (LocalSt (Names v) :+: g) a

-- Need to use carriers using CZ and CS because the state needs to be updated
-- after running local continuation, before running remaining continuation.
data CarrierRn f g v a n
    = Rn { runRn :: Hdl f g v (CarrierRn' f g v a n) }

data CarrierRn' f g v a :: Nat -> * where
    CZ :: a -> CarrierRn' f g v a 'Z
    CS :: (Hdl f g v (CarrierRn' f g v a n)) -> CarrierRn' f g v a ('S n)

genRn :: (Functor f, Functor g) => a -> CarrierRn f g v a 'Z
genRn x = Rn (return (CZ x))

algRn :: (Functor f, Functor g, Ord v) => Alg (Rename v :+: f) (LocalName v :+: g) (CarrierRn f g v a)
algRn = A a d p where
    a :: (Functor f, Functor g, Ord v) => (Rename v :+: f) (CarrierRn f g v a n) -> CarrierRn f g v a n
    a (Name v fk) = Rn $ do
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

    d :: (Functor f, Functor g, Ord v) => (LocalName v :+: g) (CarrierRn f g  v a ('S n)) -> CarrierRn f g v a n
    d (Local vs k) = Rn $ do
        saved <- getNames
        ins   <- insManyFresh vs

        -- Run nested continuation with local state.
        -- run' is the continuation remaining after the local continuation.
        -- Before running this the state is restored.
        (CS run', localEnv) <- local ins (do
            r <- runRn k
            e <- getNames
            return (r, e))

        -- Run remaining continuation with restored state.
        put (restoreNames saved localEnv)
        run'

    d (Other op) = Rn (Scope (fmap (\(Rn prog) -> fmap f prog) (R op))) where
        f :: (Functor f, Functor g) => CarrierRn' f g v a ('S n) -> Hdl f g v (CarrierRn' f g v a n)
        f (CS prog) = prog

    p :: (Functor f, Functor g) => CarrierRn f g v a n -> CarrierRn f g v a ('S n)
    p (Rn runRn) = Rn (return (CS runRn))

mkRename :: (Functor f, Functor g, Ord v)
         => Prog (Rename v :+: f) (LocalName v :+: g) a
         -> Prog (State (Names v) :+: Fresh :+: f) (LocalSt (Names v) :+: g) a
mkRename prog = case run genRn algRn prog of
    (Rn prog') -> do
        (CZ x) <- prog'
        return x

handleRename :: (Functor f, Functor g, Ord v) => Prog (Rename v :+: f) (LocalName v :+: g) a -> Prog f g a
handleRename prog = do
    -- Discard resulting Names, and next Fresh
    ((x, _), _) <- (handleFresh . handleState emptyNames . mkRename) prog
    return x
