
-- Composite scoped effect handler to perform renaming, implemented using
-- composite effects methods. Allows other effects to be added, e.g. exceptions
-- to ensure procedures have been defined before being used.

{-# LANGUAGE ViewPatterns, PatternSynonyms, TypeOperators, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, DataKinds, KindSignatures, GADTs #-}

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
import Debug.Trace

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

type Prefix = String
data FreshName = FreshName Prefix Word deriving (Eq, Show)

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

type P f g a = Prog (State Names :+: Fresh :+: f) (LocalSt Names :+: g) a

-- Need to use carriers using CZ and CS because the state needs to be updated
-- after running local continuation, before running remaining continuation.
data CarrierRn f g a n
    = Rn { runRn :: P f g (CarrierRn' f g a n) }

data CarrierRn' f g a :: Nat -> * where
    CZ :: a -> CarrierRn' f g a 'Z
    CS :: (P f g (CarrierRn' f g a n)) -> CarrierRn' f g a ('S n)

genRn :: (Functor f, Functor g) => a -> CarrierRn f g a 'Z
genRn x = Rn (return (CZ x))

algRn :: (Functor f, Functor g) => Alg (Rename :+: f) (LocalName :+: g) (CarrierRn f g a)
algRn = A a d p where
    a :: (Functor f, Functor g) => (Rename :+: f) (CarrierRn f g a n) -> CarrierRn f g a n
    a (Name v fk) = Rn $ do
        env <- get
        case Map.lookup v env of
           -- Mapping already exists, so just return it.
           Just fresh -> runRn (fk fresh)
           -- No mapping exists, so create a new one.
           Nothing -> do
               f <- insFresh v
               runRn (fk f)

    a (Other op) = Rn (Op (fmap runRn (R (R op))))

    d :: (Functor f, Functor g) => (LocalName :+: g) (CarrierRn f g a ('S n)) -> CarrierRn f g a n
    d (Local vs k) = Rn $ do
        saved <- getNames
        ins   <- insManyFresh vs

        -- Run nested continuation with local state.
        -- run' is the continuation remaining after the local continuation.
        -- Before running this the state is restored.
        (CS run', localEnv) <- local ins (do
            r <- runRn k
            e <- get
            return (r, e :: Names))

        -- Run remaining continuation with restored state.
        put (restoreNames saved localEnv)
        run'

    d (Other op) = Rn (Scope (fmap (\(Rn prog) -> fmap f prog) (R op))) where
        f :: (Functor f, Functor g) => CarrierRn' f g a ('S n) -> P f g (CarrierRn' f g a n)
        f (CS prog) = prog

    getNames :: (Functor f, Functor g) => Prog (State Names :+: f) (LocalSt Names :+: g) Names
    getNames = get

    p :: (Functor f, Functor g) => CarrierRn f g a n -> CarrierRn f g a ('S n)
    p (Rn runRn) = Rn (return (CS runRn))

mkRename :: (Functor f, Functor g)
         => Prog (Rename :+: f) (LocalName :+: g) a
         -> Prog (State Names :+: Fresh :+: f) (LocalSt Names :+: g) a
mkRename prog = case run genRn algRn prog of
    (Rn prog') -> do
        (CZ x) <- prog'
        return x

handleRename :: (Functor f, Functor g) => Prog (Rename :+: f) (LocalName :+: g) a -> Prog f g a
handleRename prog = do
    -- Discard resulting Names, and next Fresh
    ((x, _), _) <- (handleFresh . handleState emptyNames . mkRename) prog
    return x
