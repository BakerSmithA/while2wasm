
-- Produces mapping from variable name to whether it is local or foreign to a
-- scope. This is used to decide whether a WebAssembly function owns a variable.
--
-- WARNING: Assumes all variable names are unique.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms, DataKinds, KindSignatures, GADTs #-}

module Transform.Capture.LocationEff
( LocalVars
, ForeignVars
, Locations
, LocOp
, Add
, Discard
, seen
, getLocations
, addLocals
, discardLocals
, handleLoc
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Reader
import Helper.Eff.State
import Helper.Eff
import Debug.Trace

type LocalVars   v = Set v
type ForeignVars v = Set v
type Locations v = (LocalVars v, ForeignVars v)

emptyLocatons :: Locations v
emptyLocatons = (Set.empty, Set.empty)

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data LocOp v k
    -- Tells the environment that variable v was seen at the current scope.
    = Seen' v k
    -- Retrieves the mapping from variable names to locations in the curret scope.
    | GetLocations' (Locations v -> k)
    deriving Functor

data Add v k
    -- Tells environment that variables inside continuation are local.
    -- Also modifies mapping from variables to location to make variables local
    -- at current scope.
    = Add' [v] k
    deriving Functor

-- Separated from Add type because no v parameter, so to avoid ambiguous types.
data Discard k
    -- Tells environment that any variables inside continuation should be
    -- treated as foreign, unless overwritten by nested Locals.
    = Discard' k
    deriving Functor

pattern Seen v k <- (prj -> Just (Seen' v k))
seen :: (Functor f, Functor g, LocOp v :<: f) => v -> Prog f g ()
seen v = injectP (Seen' v (Var ()))

pattern GetLocations fk <- (prj -> Just (GetLocations' fk))
getLocations :: (Functor f, Functor g, LocOp v :<: f) => Prog f g (Locations v)
getLocations = injectP (GetLocations' Var)

pattern Add vs k <- (prj -> Just (Add' vs k))
addLocals :: (Functor f, Functor g, Add v :<: g) => [v] -> Prog f g a -> Prog f g a
addLocals vs inner = injectPSc (fmap (fmap return) (Add' vs inner))

pattern Discard k <- (prj -> Just (Discard' k))
discardLocals :: (Functor f, Functor g, Discard :<: g) => Prog f g a -> Prog f g a
discardLocals inner = injectPSc (fmap (fmap return) (Discard' inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- Whether a variable was declared at the current scope.
type IsLocal v = v -> Bool

-- Simple shallow DSL to describe which variables are local.
allLocal :: IsLocal v
allLocal = const True

noneLocal :: IsLocal v
noneLocal = const False

isLocalFromList :: Ord v => [v] -> IsLocal v
isLocalFromList vs v = v `Set.member` Set.fromList vs

orLocal :: IsLocal v -> IsLocal v -> IsLocal v
orLocal x y v = x v || y v

type Op  f v     = Ask    (IsLocal v) :+: State   (Locations v) :+: f
type Sc  g v     = LocalR (IsLocal v) :+: LocalSt (Locations v) :+: g
type Ctx f g v a = Prog (Op f v) (Sc g v) a

data Carrier f g v a n
    = Lc { runL :: Ctx f g v (Carrier' f g v a n) }

data Carrier' f g v a :: Nat -> * where
    CZ :: a -> Carrier' f g v a 'Z
    CS :: (Ctx f g v (Carrier' f g v a n)) -> Carrier' f g v a ('S n)

askIsLocal :: (Functor f, Functor g, Ask (IsLocal v) :<: f) => Prog f g (IsLocal v)
askIsLocal = ask

getLocMappings :: (Functor f, Functor g, State (Locations v) :<: f) => Prog f g (Locations v)
getLocMappings = get

putLocMappings :: (Functor f, Functor g, State (Locations v) :<: f) => Locations v -> Prog f g ()
putLocMappings = put

genL :: (Functor f, Functor g) => a -> Carrier f g v a 'Z
genL x = Lc (return (CZ x))

algL :: (Functor f, Functor g, Ord v) => Alg (LocOp v :+: f) (Add v :+: Discard :+: g) (Carrier f g v a)
algL = A a d p where
    a :: (Functor f, Functor g, Ord v) => (LocOp v :+: f) (Carrier f g v a n) -> Carrier f g v a n
    a = undefined

    d :: (Functor f, Functor g, Ord v) => (Add v :+: Discard :+: g) (Carrier f g v a ('S n)) -> Carrier f g v a n
    d = undefined

    p :: (Functor f, Functor g, Ord v) => Carrier f g v a n -> Carrier f g v a ('S n)
    p = undefined

mkLoc :: (Functor f, Functor g, Ord v) => Prog (LocOp v :+: f) (Add v :+: Discard :+: g) a -> Ctx f g v a
mkLoc prog = case run genL algL prog of
    (Lc prog') -> do
        (CZ x) <- prog'
        return x

-- Returns result and mapping from variable names to locations at the top level scope.
handleLoc :: (Functor f, Functor g, Ord v) => Prog (LocOp v :+: f) (Add v :+: Discard :+: g) a -> Prog f g a
handleLoc = fmap fst . handleState emptyLocatons . handleReader allLocal . mkLoc
