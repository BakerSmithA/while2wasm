
-- Composite scoped effect handler to find variables which are written
-- across different scopes. This affects whether variables need to be stored as
-- values or pointers in outputted WASM.
--
-- WARNING: Makes assumption that all variables are unique.

{-# LANGUAGE ViewPatterns, PatternSynonyms, TypeOperators, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, DataKinds, KindSignatures, GADTs #-}

module Transform.Capture.DirtyEff
( Modified
, ModScope
, DirtyVars
, modified
, modScope
, handleDirtyVars
) where

import Data.Word (Word)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Helper.Scope.Prog
import Helper.Scope.Nest
import Helper.Co
import Helper.Inj
import Helper.Eff
import Helper.Eff.State
import Helper.Eff.Reader
import Helper.Eff.New
import Helper.Eff.Writer

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Modified v k
    -- Tells environment that variable v was modified.
    = Modified' v k
    deriving Functor

data ModScope k
    -- Tells environment that scope was entered.
    -- If a variable was modified in above scope, and lower scope, then it
    -- is dirty.
    = ModScope' k
    deriving Functor

pattern Modified v k <- (prj -> Just (Modified' v k))
modified :: (Functor f, Functor g, Modified v :<: f) => v -> Prog f g ()
modified v = injectP (Modified' v (Var ()))

pattern ModScope k <- (prj -> Just (ModScope' k))
modScope :: (Functor f, Functor g, ModScope :<: g) => Prog f g a -> Prog f g a
modScope inner = injectPSc (fmap (fmap return) (ModScope' inner))

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

emptyLastScope :: LastScope v
emptyLastScope = Map.empty

-- Ordering ensures writer and fresh are global.
-- Only need a reader for keeping track of the current scope index, because
-- the index only changes when entering scope, therefore this can be done
-- using scope of reader.
type Op  f v   = State   (LastScope v) :+: Ask    ScopeIdx :+: New Word :+: Tell (DirtyVars v) :+: f
type Sc  g v   = LocalSt (LastScope v) :+: LocalR ScopeIdx :+: g
type Ctx f g v = Prog (Op f v) (Sc g v)

type CarrierD f g v a = Nest1 (Ctx f g v) a

getLastScope :: (Functor f, Functor g) => Ctx f g v (LastScope v)
getLastScope = get

getScopeIdx :: (Functor f, Functor g) => Ctx f g v ScopeIdx
getScopeIdx = ask

newScopeIdx :: (Functor f, Functor g) => Ctx f g v ScopeIdx
newScopeIdx = new

genD :: (Functor f, Functor g) => a -> CarrierD f g v a 'Z
genD x = Nest1 (return (NZ1 x))

algD :: (Functor f, Functor g, Ord v) => Alg (Modified v :+: f) (ModScope :+: g) (CarrierD f g v a)
algD = A a d p where
    a ::  (Functor f, Functor g, Ord v) => (Modified v :+: f) (CarrierD f g v a n) -> CarrierD f g v a n
    -- Tells environment a variable was modified. If modified in two different
    -- scopes then make a dirty variable.
    a (Modified v k) = Nest1 $ do
        varScopes <- getLastScope
        case v `Map.lookup` varScopes of
            -- Variable never seen before, therefore the scope the variable
            -- was seen at is the current scope index.
            Nothing    -> do
                currScopeIdx <- getScopeIdx
                put (Map.insert v currScopeIdx varScopes)
                runNest1 k

            -- Variable has been seen before.
            Just scIdx -> do
                currScopeIdx <- getScopeIdx
                if scIdx == currScopeIdx
                    -- Variable modified in same scope, therefore no action required.
                    then runNest1 k
                    -- Variable modified in a different scope, therefore, update
                    -- set of dirty variables.
                    else do
                        tell (Set.singleton v)
                        runNest1 k

    a (Other op) = Nest1 (Op (fmap runNest1 (R $ R $ R $ R op)))

    d ::  (Functor f, Functor g) => (ModScope :+: g) (CarrierD f g v a ('S n)) -> CarrierD f g v a n
    -- Enters scope, in which if a variable is modified and modified in above
    -- scope then the variable will be made dirty.
    d (ModScope k) = Nest1 $ do
        -- Does not change the scope index assigned to this scope, only the
        -- next fresh scope index that will be used.
        newScIdx <- newScopeIdx
        -- Run nested continuation with new scope index.
        (NS1 run') <- localR newScIdx (runNest1 k)
        -- Run rest of non-nested continuation.
        run'

    d (Other op) = Nest1 (Scope (fmap (\(Nest1 prog) -> fmap f prog) (R $ R op))) where
        f :: (Functor f, Functor g) => Nest1' (Ctx f g v) a ('S n) -> Ctx f g v (Nest1' (Ctx f g v) a n)
        f (NS1 prog) = prog

    p ::  (Functor f, Functor g) => CarrierD f g v a n -> CarrierD f g v a ('S n)
    p (Nest1 runNest1) = Nest1 (return (NS1 runNest1))

mkCtx :: (Functor f, Functor g, Ord v) => Prog (Modified v :+: f) (ModScope :+: g) a -> Ctx f g v a
mkCtx prog = case run genD algD prog of
    (Nest1 prog') -> do
        (NZ1 x) <- prog'
        return x

handleDirtyVars :: (Functor f, Functor g, Ord v) => Prog (Modified v :+: f) (ModScope :+: g) a -> Prog f g (a, DirtyVars v)
handleDirtyVars prog = do
    -- Result has type (((a, LastScope v), Word), DirtyVars v)
    -- Therefore, we know fresh is global (i.e. wrapping states), and so is
    -- DirtyVars. This ensures local versions are not made inside any local
    -- state scopes.
    let srtScopeIdx = 0
        handle = handleWriter . handleNew (succ srtScopeIdx) . handleReader srtScopeIdx . handleState emptyLastScope
    (((x, _), _), vs) <- (handle . mkCtx) prog
    return (x, vs)
