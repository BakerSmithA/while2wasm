
-- Composite scoped effect handler to annotate variables with whether they are
-- local or foreign to the current scope. Within the same scope, all occurrences
-- of the variable will be of the same type.
--
-- Whether a variable is local or foreign  affects which functions variables
-- belong to in outputted WebAssembly.
--
-- WARNING: Makes assumption that all variables are unique.

{-# LANGUAGE ViewPatterns, PatternSynonyms, TypeOperators, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Transform.Capture.StoreEff
( Store(..)
, StoreType
, Add
, Discard
, storeType
, addLocals
, discardLocals
, handleStore
) where

import qualified Data.Set as Set
import Helper.Prog
import Helper.Co
import Helper.Eff.State
import Helper.Eff

-- Type needs to wrap each variable in AST because the same variable at
-- different points in the AST may be local or foreign. This is unlike whether
-- a variable is dirty or clean, which is a global property of each variable.
data Store v
    -- Variable local to a level of scope, analagous to a variable decalared
    -- inside a function.
    = Local v
    -- Variable used outside the scope it was declared, analagous to an argument
    -- passed into a function.
    | Foreign v
    deriving (Eq, Show)

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data StoreType v k
    -- Retrieves whether a variable is local or foreign to the current scope.
    = StoreType' v (Store v -> k)
    deriving Functor

data Add v k
    -- Tells environment that variables inside continuation are local.
    -- This does not change whether other variables are local or foreign.
    = Add' [v] k
    deriving Functor

-- Separated from Add type because no v parameter, so to avoid ambiguous types.
data Discard k
    -- Tells environment that any variables inside continuation should be
    -- treated as foreign, unless overwritten by nested Locals.
    = Discard' k
    deriving Functor

pattern StoreType v fk <- (prj -> Just (StoreType' v fk))
storeType :: (Functor f, Functor g, StoreType v :<: f) => v -> Prog f g (Store v)
storeType v = inject (StoreType' v Var)

pattern Add vs k <- (prj -> Just (Add' vs k))
addLocals :: (Functor f, Functor g, Add v :<: g) => [v] -> Prog f g a -> Prog f g a
addLocals vs inner = injectS (fmap (fmap return) (Add' vs inner))

pattern Discard k <- (prj -> Just (Discard' k))
discardLocals :: (Functor f, Functor g, Discard :<: g) => Prog f g a -> Prog f g a
discardLocals inner = injectS (fmap (fmap return) (Discard' inner))

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

type Ctx f g v a = Prog (State (IsLocal v) :+: f) (LocalSt (IsLocal v) :+: g) a

data CarrierSt f g v a n
    = St { runSt :: Ctx f g v (CarrierSt' f g v a n) }

data CarrierSt' f g v a :: Nat -> * where
    CZ :: a -> CarrierSt' f g v a 'Z
    CS :: (Ctx f g v (CarrierSt' f g v a n)) -> CarrierSt' f g v a ('S n)

isVLocal :: (Functor f, Functor g) => v -> Ctx f g v Bool
isVLocal v = do
    isLocal <- get
    return (isLocal v)

getIsLocal :: (Functor f, Functor g) => Ctx f g v (IsLocal v)
getIsLocal = get

noneLocal' :: (Functor f, Functor g) => Ctx f g v (IsLocal v)
noneLocal' = return noneLocal

genSt :: (Functor f, Functor g) => a -> CarrierSt f g v a 'Z
genSt x = St (return (CZ x))

algSt :: (Functor f, Functor g, Ord v) => Alg (StoreType v :+: f) (Add v :+: Discard :+: g) (CarrierSt f g v a)
algSt = A a d p where
    a :: (Functor f, Functor g) => (StoreType v :+: f) (CarrierSt f g v a n) -> CarrierSt f g v a n
    a (StoreType v fk) = St $ do
        b <- isVLocal v
        if b
            then runSt (fk (Local v))
            else runSt (fk (Foreign v))

    a (Other op) = St (Op (fmap runSt (R op)))

    d :: (Functor f, Functor g, Ord v) => (Add v :+: Discard :+: g) (CarrierSt f g v a ('S n)) -> CarrierSt f g v a n
    d (Add vs k) = St $ do
        isLocal <- getIsLocal
        let vsIsLocal = isLocalFromList vs
        -- Inside local block, both the original and new variables are local.
        (CS run') <- localSt (isLocal `orLocal` vsIsLocal) (runSt k)
        -- Original locals are restored by local state.
        run'

    d (Discard k) = St $ do
        -- TODO: How to make this work outwith ugly-ness. Problem with types regarding v.
        n <- noneLocal'
        -- Run continuation with no local variables.
        (CS run') <- localSt n (runSt k)
        -- Original locals are restored by local state.
        run'

    d (Other (Other op)) = St (Scope (fmap (\(St prog) -> fmap f prog) (R op))) where
        f :: (Functor f, Functor g) => CarrierSt' f g v a ('S n) -> Ctx f g v (CarrierSt' f g v a n)
        f (CS prog) = prog

    p :: (Functor f, Functor g, Ord v) => CarrierSt f g v a n -> CarrierSt f g v a ('S n)
    p (St runSt) = St (return (CS runSt))

mkStore :: (Functor f, Functor g, Ord v) => Prog (StoreType v :+: f) (Add v :+: Discard :+: g) a -> Ctx f g v a
mkStore prog = case run genSt algSt prog of
    (St prog') -> do
        (CZ x) <- prog'
        return x

handleStore :: (Functor f, Functor g, Ord v)
            => Prog (StoreType v :+: f) (Add v :+: Discard :+: g) a -> Prog f g a
handleStore prog = do
    -- Discard resulting state.
    (x, _) <- (handleState allLocal . mkStore) prog
    return x
