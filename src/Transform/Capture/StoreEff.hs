
-- Composite scoped effect handler to modify variables to be local or foreign.
-- Implemented using as a composite effect to allow to be used with effect
-- handler which determines whether a variable is a pointer or value.

{-# LANGUAGE ViewPatterns, PatternSynonyms, TypeOperators, DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts, DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Transform.Capture.StoreEff where

import Data.Map (Map)
import qualified Data.Map as Map
import Helper.Prog
import Helper.Co
import Helper.Eff.State

data Store v
    -- Variable local to a level of scope.
    = Local v
    -- Variable used outside the scope it was declared.
    | Foreign v
    deriving Eq

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Type v k
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
storeType :: (Functor f, Functor g) => Type v :<: f => v -> Prog f g (Store v)
storeType v = inject (StoreType' v Var)

pattern Add vs k <- (prj -> Just (Add' vs k))
added :: (Functor f, Functor g) => Add v :<: g => [v] -> Prog f g () -> Prog f g ()
added vs inner = injectS (fmap (fmap return) (Add' vs inner))

pattern Discard k <- (prj -> Just (Discard' k))
discard :: (Functor f, Functor g) => Discard :<: g => Prog f g () -> Prog f g ()
discard inner = injectS (fmap (fmap return) (Discard' inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------


-- Whether a variable was declared at the current scope.
type IsLocal  v = v -> Bool
-- Where a variable is stored corresponding to its name.
type VarTypes v = Map v (Store v)
-- So it can be easily passed around.
type Env v = (IsLocal v, VarTypes v)

emptyTopEnv :: Env v
emptyTopEnv = undefined

type Hdl f g v a = Prog (State (Env v) :+: f) (LocalSt (Env v) :+: g) a

data CarrierSt f g v a n
    = St { runSt :: Hdl f g v (CarrierSt' f g v a n) }

data CarrierSt' f g v a :: Nat -> * where
    CZ :: a -> CarrierSt' f g v a 'Z
    CS :: (Hdl f g v (CarrierSt' f g v a n)) -> CarrierSt' f g v a ('S n)

genSt :: (Functor f, Functor g) => a -> CarrierSt f g v a 'Z
genSt x = St (return (CZ x))

algSt :: (Functor f, Functor g) => Alg (Type v :+: f) (Add v :+: Discard :+: g) (CarrierSt f g v a)
algSt = undefined

mkStore :: (Functor f, Functor g, Ord v)
        => Prog (Type v :+: f) (Add v :+: Discard :+: g) a
        -> Prog (State (Env v) :+: f) (LocalSt (Env v) :+: g) a
mkStore prog = case run genSt algSt prog of
    (St prog') -> do
        (CZ x) <- prog'
        return x

handleStore :: (Functor f, Functor g, Ord v)
            => Prog (Type v :+: f) (Add v :+: Discard :+: g) a -> Prog f g a
handleStore prog = do
    -- Discard resulting state.
    (x, _) <- (handleState emptyTopEnv . mkStore) prog
    return x
