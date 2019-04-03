
-- Composite scoped effect handler to modify variables to be local or foreign.
-- Implemented using as a composite effect to allow to be used with effect
-- handler which determines whether a variable is a pointer or value.

{-# LANGUAGE DeriveFunctor, ViewPatterns, PatternSynonyms, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module Transform.Capture.StoreEff where

import Data.Map (Map)
import qualified Data.Map as Map
import Helper.Prog
import Helper.Co

data Store v
    -- Variable local to a level of scope.
    = Local v
    -- Variable used outside the scope it was declared.
    | Foreign v

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

-- Like State effect.
data CarrierSt f g v a n
    = St { runSt :: Env v -> Prog f g (CarrierSt' f g v a n, Env v) }

data CarrierSt' f g v a :: Nat -> * where
    CZ :: a -> CarrierSt' f g v a 'Z
    CS :: (Env v -> Prog f g (CarrierSt' f g v a n, Env v)) -> CarrierSt' f g v a ('S n)

genSt :: (Functor f, Functor g) => a -> CarrierSt' f g v a 'Z
genSt x = St $ \env -> (return (CZ x, env))
