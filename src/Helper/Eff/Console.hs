
-- Console effects, i.e. get-line, put-line

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, GADTs #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms, DataKinds #-}

module Helper.Eff.Console where

import Helper.Scope.Prog
import Helper.Co
import Helper.Inj
import Helper.Eff

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Console k
    = Gets' (String -> k)
    | Puts' String k
    deriving Functor

pattern Gets fk <- (prj -> Just (Gets' fk))
gets :: Console :<: f => Prog f g String
gets = injectP (Gets' Var)

pattern Puts s k <- (prj -> Just (Puts' s k))
puts :: Console :<: f => String -> Prog f g ()
puts s = injectP (Puts' s (Var ()))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

type Carrier f g a = CarrierId (Prog f g (IO a))

gen :: (Functor f, Functor g) => a -> CarrierId (Prog f g (IO a)) 'Z
gen x = Id (return (return x))

alg :: (Functor f, Functor g) => Alg (Console :+: f) g (CarrierId (Prog f g (IO a)))
alg = A a d p where
    a :: (Functor f, Functor g) => (Console :+: f) (CarrierId (Prog f g (IO a)) n) -> CarrierId (Prog f g (IO a)) n
    a (Gets _) = error "Not done yet"
    a (Puts s k) = Id $ do
        k' <- unId k
        return (do
            putStrLn s
            k')
    a (Other op) = Id (Op (fmap unId op))

    d :: (Functor f, Functor g) => g (CarrierId (Prog f g (IO a)) ('S n)) -> CarrierId (Prog f g (IO a)) n
    d op = Id $ (Scope (fmap (return . unId) op))

    p :: CarrierId (Prog f g (IO a)) n -> CarrierId (Prog f g (IO a)) ('S n)
    p (Id p) = Id p

handleConsole :: (Functor f, Functor g) => Prog (Console :+: f) g a -> Prog f g (IO a)
handleConsole prog = runId gen alg prog
