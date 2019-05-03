
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

type Carrier a = CarrierId (IO a)

gen :: a -> CarrierId (IO a) 'Z
gen x = Id (return x)

alg :: Functor f => Alg (Console :+: f) g (Carrier a)
alg = A a d p where
    a :: Functor f => (Console :+: f) (Carrier a n) -> Carrier a n
    a (Gets fk) = Id $ do
        s <- getLine
        unId (fk s)

    a (Puts )

    -- a (Gets fk) = Id $ do
    --     _
    --
    -- a (Puts s k) = Id $ do
    --     k' <- unId k
    --     return (do
    --         putStrLn s
    --         k')

    -- a (Other op) = Id (Op (fmap unId op))

    d :: g (Carrier a ('S n)) -> Carrier a n
    d = undefined

    -- d op = Id $ (Scope (fmap (return . unId) op))

    p :: Carrier a n -> Carrier a ('S n)
    p = undefined
    -- p (Id p) = Id p

handleConsole :: (Functor f, Functor g) => Prog (Console :+: f) g a -> IO a
handleConsole prog = runId gen alg prog
