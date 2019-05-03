
-- Console effects, i.e. get-line, put-line

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, GADTs #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms, DataKinds #-}

module Helper.Eff.Console where

import Helper.Scope.Prog
import Helper.Scope.ProgT (ProgT, AlgT)
import qualified Helper.Scope.ProgT as T
import Helper.Co
import Helper.Inj
import Helper.Eff
import Helper.Eff.Void

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Console k
    = Gets' (String -> k)
    | Puts' String k
    deriving Functor

pattern Gets fk <- (prj -> Just (Gets' fk))
gets :: Console :<: f => ProgT f g IO String
gets = undefined
-- gets = injectP (Gets' Var)

pattern Puts s k <- (prj -> Just (Puts' s k))
puts :: Console :<: f => String -> ProgT f g IO ()
puts = undefined
-- puts s = injectP (Puts' s (Var ()))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

type Carrier f g a = CarrierId (ProgT f g IO a)

gen :: (Functor f, Functor g) => a -> IO (CarrierId (Prog f g a) 'Z)
gen x = return (Id (return x))

alg :: (Functor f, Functor g) => AlgT (Console :+: f) g IO (CarrierId (Prog f g a))
alg = T.A a d p where
    a :: (Functor f, Functor g) => (Console :+: f) (IO (CarrierId (Prog f g a) n)) -> IO (CarrierId (Prog f g a) n)
    a (Gets fk) = do
        s <- getLine
        fk s

    a (Puts s k) = do
        putStrLn s
        k

    a (Other op) = undefined

    d :: g (IO (CarrierId (Prog f g a) ('S n))) -> IO (CarrierId (Prog f g a) n)
    d = undefined

    p :: IO (CarrierId (Prog f g a) n) -> IO (CarrierId (Prog f g a) ('S n))
    p = undefined

handleConsole :: (Functor f, Functor g) => ProgT (Console :+: f) g IO a -> IO (Prog f g a)
handleConsole prog = do
    x <- T.run gen alg prog
    return (unId x)

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

ex1 :: ProgT (Console :+: Void) Void IO ()
ex1 = do
    undefined

runEx :: ProgT (Console :+: Void) Void IO () -> IO ()
runEx prog = do
    x <- handleConsole prog
    return (handleVoid x)
