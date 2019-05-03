
-- Exception effect handler and semantics, i.e. ability to 'throw' an error.
-- Therefore, the return type is wrapped in an Either.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}

module Helper.Eff.Exception
( Throw
, Catch
, throw
, catch
, handleExc
) where

import Helper.Scope.Prog
import Helper.Co
import Helper.Inj
import Helper.Eff
import Helper.Eff.Void

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

-- Throw an error of type e, and do not perform anything after as execution
-- will either stop (if there is no catch), or jump to the catch handler.
data Throw e k
    = Throw' e
    deriving Functor

-- Catch a thrown error of type e.
data Catch e k
    = Catch' (e -> k) k
    deriving Functor

pattern Throw err <- (prj -> Just (Throw' err))
throw :: Throw e :<: f => e -> Prog f g a
throw err = injectP (Throw' err)

pattern Catch hdl k <- (prj -> Just (Catch' hdl k))
catch :: (Functor f, Catch e :<: g) => (e -> Prog f g a) -> Prog f g a -> Prog f g a
catch hdl k = injectPSc (fmap (fmap return) (Catch' hdl k))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

data CarrierExc f g e a (n :: Nat)
    = Exc { runExc :: Prog f g (Either e (CarrierExc' f g e a n)) }

data CarrierExc' f g e a :: Nat -> * where
    CZ :: a -> CarrierExc' f g e a 'Z
    CS :: (Prog f g (Either e (CarrierExc' f g e a n))) -> CarrierExc' f g e a ('S n)

genExc :: (Functor f, Functor g) => a -> CarrierExc f g e a 'Z
genExc x = Exc (return (Right (CZ x)))

algExc :: (Functor f, Functor g) => Alg (Throw e :+: f) (Catch e :+: g) (CarrierExc f g e a)
algExc = A a d p where
    a :: (Functor f, Functor g) => (Throw e :+: f) (CarrierExc f g e a n) -> CarrierExc f g e a n
    -- Don't perform continuation if an error is thrown.
    a (Throw err) = Exc (return (Left err))
    a (Other op)  = Exc (Op (fmap runExc op))

    d :: (Functor f, Functor g) => (Catch e :+: g) (CarrierExc f g e a ('S n)) -> CarrierExc f g e a n
    d (Catch hdl k) = Exc $ do
        x <- runExc k
        case x of
            Right (CS k') -> k'
            Left err -> do
                r <- runExc (hdl err)
                case r of
                    Right (CS k') -> k'
                    Left err      -> return (Left err)

    d (Other op)  = Exc (Scope (fmap f op)) where
        f :: (Functor f, Functor g) => CarrierExc f g e a ('S n) -> Prog f g (Prog f g (Either e (CarrierExc' f g e a n)))
        f (Exc runExc) = do
            r <- runExc
            case r of
                Left err     -> return (return (Left err))
                Right (CS x) -> return x

    p :: (Functor f, Functor g) => CarrierExc f g e a n -> CarrierExc f g e a ('S n)
    p (Exc runExc) = Exc $ do
        r <- runExc
        case r of
            Left err -> return (Left err)
            Right x  -> return (Right (CS (return (Right x))))

-- Converts any program which might thrown an exception into a program that
-- returns either its result, or an error.
handleExc :: (Functor f, Functor g) => Prog (Throw e :+: f) (Catch e :+: g) a -> Prog f g (Either e a)
handleExc prog = do
    r <- runExc (run genExc algExc prog)
    case r of
        Left err     -> return (Left err)
        Right (CZ x) -> return (Right x)
