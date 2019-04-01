
-- Exception effect handler and semantics, i.e. ability to 'throw' an error.
-- Therefore, the return type is wrapped in an Either.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures, StandaloneDeriving #-}

module Helper.Eff.Exception where

import Helper.Prog
import Helper.Co
import Helper.Eff
import Helper.Eff.Void

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

-- TODO
-- Throw an error of type e, and do not perform anything after as execution
-- will either stop (if there is no catch), or jump to the catch handler.
data Throw k
    = Throw' String
    deriving Functor

-- Catch a thrown error of type e.
data Catch k where
    Catch' :: (String -> Prog f g a) -> k -> Catch k

deriving instance Functor Catch

pattern Throw err <- (prj -> Just (Throw' err))
throw :: Throw :<: f => String -> Prog f g a
throw err = inject (Throw' err)

pattern Catch hdl inner <- (prj -> Just (Catch' hdl inner))
catch :: (Functor f, Catch :<: g) => (String -> Prog f g a) -> Prog f g a -> Prog f g a
catch hdl inner = injectS (fmap (fmap return) (Catch' hdl inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- Each time a catch is encountered, a new level of scope is entered and
-- a new handler is added to the top of 'stack'. This is the handler that will
-- be used if an exception is encountered inside the catch.
--
-- Like a Reader, the exception handler is used as the environment
--  Maybe (String -> a) -> m (Either String a)

type ErrHdl a = Maybe (String -> a)
data CarrierExc f g a (n :: Nat)
    = Exc { runExc :: ErrHdl (Prog f g a) -> Prog f g (Either String (CarrierExc' f g a n)) }

data CarrierExc' f g a :: Nat -> * where
    CZ :: a -> CarrierExc' f g a 'Z
    CS :: (ErrHdl (Prog f g a) -> Prog f g (Either String (CarrierExc' f g a n))) -> CarrierExc' f g a ('S n)

genExc :: (Functor f, Functor g) => a -> CarrierExc f g a 'Z
genExc = undefined
-- genExc x = Exc $ \_ -> (return (Right (CZ x)))

algExc :: (Functor f, Functor g) => Alg (Throw :+: f) (Catch :+: g) (CarrierExc f g a)
algExc = A a d p where
    a :: (Functor f, Functor g) => (Throw :+: f) (CarrierExc f g a n) -> CarrierExc f g a n
    -- If an error handler exists, use that to handle the error. Otherwise
    -- simply return the error.
    a (Throw err) = undefined
    a (Other op) = Exc $ \hdl -> (Op (fmap (\(Exc run) -> run hdl) op))

    d :: (Functor f, Functor g) => (Catch :+: g) (CarrierExc f g a ('S n)) -> CarrierExc f g a n
    d (Catch newHdl (Exc runExc)) = Exc $ \hdl -> do
        -- Run prog nested in catch using the exception handler supplied
        -- with the catch.
        x <- runExc (Just newHdl)
        case x of
            -- Execute the rest of the continuation using the old exception handler.
            Right (CS runExc') -> runExc' hdl
            Left err           -> undefined

    d (Other op) = undefined

    p :: (Functor f, Functor g) => CarrierExc f g a n -> CarrierExc f g a ('S n)
    p = undefined

-- Converts any program which might thrown an exception into a program that
-- returns either its result, or an error.
handleExc :: (Functor f, Functor g) => Prog (Throw :+: f) (Catch :+: g) a -> Prog f g (Either String a)
handleExc = undefined
-- handleExc prog = do
--     r <- runExc (run genExc algExc prog) Nothing
--     case r of
--         Left err     -> return (Left err)
--         Right (CZ x) -> return (Right x)

test1 :: Prog (Throw :+: Void) (Catch :+: Void) Int
test1 = do
    throw "Hello"
    return 1

test2 :: Prog (Throw :+: Void) (Catch :+: Void) Int
test2 = do
    catch undefined undefined

runTest :: IO ()
runTest = do
    let r = (handleVoid . handleExc) test2
    putStrLn (show r)
