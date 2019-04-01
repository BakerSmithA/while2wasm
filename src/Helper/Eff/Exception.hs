
-- Exception effect handler and semantics, i.e. ability to 'throw' an error.
-- Therefore, the return type is wrapped in an Either.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE DataKinds, GADTs, KindSignatures #-}

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
data Catch k
    = Catch' (String -> k) k
    deriving Functor

pattern Throw err <- (prj -> Just (Throw' err))
throw :: Throw :<: f => String -> Prog f g a
throw err = inject (Throw' err)

pattern Catch hdl k <- (prj -> Just (Catch' hdl k))
catch :: (Functor f, Catch :<: g) => (String -> Prog f g ()) -> Prog f g ()
catch hdl = injectS (fmap (fmap return) (Catch' hdl (Var ())))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- Each time a catch is encountered, a new level of scope is entered and
-- a new handler is added to the top of 'stack'. This is the handler that will
-- be used if an exception is encountered inside the catch.

type ErrHdl a = Maybe (String -> a)
data CarrierExc f g a (n :: Nat)
    = Exc { runExc :: ErrHdl (CarrierExc' f g a n) -> Prog f g (Either String (CarrierExc' f g a n)) }

data CarrierExc' f g a :: Nat -> * where
    CZ :: a -> CarrierExc' f g a 'Z
    CS :: (ErrHdl (CarrierExc' f g a n) -> Prog f g (Either String a)) -> CarrierExc' f g a ('S n)

genExc :: (Functor f, Functor g) => a -> CarrierExc f g a 'Z
genExc x = Exc $ \_ -> (return (Right (CZ x)))

algExc :: (Functor f, Functor g) => Alg (Throw :+: f) (Catch :+: g) (CarrierExc f g a)
algExc = A a d p where
    a :: (Functor f, Functor g) => (Throw :+: f) (CarrierExc f g a n) -> CarrierExc f g a n
    -- If an error handler exists, use that to handle the error. Otherwise
    -- simply return the error.
    a (Throw err) = Exc $ \hdl ->
        case hdl of
            Nothing -> return (Left err)
            Just h  -> return (Right (h err))
    a (Other op) = Exc $ \hdl -> (Op (fmap (\(Exc run) -> run hdl) op))

    d :: (Functor f, Functor g) => (Catch :+: g) (CarrierExc f g a ('S n)) -> CarrierExc f g a n
    d (Catch newHdl k) = Exc $ \hdl -> do
        -- Run continuation with new handler supplied by catch block.
        r <- runExc k (h newHdl)
        -- Continue after catch using old handler.
        either undefined (go hdl) r where
            go  :: (Functor f, Functor g) => ErrHdl (CarrierExc' f g a n) -> CarrierExc' f g a ('S n) -> Prog f g (Either String (CarrierExc' f g a n))
            go hdl (CS runExc') = do
                x <- runExc' hdl
                case x of
                    Left err -> undefined
                    Right y  -> return (Right _)

            --  _ :: Prog f2 g2 (Either String (CarrierExc' f2 g2 a2 n1))
            --  runExc' :: ErrHdl (CarrierExc' f2 g2 a2 n2) -> Prog f2 g2 (Either String a2)

    d (Other op)  = undefined

    p :: CarrierExc f g a n -> CarrierExc f g a ('S n)
    p = undefined

    h :: (String -> CarrierExc f g a ('S n)) -> ErrHdl (CarrierExc' f g a ('S n))
    h = undefined

-- Converts any program which might thrown an exception into a program that
-- returns either its result, or an error.
handleExc :: (Functor f, Functor g) => Prog (Throw :+: f) (Catch :+: g) a -> Prog f g (Either String a)
handleExc prog = do
    r <- runExc (run genExc algExc prog) Nothing
    case r of
        Left err     -> return (Left err)
        Right (CZ x) -> return (Right x)

test :: Prog (Throw :+: Void) (Catch :+: Void) Int
test = do
    throw "Hello"
    return 1

runTest :: IO ()
runTest = do
    let r = (handleVoid . handleExc) test
    putStrLn (show r)
