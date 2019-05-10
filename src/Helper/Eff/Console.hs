
-- Console effects, i.e. get-line, put-line

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, GADTs #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms, DataKinds, KindSignatures #-}

module Helper.Eff.Console where

import Helper.Scope.Prog
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
gets :: Console :<: f => Prog f g String
gets = injectP (Gets' Var)

pattern Puts s k <- (prj -> Just (Puts' s k))
puts :: Console :<: f => String -> Prog f g ()
puts s = injectP (Puts' s (Var ()))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

data Carrier f g a :: Nat -> * where
    CZ :: Prog f g a -> Carrier f g a 'Z
    CS :: Prog f g (IO (Carrier f g a n)) -> Carrier f g a ('S n)
    CN :: Prog f g (IO (Carrier f g a n)) -> Carrier f g a n

gen :: a -> Carrier f g a 'Z
gen = undefined

alg :: (Functor f, Functor g) => Alg (Console :+: f) g (Carrier f g a)
alg = A a undefined undefined where
    a :: (Functor f, Functor g) => (Console :+: f) (Carrier f g a n) -> Carrier f g a n
    a (Gets fk) = CN $ return $ do
        s <- getLine
        return (fk s)

handleConsole :: (Functor f, Functor g) => Prog (Console :+: f) g a -> Prog f g (IO a)
handleConsole prog =
    case run gen alg prog of
        CZ p -> fmap return p

-- data Carrier f g a n = C { runC :: Prog f g (IO (Carrier' f g a n)) }
--
-- data Carrier' f g a :: Nat -> * where
--     CZ :: a -> Carrier' f g a 'Z
--     CS :: Prog f g (IO (Carrier' f g a n)) -> Carrier' f g a ('S n)
--
-- gen :: a -> Carrier f g a 'Z
-- gen = undefined
--
-- alg :: Alg (Console :+: f) g (Carrier f g a)
-- alg = A a undefined undefined where
--     a :: (Console :+: f) (Carrier f g a n) -> Carrier f g a n
--     a (Gets fk) = C $ return $ do
--         s <- getLine
--         _
--
-- handleConsole :: (Functor f, Functor g) => Prog (Console :+: f) g a -> Prog f g (IO a)
-- handleConsole prog =
--     case run gen alg prog of
--         C x -> fmap unZ x where
--             unZ :: IO (Carrier' f g a 'Z) -> IO a
--             unZ y = do
--                 CZ y' <- y
--                 return y'
