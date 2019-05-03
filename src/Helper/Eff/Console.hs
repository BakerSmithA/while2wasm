
-- Console effects, i.e. get-line, put-line

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, GADTs #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms, DataKinds, KindSignatures #-}

module Helper.Eff.Console where

import Helper.Scope.Prog
import Helper.Scope.ProgT (ProgT)
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
gets :: Console :<: f => Prog f g String
gets = injectP (Gets' Var)

pattern Puts s k <- (prj -> Just (Puts' s k))
puts :: Console :<: f => String -> Prog f g ()
puts s = injectP (Puts' s (Var ()))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

data Carrier f g a n = C { runC :: ProgT f g IO (Carrier' f g a n) }

data Carrier' f g a :: Nat -> * where
    CZ :: a -> Carrier' f g a 'Z
    CS :: ProgT f g IO (Carrier' f g a n) -> Carrier' f g a ('S n)

handleConsole :: ProgT (Console :+: f) g IO a -> IO (Carrier f g a 'Z)
handleConsole prog = T.run undefined undefined prog

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
