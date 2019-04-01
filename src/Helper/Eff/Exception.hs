
-- Exception effect handler and semantics, i.e. ability to 'throw' an error.
-- Therefore, the return type is wrapped in an Either.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Helper.Eff.Exception where

import Helper.Prog
import Helper.Co

-- Throw an error of type e.
data Throw e k
    = Throw' e k
    deriving Functor

pattern Throw e k <- (prj -> Just (Throw' e k))
throw :: Throw e :<: f => e -> Prog f g ()
throw err = inject (Throw' err (Var ()))
