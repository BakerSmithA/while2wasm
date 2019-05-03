
-- Console effects, i.e. get-line, put-line

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, GADTs #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms, DataKinds #-}

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
