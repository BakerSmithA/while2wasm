
{-# LANGUAGE DeriveFunctor #-}

module Helper.Eff.File where

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data File k
    = Contents (String -> k)
    deriving Functor

data Open k
    = Open (String -> k)
    deriving Functor

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------
