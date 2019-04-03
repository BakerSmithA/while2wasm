
-- Renames variables in AST to be represented using unique numbers instead of
-- strings.

{-# LANGUAGE FlexibleContexts #-}

module Transform.Rename
( FreshName(..)
, rename
) where

import Transform.Rename.AST
import Transform.Rename.RenameEff
import Helper.Prog
import Helper.Alg
import Helper.Eff.Void
import Helper.Eff.Exception

-- Use extensible effects methods to run handlers.
handle :: RenameHandler h i -> Either String (Prog h i ())
handle = handleVoid . handleRename . handleRename . handleExc

rename :: (Functor h, Functor i)
       => (OpAlg f (Carrier h i), ScopeAlg g (Carrier h i))
       => Prog f g () -> Either String (Prog h i ())
rename = handle . makeRn
