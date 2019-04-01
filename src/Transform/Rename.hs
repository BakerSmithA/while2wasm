
-- Renames variables in AST to be represented using unique numbers instead of
-- strings.

{-# LANGUAGE FlexibleContexts #-}

module Transform.Rename
( Fresh(..)
, rename
) where

import Transform.Rename.AST_01
import Transform.Rename.RenameEff
import Helper.Prog
import Helper.Alg
import Helper.Eff.Void

-- Use Datatypes a la Carte to convert AST to handler, the types in the AST
-- are not the same before and after, indicated by the change from Prog f g to Prog h i.
make :: (Functor h, Functor i)
     => (OpAlg f (Carrier h i), ScopeAlg g (Carrier h i))
     => Prog f g () -> RenameHandler h i
make = evalId gen where
    gen x = Id (return (return x))

-- Use extensible effects methods to run handlers.
handle :: RenameHandler h i -> Prog h i ()
handle = handleVoid . handleRename

rename :: (Functor h, Functor i)
       => (OpAlg f (Carrier h i), ScopeAlg g (Carrier h i))
       => Prog f g () -> Prog h i ()
rename = handle . make
