
-- Renames variables in AST to be represented using unique numbers instead of
-- strings.

{-# LANGUAGE FlexibleContexts #-}

module Transform.Rename where

import Transform.Rename.AST_01
import Transform.Rename.RenameEff
import Helper.Prog
import Helper.Alg
import Helper.Eff.Void

-- Use Datatypes a la Carte to convert AST to handler.
make :: (OpAlg f (Carrier f g), ScopeAlg g (Carrier f g)) => Prog f g () -> RenameHandler f g
make = evalId gen where
    gen x = Id (return (return x))

-- Use extensible effects methods to run handlers.
handle :: RenameHandler f g -> Prog f g ()
handle = handleVoid . handleRename

rename :: (OpAlg f (Carrier f g), ScopeAlg g (Carrier f g)) => Prog f g () -> Prog f g ()
rename = handle . make
