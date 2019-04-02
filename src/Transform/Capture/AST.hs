
-- Maps from AST to effect handler to produce map of variable names to
-- whether they should be stored as a value or pointer.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

module Transform.Capture.AST where

import Front.AST
import Transform.Capture.CaptureEff
import Helper.Alg

type Carrier v = CarrierId (Capture v)

instance Ord v => OpAlg (VarExp v) (Carrier v) where
    alg = undefined
