
-- Conversion from While AST to CodeGen to generate WebAssembly.

{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Back.AST where

import Front.AST
import Back.CodeGen
import Back.WASM
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Void

type Op      = Emit  :+: Void
type Sc      = Block :+: Void
type Carrier = Prog Op Sc ()

instance FreeAlg (VarExp SrcVar) Carrier where
    alg (GetVar v) = undefined
