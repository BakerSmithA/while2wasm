
-- Semantics of CodeGen.

{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}

module Back.CodeGenSem
( handleCodeGen
) where

import Back.WASM
import Back.CodeGenSyntax
import Helper.Scope.Prog

handleCodeGen :: Prog GenData Function WASM -> (WASM, [Func])
handleCodeGen = undefined

-- handleCodeGen prog = case runCG (run gen alg prog) emptyGenEnv of
--     (_, env) -> peekBlock (blocks env)
