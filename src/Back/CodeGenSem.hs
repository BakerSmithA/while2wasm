
-- Semantics of CodeGen.

{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}

module Back.CodeGenSem
( handleCodeGen
) where

import Back.WASM
import Back.CodeGenSyntax
import Helper.Scope.Prog

data Env = Env

data Carrier n = CG { runCG :: Env -> (Carrier' n, Env) }

data Carrier' :: Nat -> * where
    CZ :: WASM -> Carrier' 'Z
    CS :: (Env -> (Carrier' n, Env)) -> Carrier' ('S n)

gen :: WASM -> Carrier 'Z
gen wasm = CG (\env -> (CZ wasm, env))

alg :: Alg GenData Function Carrier
alg = A a d p where
    a :: GenData (Carrier n) -> Carrier n
    a = undefined

    d :: Function (Carrier ('S n)) -> Carrier n
    d = undefined

    p :: Carrier n -> Carrier ('S n)
    p = undefined

handleCodeGen :: Prog GenData Function WASM -> (WASM, [Func])
handleCodeGen prog = case runCG (run gen alg prog) Env of
    (CZ wasm, env) -> (wasm, [])
