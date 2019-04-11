
-- Semantics of CodeGen.

{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}

module Back.CodeGenSem
( handleCodeGen
) where

import Back.WASM hiding (funcs)
import Back.CodeGenSyntax
import Helper.Scope.Prog

data Env = Env {
    funcs :: [Func]
}

emptyEnv :: Env
emptyEnv = Env []

data Carrier n = CG { runCG :: Env -> (Carrier' n, Env) }

data Carrier' :: Nat -> * where
    CZ :: WASM -> Carrier' 'Z
    CS :: (Env -> (Carrier' n, Env)) -> Carrier' ('S n)

gen :: WASM -> Carrier 'Z
gen wasm = CG (\env -> (CZ wasm, env))

alg :: Alg GenData Function Carrier
alg = A a d p where
    a :: GenData (Carrier n) -> Carrier n
    a (VarType v fk) = CG $ \env -> runCG (fk (Local (Val v))) env

    d :: Function (Carrier ('S n)) -> Carrier n
    d (Function pname ret locals params spOffsets body) = CG $ \env ->
        case runCG body env of
            (CS runK, env') -> undefined

    p :: Carrier n -> Carrier ('S n)
    p = undefined

handleCodeGen :: Prog GenData Function WASM -> (WASM, [Func])
handleCodeGen prog = case runCG (run gen alg prog) emptyEnv of
    (CZ wasm, env) -> (wasm, funcs env)
