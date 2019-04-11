
-- Semantics of CodeGen.

{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}

module Back.CodeGenSem where

import Back.CodeGenSyntax
import Helper.Scope.Prog

--------------------------------------------------------------------------------
-- Auxillary Types to help describe semantics
--------------------------------------------------------------------------------

-- WASM instructions are emitted onto the top of a stack of instructions.
-- The WebAssembly on top is the instruction currently being built (and composed
-- using (>>=)) Since CodeGen acts like a writer for instructions, a stack
-- of instructions is used to represent scope. The topmost instruction is
-- therefore the instruction at the innermost scope.

newtype GenState f g = GenState {
    -- Stack of WebAssembly instructions representing instructions at different
    -- scopes. Instructions are emitted become the continuation of the topmost
    -- instruction.
    instrStack :: [Prog f g ()]
}

data Carrier f g n = CG { runCG :: (GenState f g -> (Carrier' f g n, GenState f g)) }

data Carrier' f g :: Nat -> * where
    CZ :: Carrier' f g 'Z
    CS :: (GenState f g -> (Carrier' f g n, GenState f g)) -> Carrier' f g ('S n)

gen :: () -> Carrier f g 'Z
gen _ = CG (\st -> (CZ, st))

alg :: Alg Emit Block (Carrier f g)
alg = A a d p where
    a :: Emit (Carrier f g n) -> Carrier f g n
    a = undefined

    d :: Block (Carrier f g ('S n)) -> Carrier f g n
    d = undefined

    p :: Carrier f g n -> Carrier f g ('S n)
    p = undefined

handleCodeGen :: GenState f g -> Prog Emit Block () -> GenState f g
handleCodeGen st prog = case runCG (run gen alg prog) st of
    (_, st') -> st'
