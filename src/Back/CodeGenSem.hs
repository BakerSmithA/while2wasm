
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

-- Terminology:
--  Block      : Many WebAssembly instructions composed together using (>>=)
--  BlockStack : Blocks in a stack, where top block represents innermost scope.

-- Stack of WebAssembly instructions representing instructions at different
-- scopes. Instructions are emitted become the continuation of the topmost
-- instruction.
type BlockStack f g = [Prog f g ()]

-- Set continuation of block on top of block-stack to be wasm.
appendInstr :: (Functor f, Functor g) => Prog f g () -> BlockStack f g -> BlockStack f g
appendInstr wasm []           = [wasm]
appendInstr wasm (block:rest) = (block >> wasm):rest

-- Return block on top of stack without popping it.
peekBlock :: BlockStack f g -> Prog f g ()
peekBlock []       = error "No blocks on stack"
peekBlock (wasm:_) = wasm

-- Remove block from top of stack.
popBlock ::  BlockStack f g -> BlockStack f g
popBlock []       = error "No blocks on stack"
popBlock (_:rest) = rest

-- Create a new block of instructions of top of stack.
pushBlock :: Prog f g () -> BlockStack f g -> BlockStack f g
pushBlock wasm blocks = wasm:blocks

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

data Carrier f g n = CG { runCG :: (BlockStack f g -> (Carrier' f g n, BlockStack f g)) }

data Carrier' f g :: Nat -> * where
    CZ :: Carrier' f g 'Z
    CS :: (BlockStack f g -> (Carrier' f g n, BlockStack f g)) -> Carrier' f g ('S n)

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

handleCodeGen :: BlockStack f g -> Prog Emit Block () -> BlockStack f g
handleCodeGen st prog = case runCG (run gen alg prog) st of
    (_, st') -> st'
