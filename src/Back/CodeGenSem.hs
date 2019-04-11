
-- Semantics of CodeGen.

{-# LANGUAGE GADTs, DataKinds, KindSignatures #-}

module Back.CodeGenSem
( FuncMeta(..)
, GenEnv(..)
, handleCodeGen
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Back.CodeGenSyntax
import Helper.Scope.Prog

--------------------------------------------------------------------------------
-- Auxillary BlockStack
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
type BlockStack = [WASM]

-- Set continuation of block on top of block-stack to be wasm.
appendInstr :: WASM -> BlockStack -> BlockStack
appendInstr wasm []           = [wasm]
appendInstr wasm (block:rest) = (block >> wasm):rest

-- Return block on top of stack without popping it.
peekBlock :: BlockStack -> WASM
peekBlock []       = error "No blocks on stack"
peekBlock (wasm:_) = wasm

-- Remove block from top of stack.
popBlock ::  BlockStack -> BlockStack
popBlock []       = error "No blocks on stack"
popBlock (_:rest) = rest

-- Create a new block of instructions of top of stack.
pushBlock :: WASM -> BlockStack -> BlockStack
pushBlock wasm blocks = wasm:blocks

--------------------------------------------------------------------------------
-- Auxillary Env
--------------------------------------------------------------------------------

-- Meta data about the function currently being compiled.
data FuncMeta = FuncMeta {
    localVars :: Set SrcVar
  , paramVars :: Set SrcVar
}

-- Global state regarding compilation.
data GenEnv = GenEnv {
    blocks    :: BlockStack
  , dirtyVars :: Set SrcVar
}

emptyGenEnv :: GenEnv
emptyGenEnv = GenEnv [] Set.empty

modifyBlockStack :: (BlockStack -> BlockStack) -> GenEnv -> GenEnv
modifyBlockStack f env = env { blocks = f (blocks env) }

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

data Carrier n = CG { runCG :: (GenEnv -> (Carrier' n, GenEnv)) }

data Carrier' :: Nat -> * where
    CZ :: Carrier' 'Z
    CS :: (GenEnv -> (Carrier' n, GenEnv)) -> Carrier' ('S n)

gen :: () -> Carrier 'Z
gen _ = CG (\st -> (CZ, st))

alg :: Alg Emit Block Carrier
alg = A a d p where
    a :: Emit (Carrier n) -> Carrier n
    a (Emit wasm k)  = CG $ \env -> runCG k (modifyBlockStack (appendInstr wasm) env)
    a (VarType v fk) = CG $ \env -> runCG (fk (Local (Val v))) env
    a _ = undefined

    d :: Block (Carrier ('S n)) -> Carrier n
    d = undefined

    p :: Carrier n -> Carrier ('S n)
    p = undefined

handleCodeGen :: Prog Emit Block () -> WASM
handleCodeGen prog = case runCG (run gen alg prog) emptyGenEnv of
    (_, env) -> peekBlock (blocks env)
