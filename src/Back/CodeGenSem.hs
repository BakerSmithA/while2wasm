
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
import Back.WASM
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
    blocks        :: BlockStack
  , completeFuncs :: [Func]
  , dirtyVars     :: Set SrcVar
}

emptyGenEnv :: GenEnv
emptyGenEnv = GenEnv [] [] Set.empty

modifyBlockStack :: (BlockStack -> BlockStack) -> GenEnv -> GenEnv
modifyBlockStack f env = env { blocks = f (blocks env) }

makeFunc :: SrcProc -> DoesRet -> SrcLocalVars -> SrcParamVars -> GenEnv -> GenEnv
makeFunc pname ret locals params env = env' where
    env'           = env { completeFuncs=completeFuncs', blocks=rest }
    completeFuncs' = func:(completeFuncs env)
    func           = Func (wasmName pname) ret locals' params' body
    locals'        = map wasmName (Set.elems locals)
    params'        = map wasmName (Set.elems params)
    (body:rest)    = blocks env

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
    a (CurrInstr fk) = CG $ \env -> runCG (fk (peekBlock (blocks env))) env
    a (VarType v fk) = CG $ \env -> runCG (fk (Local (Val v))) env

    d :: Block (Carrier ('S n)) -> Carrier n
    d (CodeBlock k) = CG $ \env ->
        let env' = modifyBlockStack (pushBlock (return ())) env
        in case runCG k env' of
            (CS runK, env'') -> runK (modifyBlockStack popBlock env'')

    d (Function pname ret locals params spOffsets body) = CG $ \env ->
        let env' = modifyBlockStack (pushBlock (return ())) env
        in case runCG body env' of
            (CS runK, env'') -> runK (makeFunc pname ret locals params env'')

    p :: Carrier n -> Carrier ('S n)
    p (CG runCG) = CG $ \env -> (CS runCG, env)

handleCodeGen :: Prog Emit Block () -> (WASM, [Func])
handleCodeGen prog = case runCG (run gen alg prog) emptyGenEnv of
    (_, env) -> (peekBlock (blocks env), completeFuncs env)
