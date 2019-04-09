
-- DSL for WebAssembly. Respresented using Prog to allow scoping structures
-- such as if-else, loop, and block. Also uses Datatypes a la Carte to allow
-- for language to be extended. This is important as WASM is in MVP and has
-- new features scheduled.

{-# LANGUAGE DeriveFunctor, TypeOperators #-}

module Back.WASM
( LocalName
, GlobalName
, FuncName
, ExportName
, MemOffset
, Label
, UniOp(..)
, BinOp(..)
, RelOp(..)
, WASM
, DoesRet
, Func(..)
, Mutability(..)
, Global(..)
, Memory(..)
, ExportType(..)
, Export(..)
, Module(..)
, nop
, constNum
, uniOp
, binOp
, relOp
, getLocal
, setLocal
, getGlobal
, setGlobal
, load
, store
, br
, brIf
, ret
, call
, block
, ifElse
, loop
) where

import Data.Word (Word32)
import Helper.Scope.Prog
import Helper.Co

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

-- Index of local variable in current function.
type LocalName = String
-- Index of global variable.
type GlobalName = String
-- Used to reference a function for calling.
type FuncName = String
-- Used to reference memory, e.g. to export it.
type MemName = String
-- Used to export functions, memory, etc
type ExportName = String

-- Offset from base memory location for loads or stores.
type MemOffset = Word32

-- Used to branch to a location in code.
type Label = Word32

data UniOp = NOT                   deriving Show
data BinOp = ADD | SUB | MUL | AND deriving Show
data RelOp = EQU | LEQ             deriving Show

data Instr k
    -- Do nothing.
    = NOP k
    deriving Functor

-- Arithmetic instructions.
data ArithInstr k
    -- Push constant onto stack.
    = CONST Integer k
    -- Perform operation with items on top of stack.
    | BIN_OP BinOp k
    | REL_OP RelOp k
    deriving Functor

-- Instructions for getting/setting variables.
data VarInstr k
    -- Push local variable onto stack.
    = GET_LOCAL LocalName k
    -- Pop stack and set local variable to popped value.
    | SET_LOCAL LocalName k
    -- Push global variable onto stack.
    | GET_GLOBAL GlobalName k
    -- Pop stack and set global variable to popped value.
    | SET_GLOBAL GlobalName k
    deriving Functor

-- Instructions for interacting with memory.
data MemInstr k
    -- Pop value from stack to use as base address. Load value from base+offset
    -- and push to stack.
    = LOAD MemOffset k
    -- Pop value from stack to use as base address. Pop another value to store.
    -- Store value at base+offset.
    | STORE MemOffset k
    deriving Functor

-- Instructions for changing control flow.
data BranchInstr k
    -- Unconditional branch, the semantics of which depends on the branch location.
    = BR Label k
    -- Branch conditionally on popped value. If non-zero then take branch,
    -- otherwise continue execution flow.
    | BR_IF Label k
    -- Call function
    | CALL FuncName k
    -- Return value on top of stack from function.
    | RET k
    deriving Functor

-- WASM control structures with scope.
data Control k
    -- Block of instructions. Branching to block from inside will jump forward
    -- to the end of the block.
    = BLOCK k
    -- Performs then or else branch depending on whether value popped from stack
    -- is non-zero or zero, respectively. Branching to the if-statement from
    -- inside will jump forward out of either branch.
    | IF k k
    -- Performs loop body. Branching to the loop from inside will jump back
    -- to the start of the loop body. Therefore, couple with BR and BR_IF for
    -- standard loop semantics.
    | LOOP k
    deriving Functor

type Op     = Instr :+: ArithInstr :+: VarInstr :+: MemInstr :+: BranchInstr
type WASM a = Prog Op Control a

type DoesRet = Bool

data Func = Func {
    name    :: FuncName
  , doesRet :: DoesRet
  , locals  :: [LocalName]
  , params  :: [LocalName]
  , body    :: WASM ()
}

data Mutability
    = Mut
    deriving (Eq, Show)

-- Global variables available to all functions, with an initial value.
data Global = Global GlobalName Mutability Integer

-- TODO: Check this is min addr
type MinAddr = Word32
data Memory = Memory MemName MinAddr

data ExportType
    = ExportFunc FuncName
    | ExportMem MemName

data Export = Export ExportName ExportType

data Module = Module {
    funcs    :: [Func]
  , globals  :: [Global]
  , memories :: [Memory]
  , exports  :: [Export]
}

-- Smart constructors

nop :: WASM ()
nop = injectP (NOP (Var ()))

constNum :: Integer -> WASM ()
constNum i = injectP (CONST i (Var ()))

uniOp :: UniOp -> WASM ()
uniOp NOT = do constNum 1; relOp EQU

binOp :: BinOp -> WASM ()
binOp op = injectP (BIN_OP op (Var ()))

relOp :: RelOp -> WASM ()
relOp op = injectP (REL_OP op (Var ()))

getLocal :: LocalName -> WASM ()
getLocal name = injectP (GET_LOCAL name (Var ()))

setLocal :: LocalName -> WASM ()
setLocal name = injectP (SET_LOCAL name (Var ()))

getGlobal :: GlobalName -> WASM ()
getGlobal name = injectP (GET_GLOBAL name (Var ()))

setGlobal :: GlobalName -> WASM ()
setGlobal name = injectP (SET_GLOBAL name (Var ()))

load :: MemOffset -> WASM ()
load offset = injectP (LOAD offset (Var ()))

store :: MemOffset -> WASM ()
store offset = injectP (STORE offset (Var ()))

br :: Label -> WASM ()
br label = injectP (BR label (Var ()))

brIf :: Label -> WASM ()
brIf label = injectP (BR_IF label (Var ()))

ret :: WASM ()
ret = injectP (RET (Var ()))

call :: FuncName -> WASM ()
call name = injectP (CALL name (Var ()))

block :: WASM () -> WASM ()
block body = injectPSc (fmap (fmap return) (BLOCK body))

ifElse :: WASM () -> WASM () -> WASM ()
ifElse t e = injectPSc (fmap (fmap return) (IF t e))

loop :: WASM () -> WASM ()
loop body = injectPSc (fmap (fmap return) (LOOP body))
