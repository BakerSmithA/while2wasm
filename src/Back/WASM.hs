
-- DSL for WebAssembly. Respresented using Prog to allow scoping structures
-- such as if-else, loop, and block. Also uses Datatypes a la Carte to allow
-- for language to be extended. This is important as WASM is in MVP and has
-- new features scheduled.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}

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
, Instr(..)
, ArithInstr(..)
, VarInstr(..)
, MemInstr(..)
, BranchInstr(..)
, ControlInstr(..)
, DoesRet
, Func(..)
, Mutability(..)
, Global(..)
, Memory(..)
, ExportType(..)
, Export(..)
, Module(..)
, WASM
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
data ControlInstr k
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

type Op   = Instr :+: ArithInstr :+: VarInstr :+: MemInstr :+: BranchInstr
type Sc   = ControlInstr
type WASM = Prog Op Sc ()

type DoesRet = Bool

data Func = Func {
    name    :: FuncName
  , doesRet :: DoesRet
  , locals  :: [LocalName]
  , params  :: [LocalName]
  , body    :: WASM
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

nop :: Instr :<: f => Prog f g ()
nop = injectP (NOP (Var ()))

constNum :: ArithInstr :<: f => Integer -> Prog f g ()
constNum i = injectP (CONST i (Var ()))

uniOp :: (ArithInstr :<: f, Functor g) => UniOp -> Prog f g ()
uniOp NOT = do constNum 0; relOp EQU

binOp :: ArithInstr :<: f => BinOp -> Prog f g ()
binOp op = injectP (BIN_OP op (Var ()))

relOp :: ArithInstr :<: f => RelOp -> Prog f g ()
relOp op = injectP (REL_OP op (Var ()))

getLocal :: VarInstr :<: f => LocalName -> Prog f g ()
getLocal name = injectP (GET_LOCAL name (Var ()))

setLocal :: VarInstr :<: f => LocalName -> Prog f g ()
setLocal name = injectP (SET_LOCAL name (Var ()))

getGlobal :: VarInstr :<: f => GlobalName -> Prog f g ()
getGlobal name = injectP (GET_GLOBAL name (Var ()))

setGlobal :: VarInstr :<: f => GlobalName -> Prog f g ()
setGlobal name = injectP (SET_GLOBAL name (Var ()))

load :: MemInstr :<: f => MemOffset -> Prog f g ()
load offset = injectP (LOAD offset (Var ()))

store :: MemInstr :<: f => MemOffset -> Prog f g ()
store offset = injectP (STORE offset (Var ()))

br :: BranchInstr :<: f => Label -> Prog f g ()
br label = injectP (BR label (Var ()))

brIf :: BranchInstr :<: f => Label -> Prog f g ()
brIf label = injectP (BR_IF label (Var ()))

ret :: BranchInstr :<: f => Prog f g ()
ret = injectP (RET (Var ()))

call :: BranchInstr :<: f => FuncName -> Prog f g ()
call name = injectP (CALL name (Var ()))

block :: (ControlInstr :<: g, Functor f) => Prog f g () -> Prog f g ()
block body = injectPSc (fmap (fmap return) (BLOCK body))

ifElse ::(ControlInstr :<: g, Functor f) => Prog f g () -> Prog f g () -> Prog f g ()
ifElse t e = injectPSc (fmap (fmap return) (IF t e))

loop :: (ControlInstr :<: g, Functor f) => Prog f g () -> Prog f g ()
loop body = injectPSc (fmap (fmap return) (LOOP body))
