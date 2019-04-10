
-- Generating WebAssembly from processed While.
-- Demonstrates utility of scoping when compiling to a language which has
-- scoped constructs.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts, GADTs, DataKinds #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Back.CodeGen
( SrcVar
, SrcProc
, SrcLocalVars
, SrcParamVars
, LocType(..)
, ValType(..)
, Emit
, Block
, wasmName
, emit
, spName
, varSPOffset
, funcVarLocations
, callerScopeFuncParams
, varType
, codeBlock
, function
, emitGetVarAsArg
, emitGetVarVal
, emitSetVarVal
) where

import Data.Set (Set)
import Data.Map (Map)
import Transform.Rename.Rename (FreshName)
import Back.WASM
import Helper.Scope.Prog
import Helper.Scope.Nest
import Helper.Co
import Helper.Eff.State

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

-- Type of variable from source language.
type SrcVar  = FreshName
-- Type of procedure from source language.
type SrcProc = FreshName

-- Variables local to a function.
type SrcLocalVars = [SrcVar]
-- Variables passed into a function.
type SrcParamVars = [SrcVar]

-- Whether variable is stored as a value local to a function, or is passed in
-- as a parameter.
data LocType v
    = Local v
    | Param v
    deriving (Eq, Show, Functor)

-- Whether variable the value of a variable is stored in a local variable, or
-- on the stack.
data ValType v
    = Val v
    | Ptr v
    deriving (Eq, Show, Functor)

data Emit k
    -- Append an instruction to the end of the current block of current function.
    = Emit' WASM k
    -- Get instructions of innermost scoped block. This is the WebAssembly being
    -- constructed.
    | CurrInstr' (WASM -> k)
    -- Get name of stack pointer, which can be used to load variables from
    -- function's stack frame.
    | SPName' (GlobalName -> k)
    -- Get offset of a local variable from the stack pointer.
    | VarSPOffset' SrcVar (Word -> k)
    -- Get local variables and parameters to a function. Used when creating
    -- function definition.
    | FuncVarLocations' SrcProc ((SrcLocalVars, SrcParamVars) -> k)
    -- Returns names of arguments to a function, i.e. the variables in
    -- the **caller** scope that should be pushed onto the stack before calling
    -- the function.
    | CallerScopeFuncParams' SrcProc (SrcParamVars -> k)
    -- Returns the type of a variable, in the current function, given its name.
    -- Used to inform how the variable should be accessed.
    | VarType' SrcVar (LocType (ValType SrcVar) -> k)
    deriving Functor

data Block k
    -- Create a new block of instructions that nested emits will append to.
    -- Once scope is exited, this block will be popped from the stack.
    = Block' k
    -- Create a new block of instructions that nested emits will append to.
    -- Once scope is exited, the instructions will be placed in a function.
    | Function' SrcProc DoesRet k
    deriving Functor

pattern Emit instr k <- (prj -> Just (Emit' instr k))
emit :: Emit :<: f => WASM -> Prog f g ()
emit i = injectP (Emit' i (Var ()))

pattern CurrInstr fk <- (prj -> Just (CurrInstr' fk))
currInstr :: Emit :<: f => Prog f g WASM
currInstr = injectP (CurrInstr' Var)

pattern SPName fk <- (prj -> Just (SPName' fk))
spName :: Emit :<: f => Prog f g GlobalName
spName = injectP (SPName' Var)

pattern VarSPOffset var fk <- (prj -> (Just (VarSPOffset' var fk)))
varSPOffset :: Emit :<: f => SrcVar -> Prog f g Word
varSPOffset v = injectP (VarSPOffset' v Var)

pattern FuncVarLocations pname fk <- (prj -> Just (FuncVarLocations' pname fk))
funcVarLocations :: Emit :<: f => SrcProc -> Prog f g (SrcLocalVars, SrcParamVars)
funcVarLocations pname = injectP (FuncVarLocations' pname Var)

pattern CallerScopeFuncParams pname fk <- (prj -> Just (CallerScopeFuncParams' pname fk))
callerScopeFuncParams :: Emit :<: f => SrcProc -> Prog f g [SrcVar]
callerScopeFuncParams pname = injectP (CallerScopeFuncParams' pname Var)

pattern VarType var fk <- (prj -> Just (VarType' var fk))
varType :: Emit :<: f => SrcVar -> Prog f g (LocType (ValType SrcVar))
varType v = injectP (VarType' v Var)

-- Returns instructions emitted in the block allowing them to be wrapped up
-- in a WASM control structure, e.g. BLOCK.
pattern Block k <- (prj -> Just (Block' k))
codeBlock :: (Functor f, Emit :<: f, Block :<: g) => Prog f g () -> Prog f g WASM
codeBlock inner = injectPSc (fmap (fmap return) (Block' (do inner; currInstr)))

-- Emits nested instructions to a new function.
pattern Function fname doesRet body <- (prj -> Just (Function' fname doesRet body))
function :: (Functor f, Block :<: g) => SrcProc -> DoesRet -> Prog f g a -> Prog f g a
function name ret body = injectPSc (fmap (fmap return) (Function' name ret body))

--------------------------------------------------------------------------------
-- Convenience functions to make converting from While easier
--------------------------------------------------------------------------------

-- Converts source-code name into name that can be emitted into WASM.
wasmName :: SrcVar -> LocalName
wasmName = show

-- Emit instructions to push an argument to another function onto the stack.
-- I.e. address of a pointer, or value of a value.
emitGetVarAsArg :: (Functor g, Emit :<: f) => LocType (ValType (SrcVar)) -> Prog f g ()
emitGetVarAsArg (Local (Val v)) = emit (getLocal (wasmName v))
emitGetVarAsArg (Param (Val v)) = emit (getLocal (wasmName v))
emitGetVarAsArg (Local (Ptr v)) = emitLocalPtrAddr v
emitGetVarAsArg (Param (Ptr v)) = emit (getLocal (wasmName v))

-- Emit instuction to push the value of a variable onto the stack.
-- I.e. deference a pointer, or value of a value.
emitGetVarVal :: (Functor g, Emit :<: f) => LocType (ValType SrcVar) -> Prog f g ()
emitGetVarVal (Local (Val v)) = emit (getLocal (wasmName v))
emitGetVarVal (Param (Val v)) = emit (getLocal (wasmName v))
emitGetVarVal (Local (Ptr v)) = do emitLocalPtrAddr v; emit (load 0)
emitGetVarVal (Param (Ptr v)) = do emit (getLocal (wasmName v)); emit (load 0)

-- Pushes address of a local varible onto the stack, i.e. some offset from SP.
emitLocalPtrAddr :: (Functor g, Emit :<: f) => SrcVar -> Prog f g ()
emitLocalPtrAddr v = do
    sp     <- spName
    offset <- varSPOffset v
    emit (getGlobal sp)
    emit (constNum (fromIntegral offset))
    emit (binOp SUB)

-- Emit instruction to set value of a variable to value on top of stack.
-- I.e. store value at memory address pointed to, or set value of variable.
emitSetVarVal :: (Functor g, Emit :<: f) => LocType (ValType SrcVar) -> Prog f g () -> Prog f g ()
emitSetVarVal (Local (Val v)) val = do val; emit (setLocal (wasmName v))
emitSetVarVal (Param (Val v)) val = do val; emit (setLocal (wasmName v))
emitSetVarVal (Local (Ptr v)) val = emitSetPtr (emitLocalPtrAddr v) val
emitSetVarVal (Param (Ptr v)) val = emitSetPtr (emit (getLocal (wasmName v))) val

-- Sets the value pointed to by a pointer.
emitSetPtr :: (Functor g, Emit :<: f) => Prog f g () -> Prog f g () -> Prog f g ()
emitSetPtr addr val = do addr; val; emit (store 0)

--------------------------------------------------------------------------------
-- Aux Semantics
--------------------------------------------------------------------------------

-- Stores state about a function to which instructions can emitted to.
data FuncEnv = FuncEnv {
    -- Name of the function in source code.
    name :: FuncName
    -- Whether the function returns a value
  , doesRet :: Bool
    -- WebAssembly is appended to WASM on top of stack, i.e. using (>>=)
  , instrStack :: [WASM]
    -- Offsets to SP, of variables local to this function, which are stored
    -- on the stack.
  , varSPOffsets :: Map SrcVar Word
    -- List of variables local to this function.
  , localVars :: SrcLocalVars
    -- Variables passed in as parameters to this function.
  , paramVars :: SrcParamVars
}

-- Append instruction to end of innermost scope, i.e. topmost element.
appendInstr :: WASM -> FuncEnv -> FuncEnv
appendInstr instr env = env { instrStack=stk' } where
    stk' = case instrStack env of
        []         -> [instr]
        (blk:rest) -> (do blk; instr):rest

-- Returns the WebAssembly currently being built, i.e. at innermost scope.
peekBlock :: FuncEnv -> WASM
peekBlock env =
    case instrStack env of
        []        -> error "No instruction blocks"
        (instr:_) -> instr

-- Create a new block of instructions that will be emitted onto the end of.
pushBlock :: WASM -> FuncEnv -> FuncEnv
pushBlock instr env = env { instrStack=instr:(instrStack env) }

-- Remove the top block on instructions, meaning instructions will be emitted
-- onto the top of the tail.
popBlock :: FuncEnv -> FuncEnv
popBlock env = env { instrStack=tail (instrStack env) }

-- Stores state about global code generation.
data Env = Env {
    -- WebAssembly is emitted to function environment on top of stack.
    workingFuncs :: [FuncEnv]
    -- Functions which have finished being emitted.
  , completeFuncs :: [Func]
    -- Name of global variable used as SP.
  , globalSPName :: GlobalName
    -- Locations of variables in each procedure.
  , funcVarLocs  :: Map SrcProc (SrcLocalVars, SrcParamVars)
}

-- Returns the current working function, i.e. function to which WebAssembly
-- instructions are emitted.
workingFunc :: Env -> FuncEnv
workingFunc env = head (workingFuncs env)

-- Modify the function environment on top of the stack, e.g. append an instruction.
modifyWorkingFunc :: (FuncEnv -> FuncEnv) -> Env -> Env
modifyWorkingFunc f env =
    case workingFuncs env of
        []          -> error "No working functions"
        (func:rest) -> env { workingFuncs=(f func):rest }

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

type Op  f   = State   Env :+: f
type Sc  g   = LocalSt Env :+: g
type Ctx f g = Prog (Op f) (Sc g)

type Carrier f g = Nest (Ctx f g)

gen :: (Functor f, Functor g) => a -> Carrier f g a 'Z
gen x = Nest (return (NZ x))

alg :: (Functor f, Functor g) => Alg (Emit :+: f) (Block :+: g) (Carrier f g a)
alg = A a d p where
    a :: (Functor f, Functor g) => (Emit :+: f) (Carrier f g a n) -> Carrier f g a n
    -- Append instruction to end of current block in current function.
    a (Emit instr k) = Nest $ do
        env <- get
        put (modifyWorkingFunc (appendInstr instr) env)
        runNest k

    -- Get instructions of current block of current function.
    a (CurrInstr fk) = Nest $ do
        env <- get
        let topInstr = peekBlock (workingFunc env)
        runNest (fk topInstr)

    -- Give continuation name of stack pointer variable.
    a (SPName fk) = Nest $ do
        env <- get
        runNest (fk (globalSPName env))

    -- Give continuation offset of given variable from the stack pointer.
    a (VarSPOffset var fk) = Nest $ do
        env <- get
        undefined

    d :: (Functor f, Functor g) => (Block :+: g) (Carrier f g a ('S n)) -> Carrier f g a n
    d = undefined

    p :: (Functor f, Functor g) => Carrier f g a n -> Carrier f g a ('S n)
    p = undefined

mkCtx :: (Functor f, Functor g) => Prog (Emit :+: f) (Block :+: g) a -> Ctx f g a
mkCtx prog = case run gen alg prog of
    (Nest prog') -> fmap (\(NZ x) -> x) prog'

handleCodeGen :: (Functor f, Functor g) => Prog (Emit :+: f) (Block :+: g) a -> Prog f g (a, Module)
handleCodeGen = undefined
