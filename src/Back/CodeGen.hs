
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
, FuncMeta(..)
, wasmName
, emit
, spName
, varSPOffset
, funcVarLocations
, varType
, codeBlock
, function
, emitGetVarAsArg
, emitGetVarVal
, emitSetVarVal
, funcFromMeta
, handleCodeGen
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Transform.Rename.Rename (FreshName)
import Transform.Capture.Dirty (DirtyVars)
import Transform.Capture.Location (Locations)
import Back.WASM hiding (name, doesRet)
import Helper.Scope.Prog
import Helper.Scope.Nest
import Helper.Co
import Helper.Eff.State
import Helper.Eff.Reader
import Helper.Eff

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

-- Type of variable from source language.
type SrcVar  = FreshName
-- Type of procedure from source language.
type SrcProc = FreshName

-- Variables local to a function.
type SrcLocalVars = Set SrcVar
-- Variables passed into a function.
type SrcParamVars = Set SrcVar
-- Used to lookup where variables are stored in relation to SP.
type SrcSPOffsets = Map SrcVar Word

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
    | Function' SrcProc DoesRet SrcLocalVars SrcParamVars SrcSPOffsets k
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

pattern VarType var fk <- (prj -> Just (VarType' var fk))
varType :: Emit :<: f => SrcVar -> Prog f g (LocType (ValType SrcVar))
varType v = injectP (VarType' v Var)

-- Returns instructions emitted in the block allowing them to be wrapped up
-- in a WASM control structure, e.g. BLOCK.
pattern Block k <- (prj -> Just (Block' k))
codeBlock :: (Functor f, Emit :<: f, Block :<: g) => Prog f g () -> Prog f g WASM
codeBlock inner = injectPSc (fmap (fmap return) (Block' (do inner; currInstr)))

-- Emits nested instructions to a new function.
pattern Function fname doesRet locals params spOffsets body
    <- (prj -> Just (Function' fname doesRet locals params spOffsets body))

function :: (Functor f, Block :<: g)
         => SrcProc
         -> DoesRet
         -> SrcLocalVars
         -> SrcParamVars
         -> SrcSPOffsets
         -> Prog f g a
         -> Prog f g a

function pname doesRet locals params spOffsets body
    = injectPSc (fmap (fmap return) (Function' pname doesRet locals params spOffsets body ))

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
type InstrBlocks = [WASM]

-- Append instruction to end of innermost scope, i.e. topmost element.
appendInstr :: WASM -> InstrBlocks -> InstrBlocks
appendInstr instr []         = [instr]
appendInstr instr (blk:rest) = (do blk; instr):rest

-- Returns the WebAssembly currently being built, i.e. at innermost scope.
peekBlock :: InstrBlocks -> WASM
peekBlock []        = error "No instruction blocks"
peekBlock (instr:_) = instr

-- Create a new block of instructions that will be emitted onto the end of.
pushBlock :: WASM -> InstrBlocks -> InstrBlocks
pushBlock = (:)

-- Remove the top block on instructions, meaning instructions will be emitted
-- onto the top of the tail.
popBlock :: InstrBlocks -> InstrBlocks
popBlock = tail

-- Metadata about the function which has instructions currently being emitted to.
-- Kept separate from InstrBlocks so a Reader can be used to access these
-- variables and ensure they are not modified accidentally.
data FuncMeta = FuncMeta {
    -- Name of the function in source code.
    name :: FuncName
    -- Whether the function returns a value
  , doesRet :: Bool
    -- Offsets to SP, of variables local to this function, which are stored
    -- on the stack.
  , varSPOffsets :: Map SrcVar Word
    -- List of variables local to this function.
  , localVars :: SrcLocalVars
    -- Variables passed in as parameters to this function.
  , paramVars :: SrcParamVars
}

-- Return whether varible is a value or parameter.
varLocType :: SrcVar -> FuncMeta -> (v -> LocType v)
varLocType v func | v `Set.member` (localVars func) = Local
                  | otherwise                       = Param

-- State of function generation.
data WorkingFuncs = WorkingFuncs {
    -- WebAssembly is emitted to function environment on top of stack.
    workingFuncs :: [InstrBlocks]
    -- Functions which have finished being emitted.
  , completeFuncs :: [Func]
}

-- Returns the current working function, i.e. function to which WebAssembly
-- instructions are emitted.
workingFunc :: WorkingFuncs -> InstrBlocks
workingFunc env = head (workingFuncs env)

pushWorkingFunc :: InstrBlocks -> WorkingFuncs -> WorkingFuncs
pushWorkingFunc func env = env { workingFuncs=func:(workingFuncs env) }

-- Modify the function environment on top of the stack, e.g. append an instruction.
modifyWorkingFunc :: (InstrBlocks -> InstrBlocks) -> WorkingFuncs -> WorkingFuncs
modifyWorkingFunc f env =
    case workingFuncs env of
        []          -> env { workingFuncs=[f []] }
        (func:rest) -> env { workingFuncs=(f func):rest }

funcFromMeta :: FuncMeta -> WASM -> Func
funcFromMeta meta instr = Func (name meta) (doesRet meta) locals params instr where
    locals = map wasmName (Set.elems (localVars meta))
    params = map wasmName (Set.elems (paramVars meta))

-- Move current working function to being completed.
-- Assumes there is only one instruction left in block of intructions for top function.
completeWorkingFunc :: FuncMeta -> WorkingFuncs -> WorkingFuncs
completeWorkingFunc meta env = env' where
    env'           = env { completeFuncs=completeFuncs', workingFuncs=rest }
    completeFuncs' = func:(completeFuncs env)
    func           = funcFromMeta meta (peekBlock block)
    (block:rest)   = workingFuncs env

-- Kept separate from WorkingFuncs so a Reader can be used to access these
-- variables. This ensures they are not modified accidentally.
--
-- NOTE
-- This safety is only possible because Emit and Block are transformed into Ctx.
-- If a normal Carrier was used, and semantics all handled manually, all these
-- variables would be part of some environment which would act as a state
-- (like in Syntax and Semantics State example). Therefore allowing the
-- possiblity of incorrect variables being modified.
data GlobalMeta = GlobalMeta {
    -- Name of global variable used as SP.
    globalSPName :: GlobalName
    -- Locations of variables in each procedure.
  , funcVarLocs  :: Map SrcProc (SrcLocalVars, SrcParamVars)
    -- Whether a variable is modified in some scope, and also in a procedure
    -- inside the scope.
  , dirtyVars    :: Set SrcVar
}

-- Return whether variable is value or pointer.
varValType :: SrcVar -> GlobalMeta -> (v -> ValType v)
varValType var meta | var `Set.member` (dirtyVars meta) = Ptr
                    | otherwise                         = Val

lookupFuncVarLocs :: SrcProc -> GlobalMeta -> (SrcLocalVars, SrcParamVars)
lookupFuncVarLocs pname meta =
    case Map.lookup pname (funcVarLocs meta) of
        Nothing   -> error ("No procedure named: " ++ show pname)
        Just locs -> locs

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

type Op  f   = State   WorkingFuncs :+: Ask    FuncMeta :+: Ask    GlobalMeta :+: f
type Sc  g   = LocalSt WorkingFuncs :+: LocalR FuncMeta :+: LocalR GlobalMeta :+: g
type Ctx f g = Prog (Op f) (Sc g)

type Carrier f g = Nest (Ctx f g)

askFuncMeta :: (Functor f, Functor g) => Ctx f g FuncMeta
askFuncMeta = ask

askGlobalMeta :: (Functor f, Functor g) => Ctx f g GlobalMeta
askGlobalMeta = ask

gen :: (Functor f, Functor g) => a -> Carrier f g a 'Z
gen x = Nest (return (NZ x))

alg :: (Functor f, Functor g) => Alg (Emit :+: f) (Block :+: g) (Carrier f g a)
alg = A a d p where
    a :: (Functor f, Functor g) => (Emit :+: f) (Carrier f g a n) -> Carrier f g a n
    -- Append instruction to end of current block in current function.
    a (Emit instr k) = Nest $ do
        funcs <- get
        put (modifyWorkingFunc (appendInstr instr) funcs)
        runNest k

    -- Get instructions of current block of current function.
    a (CurrInstr fk) = Nest $ do
        funcs <- get
        let topInstr = peekBlock (workingFunc funcs)
        runNest (fk topInstr)

    -- Give continuation name of stack pointer variable.
    a (SPName fk) = Nest $ do
        globals <- askGlobalMeta
        runNest (fk (globalSPName globals))

    -- Give continuation offset of given variable from the stack pointer.
    a (VarSPOffset var fk) = Nest $ do
        funcMeta <- askFuncMeta
        case Map.lookup var (varSPOffsets funcMeta) of
            Nothing     -> error ("No var named: " ++ show var)
            Just offset -> runNest (fk offset)

    -- Give continuation local variables and parameters to function with given name.
    a (FuncVarLocations pname fk) = Nest $ do
        globals <- askGlobalMeta
        case Map.lookup pname (funcVarLocs globals) of
            Nothing   -> error ("No function named: " ++ show pname)
            Just locs -> runNest (fk locs)

    -- Give continuation the type of the supplied variable.
    a (VarType v fk) = Nest $ do
        funcMeta   <- askFuncMeta
        globalMeta <- askGlobalMeta

        let makeVarType = (varLocType v funcMeta) . (varValType v globalMeta)
        runNest (fk (makeVarType v))

    a (Other op) = Nest (Op (fmap runNest (R $ R $ R op)))

    d :: (Functor f, Functor g) => (Block :+: g) (Carrier f g a ('S n)) -> Carrier f g a n
    d (Block k) = Nest $ do
        funcs <- get

        let emptyInstr = return ()
            funcs'     = modifyWorkingFunc (pushBlock emptyInstr) funcs

        NS k' <- localSt funcs' (runNest k)
        k'

    d (Function pname ret locals params spOffsets k) = Nest $ do
        let funcMeta = FuncMeta (wasmName pname) ret spOffsets locals params

        NS k' <- localR funcMeta (do
            -- Create a new function that instructions will be emitted to.
            -- The function currently contains an empty stack of instructions.
            modify (pushWorkingFunc [])
            k' <- runNest k
            -- All instructions have been added to the function, create a
            -- WebAssembly function.
            modify (completeWorkingFunc funcMeta)
            return k')

        k'

    d (Other op) = Nest (Scope (fmap (\(Nest x) -> fmap f x) (R $ R $ R op))) where
        f :: (Functor f, Functor g) => Nest' (Ctx f g) a ('S n) -> Ctx f g (Nest' (Ctx f g) a n)
        f (NS x) = x

    p :: (Functor f, Functor g) => Carrier f g a n -> Carrier f g a ('S n)
    p (Nest prog) = Nest $ do
        x <- prog
        return (NS (return x))

mkCtx :: (Functor f, Functor g) => Prog (Emit :+: f) (Block :+: g) a -> Ctx f g a
mkCtx prog = case run gen alg prog of
    (Nest prog') -> fmap (\(NZ x) -> x) prog'

-- Returns WebAssembly of main function, and WebAssembly functions for any
-- nested procedures.
handleCodeGen :: (Functor f, Functor g)
              => FuncMeta
              -> GlobalName
              -> Map SrcProc Locations
              -> DirtyVars SrcVar
              -> Prog (Emit :+: f) (Block :+: g) a
              -> Prog f g (a, WASM, [Func])

handleCodeGen mainFuncMeta spName funcVarLocs dirtyVars prog = do
    let workingFuncs = WorkingFuncs [] []
        globalMeta   = GlobalMeta spName funcVarLocs dirtyVars

    (x, workingFuncs) <- (handleReader globalMeta . handleReader mainFuncMeta . handleState workingFuncs . mkCtx) prog
    return (x, peekBlock (workingFunc workingFuncs), completeFuncs workingFuncs)
