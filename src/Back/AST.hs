
-- Conversion from While AST to CodeGen to generate WebAssembly.

{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Back.AST where

import Data.Map (Map)
import qualified Data.Map as Map
import Front.AST hiding (call, ifElse, block)
import Back.CodeGen
import Back.WASM hiding (Export)
import Transform.Rename.Rename (FreshName)
import Transform.Capture.Dirty (DirtyVars)
import Transform.Capture.Location (Locations)
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Void

type Op      = Emit  :+: Void
type Sc      = Block :+: Void
type Carrier = Prog Op Sc ()

instance FreeAlg (VarExp SrcVar) Carrier where
    alg (GetVar v) = varType v >>= emitGetVarVal

instance FreeAlg AExp Carrier where
    alg (Num n)   = emit (constNum n)
    alg (Add x y) = x >> y >> emit (binOp ADD)
    alg (Sub x y) = x >> y >> emit (binOp SUB)
    alg (Mul x y) = x >> y >> emit (binOp MUL)

instance FreeAlg BExp Carrier where
    alg (T)       = emit (constNum 1)
    alg (F)       = emit (constNum 0)
    alg (Equ x y) = x >> y >> emit (relOp EQU)
    alg (LEq x y) = x >> y >> emit (relOp LEQ)
    alg (And x y) = x >> y >> emit (binOp AND)
    alg (Not x)   = x      >> emit (uniOp NOT)

instance FreeAlg (VarStm SrcVar) Carrier where
    alg (SetVar v x) = emitSetVar v x

emitSetVar :: SrcVar -> Carrier -> Carrier
emitSetVar v x = do
    typedV <- varType v
    emitSetVarVal typedV x

instance FreeAlg (ProcStm SrcProc) Carrier where
    alg (Call pname) = do
        (_, paramNames) <- funcVarLocations pname
        -- Because emitting instructions is a side effect, mapM_ emits
        -- all arguments to function.
        mapM_ (\v -> varType v >>= emitGetVarAsArg) paramNames
        emit (call (wasmName pname))

instance FreeAlg Stm Carrier where
    alg (Skip)            = emit nop
    alg (Export x)        = x >> emit ret
    alg (Comp s1 s2)      = s1 >> s2

    alg (If cond t e)     = do
        cond
        wasmThen <- codeBlock t
        wasmElse <- codeBlock e
        emit (ifElse wasmThen wasmElse)

    alg (While cond body) = do
        wasmBlock <- codeBlock (do
            wasmLoop <- codeBlock (do
                cond; emit (uniOp NOT); emit (brIf 1)
                body
                emit (br 0))
            emit (loop wasmLoop))
        emit (block wasmBlock)

instance FreeAlg (BlockStm SrcVar SrcProc) Carrier where
    alg (Block varDecls procDecls body) = do
        -- Procedures are emitted into separate functions distinct from
        -- the function this blocks' variable declarations and body are
        -- emitted into.
        mapM_ (uncurry emitFunc) procDecls

        mapM_ (uncurry emitSetVar) varDecls
        body

emitFunc :: SrcProc -> Carrier -> Carrier
emitFunc pname body = do
    (locals, params) <- funcVarLocations pname
    let spOffsets = Map.empty
    function pname False locals params spOffsets body

mkCodeGen :: FreeAlg f Carrier => Free f a -> Carrier
mkCodeGen = evalF (const (return ()))

compile ::  FreeAlg f Carrier => Locations -> Map SrcProc Locations -> DirtyVars SrcVar -> Free f a -> Module
compile mainVarLocs funcVarLocs dirtyVars prog = Module funcs globals memories exports where
    funcs    = mainFunc:nestedFuncs
    globals  = []
    memories = []
    exports  = []

    mainFunc                   = funcFromMeta mainFuncMeta mainWasm
    (_, mainWasm, nestedFuncs) = (handleVoid . handle . mkCodeGen) prog

    handle                   = handleCodeGen mainFuncMeta "sp" funcVarLocs dirtyVars
    mainFuncMeta             = FuncMeta "main" False Map.empty mainLocals mainParams
    (mainLocals, mainParams) = mainVarLocs
