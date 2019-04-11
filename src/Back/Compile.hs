
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Back.Compile where

import Front.AST hiding (call, ifElse, block)
import Back.WASM hiding (Export)
import Back.CodeGenSyntax
import Back.CodeGenSem
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Scope.Prog

type CodeGen = Prog Emit Block ()

instance FreeAlg (VarExp SrcVar) CodeGen where
    alg (GetVar v) = varType v >>= emitGetVarVal

instance FreeAlg AExp CodeGen where
    alg (Num n)   = emit (constNum n)
    alg (Add x y) = x >> y >> emit (binOp ADD)
    alg (Sub x y) = x >> y >> emit (binOp SUB)
    alg (Mul x y) = x >> y >> emit (binOp MUL)

instance FreeAlg BExp CodeGen where
    alg (T)       = emit (constNum 1)
    alg (F)       = emit (constNum 0)
    alg (Equ x y) = x >> y >> emit (relOp EQU)
    alg (LEq x y) = x >> y >> emit (relOp LEQ)
    alg (And x y) = x >> y >> emit (binOp AND)
    alg (Not x)   = x      >> emit (uniOp NOT)

instance FreeAlg (VarStm SrcVar) CodeGen where
    alg (SetVar v x) = emitSetVar v x

emitSetVar :: SrcVar -> CodeGen -> CodeGen
emitSetVar v x = do
    typedV <- varType v
    emitSetVarVal typedV x

instance FreeAlg (ProcStm SrcProc) CodeGen where
    alg (Call pname) = do
        (_, paramNames) <- funcVarLocations pname
        -- Because emitting instructions is a side effect, mapM_ emits
        -- all arguments to function.
        mapM_ (\v -> varType v >>= emitGetVarAsArg) paramNames
        emit (call (wasmName pname))

instance FreeAlg Stm CodeGen where
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

instance FreeAlg (BlockStm SrcVar SrcProc) CodeGen where
    alg (Block varDecls procDecls body) = do
        -- Procedures are emitted into separate functions distinct from
        -- the function this blocks' variable declarations and body are
        -- emitted into.
        -- mapM_ (uncurry emitFunc) procDecls

        mapM_ (uncurry emitSetVar) varDecls
        body

mkCodeGen :: FreeAlg f CodeGen => Free f a -> CodeGen
mkCodeGen = evalF (const (return ()))

compile' :: FreeAlg f CodeGen => Free f () -> WASM
compile' = handleCodeGen . mkCodeGen

compile :: FreeAlg f CodeGen => Free f () -> Module
compile prog = Module funcs [] [] [] where
    funcs    = [mainFunc]
    mainFunc = Func "main" False [] [] body
    body     = compile' prog
