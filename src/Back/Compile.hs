
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeOperators #-}

module Back.Compile where

import Front.AST hiding (call, ifElse, block)
import Back.WASM hiding (Export)
import Back.CodeGenSyntax
import Back.CodeGenSem
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Scope.Prog
import Helper.Co

type CodeGen = Prog GenData Function WASM

--------------------------------------------------------------------------------
-- Convenience functions to make converting from While easier
--------------------------------------------------------------------------------

-- Converts source-code name into name that can be emitted into WASM.
wasmName :: SrcVar -> LocalName
wasmName = show

-- Emit instructions to push an argument to another function onto the stack.
-- I.e. address of a pointer, or value of a value.
varAsArg :: LocType (ValType SrcVar) -> CodeGen
varAsArg (Local (Val v)) = return $ getLocal (wasmName v)
varAsArg (Param (Val v)) = return $ getLocal (wasmName v)
varAsArg (Local (Ptr v)) = localPtrAddr v
varAsArg (Param (Ptr v)) = return $ getLocal (wasmName v)

-- Emit instuction to push the value of a variable onto the stack.
-- I.e. deference a pointer, or value of a value.
varVal :: LocType (ValType SrcVar) -> CodeGen
varVal (Local (Val v)) = return $ getLocal (wasmName v)
varVal (Param (Val v)) = return $ getLocal (wasmName v)
varVal (Local (Ptr v)) = localPtrAddr v >>= \addr -> return $ addr >> load 0
varVal (Param (Ptr v)) = return $ getLocal (wasmName v) >> load 0

-- Pushes address of a local varible onto the stack, i.e. some offset from SP.
localPtrAddr :: SrcVar -> CodeGen
localPtrAddr v = do
    sp     <- spName
    offset <- varSPOffset v
    return (do
        getGlobal sp
        constNum (fromIntegral offset)
        binOp SUB)

-- Emit instruction to set value of a variable to value on top of stack.
-- I.e. store value at memory address pointed to, or set value of variable.
-- emitSetVarVal :: (Functor g, Emit :<: f) => LocType (ValType SrcVar) -> Prog f g () -> Prog f g ()
-- emitSetVarVal (Local (Val v)) val = do val; emit (setLocal (wasmName v))
-- emitSetVarVal (Param (Val v)) val = do val; emit (setLocal (wasmName v))
-- emitSetVarVal (Local (Ptr v)) val = emitSetPtr (emitLocalPtrAddr v) val
-- emitSetVarVal (Param (Ptr v)) val = emitSetPtr (emit (getLocal (wasmName v))) val

-- Sets the value pointed to by a pointer.
-- emitSetPtr :: (Functor g, Emit :<: f) => Prog f g () -> Prog f g () -> Prog f g ()
-- emitSetPtr addr val = do addr; val; emit (store 0)

instance FreeAlg (VarExp SrcVar) CodeGen where
    alg (GetVar v) = varType v >>= varVal

instance FreeAlg AExp CodeGen where
    alg (Num n)   = return $ constNum n
    alg (Add x y) = x >> y >> return (binOp ADD)
    -- alg (Sub x y) = x >> y >> emit (binOp SUB)
    -- alg (Mul x y) = x >> y >> emit (binOp MUL)

instance FreeAlg BExp CodeGen where
    alg = undefined
    -- alg (T)       = emit (constNum 1)
    -- alg (F)       = emit (constNum 0)
    -- alg (Equ x y) = x >> y >> emit (relOp EQU)
    -- alg (LEq x y) = x >> y >> emit (relOp LEQ)
    -- alg (And x y) = x >> y >> emit (binOp AND)
    -- alg (Not x)   = x      >> emit (uniOp NOT)

instance FreeAlg (VarStm SrcVar) CodeGen where
    alg = undefined
    -- alg (SetVar v x) = emitSetVar v x

-- emitSetVar :: SrcVar -> CodeGen -> CodeGen
-- emitSetVar v x = do
--     typedV <- varType v
--     emitSetVarVal typedV x

instance FreeAlg (ProcStm SrcProc) CodeGen where
    alg = undefined
    -- alg (Call pname) = do
    --     (_, paramNames) <- funcVarLocations pname
    --     -- Because emitting instructions is a side effect, mapM_ emits
    --     -- all arguments to function.
    --     mapM_ (\v -> varType v >>= emitGetVarAsArg) paramNames
    --     emit (call (wasmName pname))

instance FreeAlg Stm CodeGen where
    alg = undefined

    -- alg (Skip)            = emit nop
    -- alg (Export x)        = x >> emit ret
    -- alg (Comp s1 s2)      = s1 >> s2
    --
    -- alg _ = undefined

    -- alg (If cond t e)     = do
    --     cond
    --     wasmThen <- codeBlock t
    --     wasmElse <- codeBlock e
    --     emit (ifElse wasmThen wasmElse)
    --
    -- alg (While cond body) = do
    --     wasmBlock <- codeBlock (do
    --         wasmLoop <- codeBlock (do
    --             cond; emit (uniOp NOT); emit (brIf 1)
    --             body
    --             emit (br 0))
    --         emit (loop wasmLoop))
    --     emit (block wasmBlock)

instance FreeAlg (BlockStm SrcVar SrcProc) CodeGen where
    alg = undefined

mkCodeGen :: FreeAlg f CodeGen => Free f a -> CodeGen
mkCodeGen = evalF (const (return (return ())))

compile' :: FreeAlg f CodeGen => Free f () -> (WASM, [Func])
compile' = handleCodeGen . mkCodeGen

compile :: FreeAlg f CodeGen => Free f () -> Module
compile prog = Module funcs [] [] [] where
    funcs = mainFunc:nestedFuncs
    mainFunc = Func "main" False [] [] mainWasm
    (mainWasm, nestedFuncs) = compile' prog

--     alg (Block varDecls procDecls body) = do
--         -- Procedures are emitted into separate functions distinct from
--         -- the function this blocks' variable declarations and body are
--         -- emitted into.
--         -- mapM_ (uncurry emitFunc) procDecls
--
--         mapM_ (uncurry emitSetVar) varDecls
--         body
--
-- mkCodeGen :: FreeAlg f CodeGen => Free f a -> CodeGen
-- mkCodeGen = evalF (const (return ()))
--
-- compile' :: FreeAlg f CodeGen => Free f () -> WASM
-- compile' = handleCodeGen . mkCodeGen
--
-- compile :: FreeAlg f CodeGen => Free f () -> Module
-- compile prog = Module funcs [] [] [] where
--     funcs    = [mainFunc]
--     mainFunc = Func "main" False [] [] body
--     body     = compile' prog
