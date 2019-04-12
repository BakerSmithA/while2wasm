
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts, TypeOperators #-}

module Back.Compile where

import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad (foldM)
import Front.AST hiding (call, ifElse, block)
import Back.WASM hiding (Export)
import Back.CodeGen
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Scope.Prog
import Helper.Co

type CodeGen a = Prog Emit Block a

--------------------------------------------------------------------------------
-- Convenience functions to make converting from While easier
--------------------------------------------------------------------------------

-- Converts source-code name into name that can be emitted into WASM.
wasmName :: SrcVar -> LocalName
wasmName = show

-- Emit instructions to push an argument to another function onto the stack.
-- I.e. address of a pointer, or value of a value.
varAsArg :: LocType (ValType SrcVar) -> CodeGen WASM
varAsArg (Local (Val v)) = return $ getLocal (wasmName v)
varAsArg (Param (Val v)) = return $ getLocal (wasmName v)
varAsArg (Local (Ptr v)) = localPtrAddr v
varAsArg (Param (Ptr v)) = return $ getLocal (wasmName v)

-- Emit instuction to push the value of a variable onto the stack.
-- I.e. deference a pointer, or value of a value.
varVal :: LocType (ValType SrcVar) -> CodeGen WASM
varVal (Local (Val v)) = return $ getLocal (wasmName v)
varVal (Param (Val v)) = return $ getLocal (wasmName v)
varVal (Local (Ptr v)) = localPtrAddr v >>> return (load 0)
varVal (Param (Ptr v)) = return $ getLocal (wasmName v) >> load 0

-- Pushes address of a local varible onto the stack, i.e. some offset from SP.
localPtrAddr :: SrcVar -> CodeGen WASM
localPtrAddr v = do
    sp     <- spName
    offset <- varSPOffset v
    return (do
        getGlobal sp
        constNum (fromIntegral offset)
        binOp SUB)

-- Emit instruction to set value of a variable to value on top of stack.
-- I.e. store value at memory address pointed to, or set value of variable.
setVarVal :: LocType (ValType SrcVar) -> CodeGen WASM -> CodeGen WASM
setVarVal (Local (Val v)) val = val >>= \val' -> return (val' >> setLocal (wasmName v))
-- setVarVal (Param (Val v)) val = do val; emit (setLocal (wasmName v))
-- setVarVal (Local (Ptr v)) val = emitSetPtr (emitLocalPtrAddr v) val
-- setVarVal (Param (Ptr v)) val = emitSetPtr (emit (getLocal (wasmName v))) val

-- Sets the value pointed to by a pointer.
setPtr :: CodeGen WASM -> CodeGen WASM -> CodeGen WASM
setPtr addr val = undefined --do addr; val; emit (store 0)

infixr 0 >>>

-- TODO: Make CodeGen a monoid if WASM is a monoid?
-- Convenience function to help avoid lots of wrapping and unwrapping.
(>>>) :: CodeGen WASM -> CodeGen WASM -> CodeGen WASM
(>>>) x y = do
    x' <- x
    y' <- y
    return (x' >> y')

instance FreeAlg (VarExp SrcVar) (CodeGen WASM) where
    alg (GetVar v) = varType v >>= varVal

instance FreeAlg AExp (CodeGen WASM) where
    alg (Num n)   = return $ constNum n
    alg (Add x y) = x >>> y >>> return (binOp ADD)
    alg (Sub x y) = x >>> y >>> return (binOp SUB)
    alg (Mul x y) = x >>> y >>> return (binOp MUL)

instance FreeAlg BExp (CodeGen WASM) where
    alg (T)       = return (constNum 1)
    alg (F)       = return (constNum 0)
    alg (Equ x y) = x >>> y >>> return (relOp EQU)
    alg (LEq x y) = x >>> y >>> return (relOp LEQ)
    alg (And x y) = x >>> y >>> return (binOp AND)
    alg (Not x)   = x >>> return (uniOp NOT)

instance FreeAlg (VarStm SrcVar) (CodeGen WASM) where
    alg (SetVar v x) = varType v >>= \v' -> setVarVal v' x

instance FreeAlg (ProcStm SrcProc) (CodeGen WASM) where
    alg = undefined
    -- alg (Call pname) = do
    --     (_, paramNames) <- funcVarLocations pname
    --     -- Because emitting instructions is a side effect, mapM_ emits
    --     -- all arguments to function.
    --     mapM_ (\v -> varType v >>= emitGetVarAsArg) paramNames
    --     emit (call (wasmName pname))

-- NOTE: Using Prog to represent WebAssembly allows generation to be very
-- natural, with output looking like WebAssembly code.
instance FreeAlg Stm (CodeGen WASM) where
    alg (Skip)            = return nop
    alg (Comp s1 s2)      = s1 >>> s2
    alg (Export x)        = x >>> return ret
    alg (If cond t e)     = cond >>> ifElse <$> t <*> e
    alg (While cond body) = do
        condWasm <- cond
        bodyWasm <- body
        return (
            block (
                loop (do
                    condWasm; uniOp NOT; brIf 1
                    bodyWasm
                    br 0)))

instance FreeAlg (BlockStm SrcVar SrcProc) (CodeGen WASM) where
    alg (Block varDecls procDecls body) = do
        -- Emit procedures as WebAssembly functions distinct from the current function.
        mapM_ (uncurry genProc) procDecls
        -- Flatten variable declarations into normal variable assignments.
        foldM (\acc (v, x) -> return acc >>> genVarDecl v x) (return ()) varDecls
            >>> body

-- Return function which returns type of a variable.
makeVarType :: Set SrcVar -> Set SrcVar -> Set SrcVar -> (SrcVar -> LocType (ValType SrcVar))
makeVarType _ _ _ v = Local (Val v)

-- Return function which returns offset of a variable from the stack pointer,
-- provided the variable has a local pointer type.
makeVarSPOffset :: Set SrcVar -> Set SrcVar -> (SrcVar -> SPOffset)
makeVarSPOffset = undefined

genProc :: SrcProc -> CodeGen WASM -> CodeGen ()
genProc pname body = do
    (locals, params) <- funcVars pname
    dirty <- dirtyVars

    let varType  = makeVarType locals params dirty
        spOffset = makeVarSPOffset locals params

    funcScope varType spOffset (do
        bodyWasm <- body
        let func = Func (wasmName pname) False [] [] bodyWasm
        emitFunc func)

genVarDecl :: SrcVar -> CodeGen WASM -> CodeGen WASM
genVarDecl v x = varType v >>= \v' -> setVarVal v' x

mkCodeGen :: FreeAlg f (CodeGen WASM) => Free f a -> CodeGen WASM
mkCodeGen = evalF (const (return (return ())))

compile' :: FreeAlg f (CodeGen WASM) => Set SrcVar -> Set SrcVar -> Set SrcVar -> Free f () -> (WASM, [Func])
compile' mainLocals mainParams dirty = handleCodeGen varType spOffset . mkCodeGen where
    varType  = makeVarType mainLocals mainParams dirty
    spOffset = makeVarSPOffset mainLocals mainParams

compile :: FreeAlg f (CodeGen WASM) => Free f () -> Module
compile prog = Module funcs [] [] [] where
    funcs = mainFunc:nestedFuncs
    mainFunc = Func "main" False [] [] mainWasm
    (mainWasm, nestedFuncs) = compile' mainLocals mainParams dirty prog
    ((mainLocals, mainParams), funcVarTypes) = undefined
    dirty = undefined

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
