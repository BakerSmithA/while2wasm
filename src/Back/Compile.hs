
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE TypeOperators, ParallelListComp #-}

module Back.Compile where

import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad (foldM)
import Front.AST hiding (call, ifElse, block)
import Back.WASM hiding (Export)
import qualified Back.WASM as WASM (Export(..))
import Back.CodeGen
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Scope.Prog
import Helper.Co

type CodeGen a = Prog Emit Block a

--------------------------------------------------------------------------------
-- Convenience functions to make converting from While easier
--------------------------------------------------------------------------------

infixr 0 >>>

-- TODO: Make CodeGen a monoid if WASM is a monoid?
-- Convenience function to help avoid lots of wrapping and unwrapping.
(>>>) :: CodeGen WASM -> CodeGen WASM -> CodeGen WASM
(>>>) x y = do
    x' <- x
    y' <- y
    return (x' >> y')

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
    -- NOTE: Optimisation, if the offset is 0, then there is no reason to subtract.
    return $ case offset of
        0 -> getGlobal sp
        x -> do
            getGlobal sp
            constNum (fromIntegral offset)
            binOp SUB
            
-- Emit instruction to set value of a variable to value on top of stack.
-- I.e. store value at memory address pointed to, or set value of variable.
setVarVal :: LocType (ValType SrcVar) -> CodeGen WASM -> CodeGen WASM
setVarVal (Local (Val v)) val = val >>= \val' -> return (val' >> setLocal (wasmName v))
setVarVal (Param (Val v)) val = val >>= \val' -> return (val' >> setLocal (wasmName v))
setVarVal (Local (Ptr v)) val = setPtr (localPtrAddr v) val
setVarVal (Param (Ptr v)) val = setPtr (return (getLocal (wasmName v))) val

-- Sets the value pointed to by a pointer.
setPtr :: CodeGen WASM -> CodeGen WASM -> CodeGen WASM
setPtr addr val = addr >>> val >>> return (store 0)

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
    alg (Call pname) = do
        -- WARNING: Is there a guarantee the order of argument pushed onto the
        -- stack is the same as they appear in the function definition?
        -- I.e. because a Set is used.
        (_, params) <- funcVars pname
        foldM (\acc v -> return acc >>> (varType v >>= varAsArg)) (return ()) params
            >>> return (call (wasmName pname))

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

genVarDecl :: SrcVar -> CodeGen WASM -> CodeGen WASM
genVarDecl v x = varType v >>= \v' -> setVarVal v' x

genProc :: SrcProc -> CodeGen WASM -> CodeGen ()
genProc pname body = do
    (locals, params) <- funcVars pname
    dirty <- dirtyVars

    let varType  = makeVarType locals params dirty
        spOffset = makeVarSPOffset locals params

    funcScope varType spOffset (do
        let locals'   = funcLocals locals
            params'   = funcLocals params
            stackSize = funcStackSize locals dirty

        bodyWasm <- funcWrapper stackSize body

        let func = Func (wasmName pname) False locals' params' bodyWasm
        emitFunc func)

-- TODO: Could this be used with Prog?
-- Wrap body of the function with extending and retracting stack according to
-- size of variables stored on stack by the function.
funcWrapper :: SPOffset -> CodeGen WASM -> CodeGen WASM
funcWrapper size body = modifySP ADD size >>> body >>> modifySP SUB size

modifySP :: BinOp -> SPOffset -> CodeGen WASM
modifySP _  0   = return (return ())
modifySP op val = do
    sp <- spName
    return (do
        getGlobal sp
        constNum (fromIntegral val)
        binOp op
        setGlobal sp)

-- Return function which returns type of a variable.
makeVarType :: Set SrcVar -> Set SrcVar -> Set SrcVar -> (SrcVar -> LocType (ValType SrcVar))
makeVarType locals params dirty v = locType (valType v) where
    locType = if Set.member v locals then Local else Param
    valType = if Set.member v dirty  then Ptr   else Val

-- Return function which returns offset of a variable from the stack pointer,
-- provided the variable has a local pointer type, i.e. Local (Ptr v)
makeVarSPOffset :: Set SrcVar -> Set SrcVar -> (SrcVar -> SPOffset)
makeVarSPOffset locals dirty v =
    let i32ByteSize = 4
        stackVars   = Set.elems (Set.intersection locals dirty)
        offsets     = [(v, offset*i32ByteSize) | v <- stackVars | offset <- [0..]]
        mapping     = Map.fromList offsets

    in case v `Map.lookup` mapping of
        Nothing     -> error ("Variable not stored on stack: " ++ show v)
        Just offset -> offset

-- Total size of variables stored on the stack. Used to tell how much the
-- entry and exit of function should increase/decrease SP.
funcStackSize :: Set SrcVar -> Set SrcVar -> SPOffset
funcStackSize locals dirty = fromIntegral $ 4 * Set.size (Set.intersection locals dirty)

funcLocals :: Set SrcVar -> [LocalName]
funcLocals = map wasmName . Set.elems

mkCodeGen :: FreeAlg f (CodeGen WASM) => Free f a -> CodeGen WASM
mkCodeGen = evalF (const (return (return ())))

compile' :: FreeAlg f (CodeGen WASM)
         -- Stack pointer name
         => GlobalName
         -- Variables local to main
         -> Set SrcVar
         -- Parameters to main
         -> Set SrcVar
         -- Dirty variables
         -> Set SrcVar
         -- Variables local and parameters to each function
         -> Map SrcProc (Set SrcVar, Set SrcVar)
         -- While AST to compile
         -> Free f ()
         -> (WASM, [Func])

compile' spName mainLocals mainParams dirty funcVars ast = handleCodeGen env codeGen where
    codeGen   = funcWrapper stackSize (mkCodeGen ast)
    stackSize = funcStackSize mainLocals dirty
    env       = Env [] varType spOffset spName dirty funcVars
    varType   = makeVarType mainLocals mainParams dirty
    spOffset  = makeVarSPOffset mainLocals dirty

compile :: FreeAlg f (CodeGen WASM)
        => (Set SrcVar, Set SrcVar)
        -> Map SrcProc (Set SrcVar, Set SrcVar)
        -> Set SrcVar
        -> Free f () -> Module

compile mainVars funcVars dirtyVars prog = Module funcs globals memories exports where
    funcs = mainFunc:nestedFuncs
    mainFunc = Func "main" True locals params mainWasm
    locals   = funcLocals mainLocals
    params   = funcLocals mainParams
    (mainWasm, nestedFuncs) = compile' spName mainLocals mainParams dirtyVars funcVars prog
    (mainLocals, mainParams) = mainVars

    globals  = [Global spName Mut 0]
    memories = [Memory "memory" 1]
    exports  = [WASM.Export "main" (ExportFunc "main"), WASM.Export "memory" (ExportMem "memory")]

    spName = "sp"
