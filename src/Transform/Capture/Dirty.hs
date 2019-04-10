
-- Produces a set of all dirty variables in an AST, i.e. variables that are
-- modified in an outer scope and inside a procedure.

{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Transform.Capture.Dirty
( DirtyVars
, dirtyVars
) where

import Front.AST
import Transform.Capture.DirtyEff
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Void

type Op      v = Modified v :+: Void
type Sc        = ModScope   :+: Void
type Carrier v = Prog (Op v) Sc ()

instance FreeAlg (VarExp v) (Carrier v) where
    alg _ = return ()

instance FreeAlg AExp (Carrier v) where
    alg _ = return ()

instance FreeAlg BExp (Carrier v) where
    alg _ = return ()

instance FreeAlg (VarStm v) (Carrier v) where
    alg (SetVar v _) = modified v

instance FreeAlg (ProcStm p) (Carrier v) where
    alg _ = return ()

instance FreeAlg Stm (Carrier v) where
    alg (Skip)       = return ()
    alg (Export _)   = return ()
    alg (If _ t e)   = t >> e -- Side effect is dirty vars are written out.
    alg (While _ s)  = s
    alg (Comp s1 s2) = s1 >> s2

instance FreeAlg (BlockStm v p) (Carrier v) where
    alg (Block varDecls procDecls body) = do
        -- Because Carrier acts like a writer, mapM here writes out results without
        -- needing to explicitly collect them.
        --
        -- By saying variables declared were modified, it ensures that if
        -- the variables are modified inside any procedures then they will be
        -- marked as dirty.
        mapM (modified . fst) varDecls

        -- modScope . snd places body of each procedure inside modScope block.
        -- Therefore, if a variable is seen to be modified in this scope and
        -- inside block, the variable will be marked as dirty.
        mapM (modScope . snd) procDecls
        body

makeDirty :: FreeAlg f (Carrier v) => Free f a -> Carrier v
makeDirty = evalF (const (return ()))

-- Returns all variables in AST which are modified at some scope, and also
-- modified in the scope of a procedure inside the outer scope.
dirtyVars :: Ord v => FreeAlg f (Carrier v) => Free f a -> DirtyVars v
dirtyVars = snd . handleVoid . handleDirtyVars . makeDirty
