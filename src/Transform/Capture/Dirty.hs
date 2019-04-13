
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

-- TODO: Problem:
-- Cannot decide whether variable is dirty or not. AExp assignments should be, but
-- don't have access to name.
-- Possible solution - store name with assignment, i.e.
--
-- data Assign v k
--     = AssignAExp v k
--     | AssignArr  v [k]
--
-- Then, block decl stores assignments instead of (v, k) tuple.
-- Assignment can then be ommitted from Stm type. 
instance FreeAlg Assign (Carrier v) where
    alg = undefined

instance FreeAlg (VarStm v) (Carrier v) where
    alg (SetVar  v _)   = modified v
    -- Setting an element does not count as setting a variable, firstly because
    -- the value of the whole array was not set, and also arrays are stored as
    -- pointers and therefore are always 'dirty' in a sense.
    alg (SetElem _ _ _) = return ()

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
