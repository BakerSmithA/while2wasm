
-- Generates mapping from procedures to the variables they contain that are
-- local or foreign.

{-# LANGUAGE TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Transform.Capture.Location
( Locations
, procVarLocations
) where

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set
import Front.AST
import Transform.Capture.LocationEff
import Transform.Rename.Rename (FreshName)
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Void
import Helper.Eff.Writer

-- Mapping built up by folding over AST. Gives local and foreign vars for
-- each procedure.
type ProcVarLocations = Map FreshName (LocalVars, ForeignVars)

type Op      = LocOp         :+: Tell ProcVarLocations :+: Void
type Sc      = DiscardLocals :+: Void
type Carrier = Prog Op Sc ()

instance FreeAlg (VarExp FreshName) Carrier where
    alg (GetVar v) = seen v

instance FreeAlg AExp Carrier where
    alg (Num _)   = return ()
    alg (Add x y) = x >> y
    alg (Sub x y) = x >> y
    alg (Mul x y) = x >> y

instance FreeAlg BExp Carrier where
    alg (T)       = return ()
    alg (F)       = return ()
    alg (Equ x y) = x >> y
    alg (LEq x y) = x >> y
    alg (And x y) = x >> y
    alg (Not x)   = x

instance FreeAlg (VarStm FreshName) Carrier where
    alg (SetVar v x) = seen v >> x

instance FreeAlg (ProcStm p) Carrier where
    alg (Call _) = return ()

instance FreeAlg Stm Carrier where
    alg (Skip)       = return ()
    alg (Export x)   = x
    alg (If b t e)   = b >> t >> e
    alg (While b s)  = b >> s
    alg (Comp s1 s2) = s1 >> s2

instance FreeAlg (BlockStm FreshName FreshName) Carrier where
    alg (Block varDecls procDecls body) = do
        mapM (addLocal . fst) varDecls
        mapM (uncurry tellProcVarLocations) procDecls
        body

-- Update environment to contain mapping from procedure to
-- locations of variables inside it.
tellProcVarLocations :: FreshName -> Carrier -> Carrier
tellProcVarLocations pname body = do
    -- Get the locations of each variable inside the procedure.
    locs <- discardLocals (do
        body
        getLocations)

    tell (Map.singleton pname locs)

makeProcVarLocations :: FreeAlg f Carrier => Free f a -> Carrier
makeProcVarLocations = evalF (const (return ()))

-- Returns mapping from each procedure, to the locations of variables within each.
-- Also returns the locations of variables of the top-level scope.
procVarLocations :: FreeAlg f Carrier => Free f a -> (LocalVars, ProcVarLocations)
procVarLocations prog =
    let handle = handleVoid . handleWriter . handleLocs . makeProcVarLocations
        ((_, (topLocals, topForeigns)), procVarLocs) = handle prog
    -- Foreign variables cannot exist at the top-level, therefore they must be local.
    -- When compiled to WASM, this means meany global variables will be local
    -- to the main function.
    in (Set.union topLocals topForeigns, procVarLocs)
