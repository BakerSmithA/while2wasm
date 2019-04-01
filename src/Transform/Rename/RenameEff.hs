
-- Composite scoped effect handler to perform renaming, implemented using
-- extensible effects methods. Allows other effects to be added, e.g. exceptions
-- to ensure procedures have been defined before being used.

{-# LANGUAGE ViewPatterns, PatternSynonyms, TypeOperators, DeriveFunctor #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Transform.Rename.RenameEff
( Fresh
, Rename
, Local
, varName
, procName
, local
, handleRename
) where

import Data.Word (Word)
import Data.Map (Map)
import qualified Data.Map as Map
import Front.AST
import Helper.Prog
import Helper.Co
import Helper.Eff

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

type Fresh = Word

type VarNames  = [Ident]
type ProcNames = [Ident]

data Rename k
    -- Returns fresh name corresponding to renamed identifier. If the variable
    -- is unseen before (i.e. global variable), then a fresh mapping is created.
    = VarName' Ident (Fresh -> k)
    -- Returns fresh name corresponding to renamed procedure.
    | ProcName' Ident (Fresh -> k)
    deriving Functor

data Local k
    -- Assigns fresh names to all supplied names, and uses these new names
    -- inside continuation. After local, original names are restored.
    = Local' VarNames ProcNames k
    deriving Functor

-- Smart constructors

-- Use pattern synonyms and view patterns suggested in Effect Handlers in Scope.
-- These help make pattern matching in effect handler more readable.
pattern VarName v fk <- (prj -> Just (VarName' v fk))
varName :: (Functor f, Functor g) => Rename :<: f => Ident -> Prog f g Fresh
varName v = inject (VarName' v Var)

pattern ProcName v fk <- (prj -> Just (ProcName' v fk))
procName :: (Functor f, Functor g) => Rename :<: f => Ident -> Prog f g Fresh
procName v = inject (ProcName' v Var)

pattern Local vs ps k <- (prj -> Just (Local' vs ps k))
local :: (Functor f, Functor g) => Local :<: g => VarNames -> ProcNames -> Prog f g a -> Prog f g a
local vs ps inner = injectS (fmap (fmap return) (Local' vs ps inner))

--------------------------------------------------------------------------------
-- Semantics Auxillary
--------------------------------------------------------------------------------

-- Keeps track of mappings from original to fresh names, as well as the next
-- avaiable fresh name.
data Names = Names {
    seen    :: Map Ident Fresh
  , next    :: Fresh
}

emptyNames :: Names
emptyNames = Names Map.empty 0

-- Creates a fresh name and assigns a mapping from ident to the fresh name in
-- the current scope.
addFresh :: Ident -> Names -> (Fresh, Names)
addFresh ident names = (fresh, names') where
    names' = names { next=fresh+1, seen=seen' }
    seen'  = Map.insert ident fresh (seen names)
    fresh  = next names

-- Returns the mapping from ident to its fresh name, if one exists. Otherwise,
-- creates a new mapping.
getFresh :: Ident -> Names -> (Fresh, Names)
getFresh ident names =
    case ident `Map.lookup` (seen names) of
        Just fresh -> (fresh, names)
        Nothing    -> addFresh ident names

-- Descend into scope, and create new mappings for ids at this inner scope.
addManyFresh :: [Ident] -> Names -> Names
addManyFresh ids names = foldr f names ids where
    f ident ns = snd (addFresh ident ns)

-- Restore names to use old mappings, or new mappings if a mapping was not
-- present in the old mapping (e.g. a variable was defined at inner scope).
restoreNames :: Names -> Names -> Names
restoreNames old new = Names seen' next' where
    seen' = seen old `Map.union` seen new
    next' = next new -- Don't want to restore next fresh name

-- Simple wrapper around Names to store both variable and procedure names.
-- Must be kept separate to avoid overwriting.
data Env = Env {
    varNames  :: Names
  , procNames :: Names
}

emptyEnv :: Env
emptyEnv = Env emptyNames emptyNames

getFreshVar :: Ident -> Env -> (Fresh, Env)
getFreshVar v env = (fresh, env { varNames=varNames' }) where
    (fresh, varNames') = getFresh v (varNames env)

getFreshProc :: Ident -> Env -> (Fresh, Env)
getFreshProc f env = (fresh, env { procNames=procNames' } ) where
    (fresh, procNames') = getFresh f (procNames env)

addManyFreshVsPs :: VarNames -> ProcNames -> Env -> Env
addManyFreshVsPs vs ps env = env { varNames=varNames', procNames=procNames' } where
    varNames'  = addManyFresh vs (varNames env)
    procNames' = addManyFresh ps (procNames env)

restoreEnv :: Env -> Env -> Env
restoreEnv old new = Env vs ps where
    vs = restoreNames (varNames old) (varNames new)
    ps = restoreNames (procNames old) (procNames new)

--------------------------------------------------------------------------------
-- Effect Handler
--------------------------------------------------------------------------------

-- Same idea State from Syntax and Semantics for Operations with Scope.
data CarrierRN f g a n
    = RN { runRN :: Env -> (Prog f g (CarrierRN' f g a n, Env)) }

data CarrierRN' f g a :: Nat -> * where
    CZ :: a -> CarrierRN' f g a 'Z
    CS :: (Env -> (Prog f g (CarrierRN' f g a n, Env))) -> CarrierRN' f g a ('S n)

genRN :: (Functor f, Functor g) => a -> CarrierRN f g a 'Z
genRN x = RN $ \env -> (return (CZ x, env))

-- Convenience method for getting the fresh name of a variable or procedure
-- and 'running' the continuation supplying it the fresh name.
algGet :: (Env -> (Fresh, Env)) -> (Fresh -> CarrierRN f g a n) -> CarrierRN f g a n
algGet getFreshId fk = RN $ \env ->
    let (fresh, env') = getFreshId env
        carrier'      = fk fresh
    in runRN carrier' env'

algRN :: (Functor f, Functor g) => Alg (Rename :+: f) (Local :+: g) (CarrierRN f g a)
algRN = A a d p where
    a :: (Functor f, Functor g) => (Rename :+: f) (CarrierRN f g a n) -> CarrierRN f g a n
    a (VarName  ident fk) = algGet (getFreshVar  ident) fk
    a (ProcName ident fk) = algGet (getFreshProc ident) fk
    a (Other op)          = RN $ \env -> Op (fmap (\(RN runRN) -> runRN env) op) where

    d :: (Functor f, Functor g) => (Local :+: g) (CarrierRN f g a ('S n)) -> CarrierRN f g a n
    d (Local vs ps k) = RN $ \env -> do
        (CS runRN', env') <- runRN k (addManyFreshVsPs vs ps env)
        runRN' (restoreEnv env env')
    d (Other op) = RN $ \env -> Scope (fmap (f env) op) where
        f :: (Functor f, Functor g) => Env -> CarrierRN f g a ('S n) -> Prog f g (Prog f g (CarrierRN' f g a n, Env))
        f env l = fmap g (runRN l env) where
            g :: (CarrierRN' f g a ('S n), Env) -> Prog f g (CarrierRN' f g a n, Env)
            g (CS runRN', env') = runRN' env'

    p :: (Functor f, Functor g) => CarrierRN f g a n -> CarrierRN f g a ('S n)
    p (RN runRN) = RN $ \env -> return (CS runRN, env)

handleRename :: (Functor f, Functor g) => Prog (Rename :+: f) (Local :+: g) a -> Prog f g a
handleRename prog = do
    (CZ prog', _) <- runRN (run genRN algRN prog) emptyEnv
    return prog'
