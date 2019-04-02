
-- Scoped effect handler to generate Map from variables to whether they are
-- pointers or values.

{-# LANGUAGE DataKinds, KindSignatures, GADTs, DeriveFunctor #-}

module Transform.Captures.CaptureEff where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Helper.Prog
import Helper.Pretty

-- Different way variables can be stored.
data Var v
    -- Stored on the stack.
    = Ptr v
    -- Not captured and modified by any procedures and so can be stored as a
    -- plain value.
    | Val v
    deriving (Functor, Eq, Ord, Show)

varName :: Var v -> v
varName (Ptr v) = v
varName (Val v) = v

instance Pretty v => Pretty (Var v) where
    pretty (Ptr v) = do text "ptr."; pretty v
    pretty (Val v) = do text "val."; pretty v

-- Whether a variable is a pointer or plain value. This is used to modify
-- While AST to contain this data, instead of an external Map.
data VarType
    = PtrType
    | ValType
    deriving (Show, Eq, Ord)

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

-- Like writer monad. Tells environment about the types of variables that have
-- been 'seen' so it can update the mapping from variable names to their type.
data Seen v k
    -- Saw a variable on the left hand side of an assignment.
    = Set v k
    -- Saw a variable used in an expression, but not assigned to.
    | Used v k
    deriving Functor

data Scope v k
    -- Enter scope of a procedure. If a variable is captured and modified inside
    -- here, then it should be a pointer.
    = ProcScope k
    -- Enter scope of a block, with variables local to it.
    | BlockScope [v] k
    deriving Functor

type Capture v a = Prog (Seen v) (Scope v) a

-- Saw a variable on the left hand side of an assignment.
set :: v -> Capture v ()
set v = Op (Set v (Var ()))

-- Saw a variable used in an expression, but not assigned to.
used :: v -> Capture v ()
used v = Op (Used v (Var ()))

-- If a variable is captured and modified inside c, then said variable should be
-- a pointer.
procScope :: Capture v () -> Capture v ()
procScope c = Scope (fmap (fmap return) (ProcScope c))

-- Sets up scoped c to have given local variables.
blockScope :: [v] -> Capture v () -> Capture v ()
blockScope vs c = Scope (fmap (fmap return) (BlockScope vs c))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- Returns whether a variable was declared in the current scope.
type DeclaredAtScope v = v -> Bool
-- Mapping from variable names to their type, either value or pointer.
type VarTypes v = Map v VarType

type Env v = (DeclaredAtScope v, VarTypes v)

-- Initially, all variables are declared at this scope because we are in the
-- main function. Once a block is entered, this will be updated.
emptyEnv :: Env v
emptyEnv = (const True, Map.empty)

-- If v is already present, do nothing - if v is a pointer then do not want
-- to change it to being a plain value.
tryUpdateToVal :: Ord v => v -> Env v -> Env v
tryUpdateToVal v (declAtScope, vs) = (declAtScope, Map.insertWith keep v ValType vs) where
    keep _ oldType = oldType

-- TODO: Optimisation
-- Only variables which have their value set in two different scopes will be
-- be made pointers. Therefore, a variable could be global, but it will be
-- called 'val' because its value is not propagated outside the scope.
updateSet :: Ord v => v -> Env v -> Env v
updateSet v env@(declAtScope, vs) | declAtScope v = tryUpdateToVal v env -- It may have been set at an inner scope.
                                  | otherwise     = (declAtScope, Map.insert v PtrType vs)

-- Updates environment to reflect that a variable has been used, v.e. its added
-- as a local variable if it is not already a pointer.
updateUsed :: Ord v => v -> Env v -> Env v
updateUsed v env = tryUpdateToVal v env

-- Updates variables at this scope in the environment to contain those defined
-- by `atScope` **only**.
setVarDecls :: DeclaredAtScope v -> Env v -> Env v
setVarDecls atScope (_, vs) = (atScope, vs)

-- Updates variables at the scope to contain the list of variables, as well
-- as any variables already in scope. These variables are also added to be
-- of value type to the mapping (this may be updated though).
addVarDeclsFromLocals :: Ord v => [v] -> Env v -> Env v
addVarDeclsFromLocals decls (atAboveScope, vs) = (atScope, vs') where
    atScope v = atAboveScope v || (v `Set.member` Set.fromList decls)
    vs' = foldr (\v acc -> Map.insert v ValType acc) vs decls

-- Returns environment with mapping from `new` but varDecls from `old`.
restoreDecls :: Ord v => Env v -> Env v -> Env v
restoreDecls (varDecls, _) (_, vs) = (varDecls, vs)

-- Carrier is nested state transformers Env -> (a, Env)
data Carrier v a n
    = C { runC :: Env v -> (Carrier' v a n, Env v) }

data Carrier' v a :: Nat -> * where
    CZ :: a -> Carrier' v a 'Z
    CS :: (Env v -> (Carrier' v a n, Env v)) -> Carrier' v a ('S n)

genC :: a -> Carrier v a 'Z
genC a = C $ \env -> (CZ a, env)

algC :: Ord v => Alg (Seen v) (Scope v) (Carrier v a)
algC = A a d p where
    a :: Ord v => Seen v (Carrier v a n) -> Carrier v a n
    a (Set v k)  = C $ \env -> runC k (updateSet v env)
    a (Used v k) = C $ \env -> runC k (updateUsed v env)

    d :: Ord v => Scope v (Carrier v a ('S n)) -> Carrier v a n
    d (ProcScope k) = C $ \env ->
        -- Remove any variables declared at the scope.
        case runC k (setVarDecls (const False) env) of
            (CS runC', env') -> runC' (restoreDecls env env')

    d (BlockScope vs k) = C $ \env ->
        -- Add variables declared at the scope to those already in scope.
        -- Because blocks do not become separate functions and so variables used
        -- within them can be values.
        case runC k (addVarDeclsFromLocals vs env) of
            (CS runC', env') -> runC' (restoreDecls env env')

    p :: Ord v => Carrier v a n -> Carrier v a ('S n)
    p c = C $ \env -> (CS (runC c), env)

runCapture :: Ord v => Capture v a -> Map v VarType
runCapture c = case (runC (run genC algC c) emptyEnv) of
    (_, (_, mapping)) -> mapping
