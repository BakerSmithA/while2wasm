
-- Datatypes a la Carte methods are used to create instances of typeclasses
-- which describe how to fold over the AST to create the new AST.
--
-- An advantage of this method is it is easy to add new mappings from syntax
-- by creating a new typeclass instance. Also, the domain being mapped to will
-- not change, meaning there are no problems with needing to implement these
-- typeclasses for slightly different domains.
--
-- A disadvantage is the mapping needs to be specified for every datatype
-- that will be mapped over into the tree. Even if no renaming occurs, such as
-- is the case for AExp.

{-# LANGUAGE TypeOperators, DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE Rank2Types #-}

module Transform.Rename.AST where

import Control.Monad (liftM2)
import Front.AST
import Transform.Rename.RenameEff
import Helper.Alg
import Helper.Co
import Helper.Eff.Void
import Helper.Eff.Exception

-- TODO: Remove
import Front.Pretty

type Op  = Rename    Ident :+: Void
type Sc  = LocalName Ident :+: Void
type Hdl = Prog Op Sc

-- The renaming handler Hdl, should be a global scope around the While AST.
-- Therefore making renaming consistent across different scopes. Therefore,
-- Hdl wraps the AST.
data Carrier f g a n
    = Rn { runRn :: Hdl (Prog f g (Carrier' f g a n)) }

data Carrier' f g a :: Nat -> * where
    CZ :: a -> Carrier' f g a 'Z
    CS :: Prog f g (Carrier' f g a n) -> Carrier' f g a ('S n)

binOp :: (forall b . Prog f g b -> Prog f g b -> Prog f g b) -> Carrier f g a n -> Carrier f g a n -> Carrier f g a n
binOp f (Rn x) (Rn y) = Rn (liftM2 f x y)

instance VarExp FreshName :<: f => OpAlg (VarExp Ident) (Carrier f g a) where
    alg (GetVar v) = Rn $ do
        v' <- name v
        return (getVar v')

instance (Functor f, Functor g, AExp :<: f) => OpAlg AExp (Carrier f g a) where
    alg (Num n)   = Rn $ return (num n)
    alg (Add x y) = binOp add x y
    alg (Sub x y) = binOp sub x y
    alg (Mul x y) = binOp mul x y

instance BExp :<: f => OpAlg BExp (Carrier f g a) where
    alg (T)          = Rn $ return true
    alg (F)          = Rn $ return false
    alg (Equ x y)    = binOp equ x y
    alg (LEq x y)    = binOp leq x y
    alg (And x y)    = binOp andB x y
    alg (Not (Rn x)) = Rn $ do x' <- x; return (notB x')

instance (Functor f, Functor g, VarStm FreshName :<: f) => OpAlg (VarStm Ident) (Carrier f g a) where
    alg (SetVar v (Rn x) (Rn k)) = Rn $ do
        v' <- name v
        x' <- x
        k' <- k
        return (do setVar v' x'; k')

instance (Functor f, Functor g, ProcStm Ident :<: f) => OpAlg (ProcStm Ident) (Carrier f g a) where
    alg (Call pname (Rn k)) = Rn $ do
        k' <- k
        return (do call pname; k')

instance (Functor f, Functor g, Stm :<: f) => OpAlg Stm (Carrier f g a) where
    alg (Skip (Rn k)) = Rn $ do
        k' <- k
        return (do skip; k')
    alg (Export (Rn x) (Rn k)) = Rn $ do
        x' <- x
        k' <- k
        return (do export x'; k')

instance (Functor f, Functor g, ScopeStm :<: g) => ScopeAlg ScopeStm (Carrier f g a) where
    dem (If (Rn b) (Rn t) (Rn e)) = Rn $ do
        b' <- b; t' <- t; e' <- e
        return (do (CS k) <- ifElse b' t' e'; k)

    dem (While (Rn b) (Rn s)) = Rn $ do
        b' <- b; s' <- s
        return (do (CS k) <- while b' s'; k)

instance (Functor f, Functor g, BlockStm FreshName Ident :<: g)
    => ScopeAlg (BlockStm Ident Ident) (Carrier f g a) where

    dem (Block vs ps (Rn body)) = Rn (do
        -- TODO
        -- Continuation needs to occur outside `localNames` so names in
        -- continuation are not given incorrect names.

        (vs', ps', b') <- localNames (fsts vs) (do
            vs' <- mapM fv vs
            ps' <- mapM fp ps
            body' <- body
            return (vs', ps', body'))

        return (do (CS k) <- block vs' ps' b'; k))

fv :: (Ident, Carrier f g a ('S n)) -> Prog Op Sc (FreshName, Prog f g (Carrier' f g a ('S n)))
fv (v, Rn x) = do
    v' <- name v
    x' <- x
    return (v', x')

fp :: (Ident, Carrier f g a ('S n)) -> Prog Op Sc (Ident, Prog f g (Carrier' f g a ('S n)))
fp (pname, Rn body) = do
    body' <- body
    return (pname, body')

makeRn' :: (Functor h, Functor i)
     => (OpAlg f (Carrier h i a), ScopeAlg g (Carrier h i a))
     => Prog f g a -> Carrier h i a 'Z
makeRn' = eval gen pro where
    gen :: (Functor h, Functor i) => a -> Carrier h i a 'Z
    gen x = Rn (return (return (CZ x)))

    pro ::  (Functor h, Functor i) => Carrier h i a n -> Carrier h i a ('S n)
    pro (Rn x) = Rn $ do
        x' <- x
        return (return (CS x'))

makeRn :: (Functor h, Functor i)
     => (OpAlg f (Carrier h i a), ScopeAlg g (Carrier h i a))
     => Prog f g a -> Hdl (Prog h i a)
makeRn prog = case makeRn' prog of
    (Rn x) -> fmap (fmap (\(CZ x) -> x)) x where

handle :: Hdl (Prog h i a) -> Prog h i a
handle = handleVoid . handleRename

rename :: (Functor h, Functor i)
       => (OpAlg f (Carrier h i a), ScopeAlg g (Carrier h i a))
       => Prog f g a -> Prog h i a
rename = handle . makeRn

--------------------------------------------------------------------------------
-- Examples
--------------------------------------------------------------------------------

-- TODO: Remove

type WOp    v p = VarExp v :+: AExp :+: BExp :+: VarStm v :+: ProcStm p :+: Stm
type WScope v p = ScopeStm :+: BlockStm v p
type While  v p = Prog (WOp v p) (WScope v p) ()
type IWhile     = While Ident Ident
type FWhile     = While FreshName Ident

test :: IWhile
test = do
    setVar "x" (num 1)
    setVar "y" (num 2)
    setVar "x" (add (getVar "x") (getVar "y"))

testSc :: IWhile
testSc = do
    while true (do
        setVar "x" (num 1))
    setVar "y" (num 2)
    setVar "x" (num 2)

testBlock :: IWhile
testBlock = do
    setVar "x" (num 0)
    block [("x", num 1)] [("p", setVar "y" (num 2))] skip
    setVar "x" (num 3)

runTest :: IWhile -> IO ()
runTest ast = do
    let ast' = rename ast :: FWhile
    putStrLn "-- Before --"
    putStrLn (show ast)
    putStrLn "-- After --"
    putStrLn (show ast')
