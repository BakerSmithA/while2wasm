
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

module Transform.Identity where

import Control.Monad (liftM2)
import Front.AST
import Transform.Rename.RenameEff
import Helper.Alg
import Helper.Co
import Helper.Eff.Void
import Helper.Eff.Exception

-- TODO: Remove
import Front.Pretty

data Nest f n = Nest (f (Nest' f n))

data Nest' f :: Nat -> * where
    NZ :: a -> Nest' f 'Z
    NS :: f (Nest' f n) -> Nest' f ('S n)

-- The renaming handler Ctx, should be a global scope around the While AST.
-- Therefore making renaming consistent across different scopes. Therefore,
-- Ctx wraps the AST.
-- data Carrier f g a n
--     = Rn { runRn :: Prog f (Sc g) (Carrier' f g a n) }

-- data Carrier' f g a :: Nat -> * where
--     CZ :: a -> Carrier' f g a 'Z
--     CS :: Prog (Op f) (Sc g) (Carrier' f g a n) -> Carrier' f g a ('S n)


-- binOp :: (forall b . Prog f g b -> Prog f g b -> Prog f g b) -> Carrier f g a n -> Carrier f g a n -> Carrier f g a n
-- binOp f (Rn x) (Rn y) = Rn (f x y)


instance (Functor f, Functor g, VarExp Ident :<: f) => OpAlg (VarExp Ident) (Nest (Prog f g)) where
    alg (GetVar v) = Nest $ getVar v

-- instance (Functor f, Functor g, AExp :<: f) => OpAlg AExp (Carrier f g a) where
--     alg (Num n)   = Rn $ num n
--     alg (Add x y) = binOp add x y
--     alg (Sub x y) = binOp sub x y
--     alg (Mul x y) = binOp mul x y

-- instance (Functor f, Functor g, BExp :<: f) => OpAlg BExp (Carrier f g a) where
--     alg (T)          = Rn $ true
--     alg (F)          = Rn $ false
--     alg (Equ x y)    = binOp equ x y
--     alg (LEq x y)    = binOp leq x y
--     alg (And x y)    = binOp andB x y
--     alg (Not (Rn x)) = Rn $ notB x
--
-- instance (Functor f, Functor g, VarStm Ident :<: f) => OpAlg (VarStm Ident) (Carrier f g a) where
--     alg (SetVar v (Rn x) (Rn k)) = Rn $ do
--         -- v' <- name v
--         setVar v x
--         k
--
-- instance (Functor f, Functor g, ProcStm Ident :<: f) => OpAlg (ProcStm Ident) (Carrier f g a) where
--     alg (Call pname (Rn k)) = Rn $ do call pname; k
--
-- instance (Functor f, Functor g, Stm :<: f) => OpAlg Stm (Carrier f g a) where
--     alg (Skip (Rn k))          = Rn $ do skip; k
--     alg (Export (Rn x) (Rn k)) = Rn $ do export x; k
--
-- instance (Functor f, Functor g, BExp :<: f, ScopeStm :<: g) => ScopeAlg ScopeStm (Carrier f g a) where
--     dem (If (Rn b) (Rn t) (Rn e)) = Rn $ do
--         (CS k) <- e
--         ifElse b t e
--         k
--
--     dem (While (Rn b) (Rn s)) = Rn $ do
--         (CS k) <- while b s
--         k
--
-- instance (Functor f, Functor g, BlockStm Ident Ident :<: g)
--     => ScopeAlg (BlockStm Ident Ident) (Carrier f g a) where
--
--     dem = undefined
--
--
-- --     dem (Block vs ps (Rn body)) = Rn (do
-- --         -- TODO
-- --         -- Continuation needs to occur outside `localNames` so names in
-- --         -- continuation are not given incorrect names.
-- --
-- --         (vs', ps', b') <- localNames (fsts vs) (do
-- --             vs' <- mapM fv vs
-- --             ps' <- mapM fp ps
-- --             body' <- body
-- --             return (vs', ps', body'))
-- --
-- --
-- --
-- --         ns <- names'
-- --
-- --         -- return (do block vs' ps' b'; return CN))
-- --
-- --         return (do (CS k) <- block vs' ps' b'; k))
-- --
-- -- fv :: (Ident, Carrier f g a ('S n)) -> Prog Op Sc (FreshName, Prog f g (Carrier' f g a ('S n)))
-- -- fv (v, Rn x) = do
-- --     v' <- name v
-- --     x' <- x
-- --     return (v', x')
-- --
-- -- fp :: (Ident, Carrier f g a ('S n)) -> Prog Op Sc (Ident, Prog f g (Carrier' f g a ('S n)))
-- -- fp (pname, Rn body) = do
-- --     body' <- body
-- --     return (pname, body')
-- --
-- -- makeRn' :: (Functor h, Functor i)
-- --      => (OpAlg f (Carrier h i a), ScopeAlg g (Carrier h i a))
-- --      => Prog f g a -> Carrier h i a 'Z
-- -- makeRn' = eval gen pro where
-- --     gen :: (Functor h, Functor i) => a -> Carrier h i a 'Z
-- --     gen x = Rn (return (return (CZ x)))
-- --
-- --     pro ::  (Functor h, Functor i) => Carrier h i a n -> Carrier h i a ('S n)
-- --     pro (Rn x) = Rn $ do
-- --         x' <- x
-- --         return (return (CS x'))
-- --
-- -- makeRn :: (Functor h, Functor i)
-- --      => (OpAlg f (Carrier h i a), ScopeAlg g (Carrier h i a))
-- --      => Prog f g a -> Ctx (Prog h i a)
-- -- makeRn prog = case makeRn' prog of
-- --     (Rn x) -> fmap (fmap (\(CZ x) -> x)) x where
-- --
-- -- handle :: Ctx (Prog h i a) -> Prog h i a
-- -- handle = handleVoid . handleRename
-- --
-- -- rename :: (Functor h, Functor i)
-- --        => (OpAlg f (Carrier h i a), ScopeAlg g (Carrier h i a))
-- --        => Prog f g a -> Prog h i a
-- -- rename = handle . makeRn
--
-- -- makeRn :: (Functor f, Functor g, OpAlg f (Carrier f g a), ScopeAlg g (Carrier f g a))
-- --        => Prog f g a -> Carrier f g a 'Z
-- -- makeRn = eval gen pro where
-- --     gen :: (Functor f, Functor g) => a -> Carrier f g a 'Z
-- --     gen x = Rn (return (CZ x))
-- --
-- --     pro :: (Functor f, Functor g) => Carrier f g a n -> Carrier f g a ('S n)
-- --     pro (Rn x) = Rn (return (CS x))
--
-- makeRn :: (Functor h, Functor i, OpAlg f (Carrier h i a), ScopeAlg g (Carrier h i a))
--        => Prog f g a -> Carrier h i a 'Z
-- makeRn = eval gen pro where
--     gen :: (Functor h, Functor i) => a -> Carrier h i a 'Z
--     gen x = Rn (return (CZ x))
--
--     pro :: (Functor h, Functor i) => Carrier h i a n -> Carrier h i a ('S n)
--     pro (Rn x) = Rn (return (CS x))
--
-- makeRn' :: (Functor h, Functor i, OpAlg f (Carrier h i a), ScopeAlg g (Carrier h i a))
--        => Prog f g a -> Prog (Op h) (Sc i) a
-- makeRn' prog = case makeRn prog of
--     (Rn prog') -> do
--         (CZ x) <- prog'
--         return x
--
-- rename :: (Functor h, Functor i, OpAlg f (Carrier h i a), ScopeAlg g (Carrier h i a))
--        => Prog f g a -> Prog h i a
-- rename = handleRename . makeRn'
--
-- -- --------------------------------------------------------------------------------
-- -- -- Examples
-- -- --------------------------------------------------------------------------------

identity :: (OpAlg f (Nest (Prog f g)), ScopeAlg g (Nest (Prog f g))) => Prog f g a -> Prog f g a
identity prog = eval gen pro prog where
    gen x = return x
    pro = undefined

-- TODO: Remove

type WOp    v p = VarExp v :+: AExp :+: BExp :+: VarStm v :+: ProcStm p :+: Stm
type WScope v p = ScopeStm :+: BlockStm v p
type While  v p = Prog (WOp v p) (WScope v p) ()
type IWhile     = While Ident Ident
type FWhile     = While FreshName Ident

testVar :: IWhile
testVar = getVar "x"

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
    let ast' = rename ast :: IWhile
    putStrLn "-- Before --"
    -- TODO: Rename to pretty
    putStrLn (show ast)
    putStrLn "-- After --"
    putStrLn (show ast')
