
-- Renames variables in AST to be represented using unique numbers instead of
-- strings.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts  #-}

module Transform.Rename
( FreshName(..)
, rename
) where

import Front.AST
import Transform.Rename.RenameEff
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff
import Helper.Eff.Void

import Front.Pretty
import Helper.Pretty

-- Context in which renaming is performed. This provides fresh names and scope
-- for renaming.
type Op  = Fresh  Ident :+: Void
type Sc  = Rename Ident :+: Void
type Ctx = Prog Op Sc

instance VarExp FreshName :<: f => FreeAlg (VarExp Ident) (Ctx (Free f a)) where
    alg (GetVar v) = do
        v' <- fresh v
        return (getVar v')

instance AExp :<: f => FreeAlg AExp (Ctx (Free f a)) where
    alg = undefined

instance BExp :<: f => FreeAlg BExp (Ctx (Free f a)) where
    alg = undefined

instance VarStm FreshName :<: f => FreeAlg (VarStm Ident) (Ctx (Free f a)) where
    alg (SetVar v x) = do
        v' <- fresh v
        rnX <- x
        return (setVar v' rnX)

instance ProcStm FreshName :<: f => FreeAlg (ProcStm Ident) (Ctx (Free f a)) where
    alg = undefined

instance Stm :<: f => FreeAlg Stm (Ctx (Free f a)) where
    alg (While cond body) = while <$> cond <*> body
    alg (Comp s1 s2)      = comp <$> s1 <*> s2

instance BlockStm FreshName FreshName :<: f => FreeAlg (BlockStm Ident Ident) (Ctx (Free f a)) where
    alg = undefined

makeRename :: (Functor g, FreeAlg f (Ctx (Free g a))) => Free f a -> Ctx (Free g a)
makeRename = evalF (return . return)

renameAST :: (Functor g, FreeAlg f (Ctx (Free g a))) => Free f a -> Free g a
renameAST = handleVoid . handleRename . makeRename

test :: While Ident Ident
test = do
    while (getVar "x") (do
        setVar "x" (getVar "y"))

runTest :: IO ()
runTest = do
    let rn = renameAST test :: While FreshName FreshName
    putStrLn (toString 0 $ docAST test)
    putStrLn ("-- After-- ")
    putStrLn (toString 0 $ docAST rn)

-- import Transform.Rename.AST
-- import Transform.Rename.RenameEff
-- import Helper.Scope.Prog
-- import Helper.Scope.Alg
-- import Helper.Eff.Void
-- import Helper.Eff.Exception
--
-- -- Use extensible effects methods to run handlers.
-- handle :: RenameHandler h i -> Either String (Prog h i ())
-- handle = handleVoid . handleRename . handleRename . handleExc
--
-- rename :: (Functor h, Functor i)
--        => (OpAlg f (Carrier h i), ScopeAlg g (Carrier h i))
--        => Prog f g () -> Either String (Prog h i ())
-- rename = handle . makeRn
