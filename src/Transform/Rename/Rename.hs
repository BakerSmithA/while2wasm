
-- Renames variables in AST to be represented using unique numbers instead of
-- strings.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, TypeOperators, FlexibleContexts  #-}

module Transform.Rename.Rename
( FreshName
, RenameErr(..)
, renameAST
) where

import Front.AST
import Transform.Rename.RenameEff
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Void
import Helper.Eff.Exception

-- To allow two versions of Fresh and Rename to exist in context, i.e. for
-- variables and procedures, need to be able to distinguish between them.
newtype VarName  = VarName  Ident deriving (Eq, Ord, Show)
newtype ProcName = ProcName Ident deriving (Eq, Ord, Show)

-- Error that can occur while renaming. Used with Exception effect-handler.
data RenameErr = UndefinedProc Ident deriving (Eq, Show)

-- Context in which renaming is performed. This provides fresh names and scope
-- for renaming. Also provides execptions, which are thrown if a procedure has
-- not been seen before.
type Op      = Fresh  VarName :+: Fresh  ProcName :+: Throw RenameErr :+: Void
type Sc      = Rename VarName :+: Rename ProcName :+: Catch RenameErr :+: Void
type Carrier = Prog Op Sc

instance VarExp FreshName :<: f => FreeAlg (VarExp Ident) (Carrier (Free f a)) where
    alg (GetVar v) = do
        v' <- fresh (VarName v)
        return (getVar v')

instance AExp :<: f => FreeAlg AExp (Carrier (Free f a)) where
    alg (Num n)   = return (num n)
    alg (Add x y) = add <$> x <*> y
    alg (Sub x y) = sub <$> x <*> y
    alg (Mul x y) = mul <$> x <*> y

instance BExp :<: f => FreeAlg BExp (Carrier (Free f a)) where
    alg (T)       = return true
    alg (F)       = return false
    alg (Equ x y) = equ  <$> x <*> y
    alg (LEq x y) = leq  <$> x <*> y
    alg (And x y) = andB <$> x <*> y
    alg (Not x)   = notB <$> x

instance VarStm FreshName :<: f => FreeAlg (VarStm Ident) (Carrier (Free f a)) where
    alg (SetVar v x) = do
        v' <- fresh (VarName v)
        rnX <- x
        return (setVar v' rnX)

-- Renames procedure names and checks that the procedure has been seen before.
-- Acts as a demonstration of execeptions.
instance ProcStm FreshName :<: f => FreeAlg (ProcStm Ident) (Carrier (Free f a)) where
    alg (Call pname) = do
        seenBefore <- exists (ProcName pname)
        if seenBefore
            then do
                pname' <- fresh (ProcName pname)
                return (call pname')
            else
                throw (UndefinedProc pname)

instance Stm :<: f => FreeAlg Stm (Carrier (Free f a)) where
    alg (Skip)            = return skip
    alg (Export x)        = export <$> x
    alg (If cond t e)     = ifElse <$> cond <*> t <*> e
    alg (While cond body) = while  <$> cond <*> body
    alg (Comp s1 s2)      = comp   <$> s1   <*> s2

instance BlockStm FreshName FreshName :<: f => FreeAlg (BlockStm Ident Ident) (Carrier (Free f a)) where
    alg (Block varDecls procDecls body) = do
        rename (map VarName (fsts varDecls)) $ do
            rename (map ProcName (fsts procDecls)) $ do
                rnVarDecls  <- map2M (fresh . VarName)  id varDecls
                rnProcDecls <- map2M (fresh . ProcName) id procDecls
                rnBody      <- body
                return (block rnVarDecls rnProcDecls rnBody)

makeRename :: (Functor g, FreeAlg f (Carrier (Free g a))) => Free f a -> Carrier (Free g a)
makeRename = evalF (return . return)

-- Returns the next fresh procedure name, used as the name of the main function.
renameAST :: (Functor g, FreeAlg f (Carrier (Free g a))) => Free f a -> Either RenameErr (Free g a, FreshName)
renameAST prog =
    let handle = handleVoid . handleExc . handleRename . handleRename
    in case handle (makeRename prog) of
        Left  err                    -> Left err
        Right ((prog', _), nextProc) -> Right (prog', nextProc)
