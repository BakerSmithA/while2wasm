-- Recursively defined While AST used to parse into.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Front.Parse.Rec
( Ident
, AExp(..)
, BExp(..)
, Assign(..)
, VarDecls
, ProcDecls
, Stm(..)
) where

import qualified Front.AST as A
import Helper.Free.Free
import Helper.Co

type Ident = String

-- Arithmetic expressions.
data AExp
    = Num Integer
    | GetVar Ident
    | GetElem Ident AExp
    | Add AExp AExp
    | Sub AExp AExp
    | Mul AExp AExp
    deriving (Eq, Show)

-- Boolean expressions
data BExp
    = T
    | F
    | Equ AExp AExp
    | LEq AExp AExp
    | And BExp BExp
    | Not BExp
    deriving (Eq, Show)

data Assign
    = AssignArr  [AExp]
    | AssignAExp AExp
    deriving (Eq, Show)

-- Block containing local variable and procedure declarations.
type VarDecls  = [(Ident, Assign)]
type ProcDecls = [(Ident, Stm)]

-- Statements which can be used to change the state of the program.
data Stm
    = Skip
    | SetVar Ident Assign
    | SetElem Ident AExp AExp
    | Comp Stm Stm
    | If BExp Stm Stm
    | While BExp Stm
    | Export AExp
    | Call Ident
    | Block VarDecls ProcDecls Stm
    deriving (Eq, Show)

instance (Functor f, A.VarExp Ident :<: f, A.AExp :<: f) => Freeable AExp f where
    free (Num n)       = A.num n
    free (GetVar v)    = A.getVar v
    free (GetElem v i) = A.getElem v (free i)
    free (Add x y)     = A.add (free x) (free y)
    free (Sub x y)     = A.sub (free x) (free y)
    free (Mul x y)     = A.mul (free x) (free y)

instance (Functor f, A.VarExp Ident :<: f, A.AExp :<: f, A.BExp :<: f) => Freeable BExp f where
    free (T)       = A.true
    free (F)       = A.false
    free (Equ x y) = A.equ  (free x) (free y)
    free (LEq x y) = A.leq  (free x) (free y)
    free (And x y) = A.andB (free x) (free y)
    free (Not x)   = A.notB (free x)

instance (Functor f, A.VarExp Ident :<: f, A.AExp :<: f, A.Assign :<: f) => Freeable Assign f where
    free (AssignAExp x) = A.assignAExp (free x)
    free (AssignArr xs) = A.assignArr (map free xs)

instance (Functor f,
          A.VarExp Ident :<: f, A.AExp :<: f, A.BExp :<: f,
          A.Assign :<: f, A.VarStm Ident :<: f, A.ProcStm Ident :<: f, A.Stm :<: f,
          A.BlockStm Ident Ident :<: f)
      => Freeable Stm f where
    free (Skip)          = A.skip
    free (SetVar v x)    = A.setVar v (free x)
    free (SetElem v i x) = A.setElem v (free i) (free x)
    free (Comp s1 s2)    = A.comp (free s1) (free s2)
    free (If b t e)      = A.ifElse (free b) (free t) (free e)
    free (While b s)     = A.while (free b) (free s)
    free (Export x)      = A.export (free x)
    free (Call fname)    = A.call fname
    free (Block vs ps s) = A.block vs' ps' (free s) where
        vs' = A.mapSnd free vs
        ps' = A.mapSnd free ps
