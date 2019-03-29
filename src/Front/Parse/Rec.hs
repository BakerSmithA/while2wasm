-- Recursively defined While AST used to parse into.

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Front.Parse.Rec where

import qualified Front.AST as A
import Helper.Prog
import Helper.Co

type Ident = String

-- Arithmetic expressions.
data AExp
    = Num Integer
    | Ident Ident
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

-- Block containing local variable and procedure declarations.
type VarDecls  = [(Ident, AExp)]
type ProcDecls = [(Ident, Stm)]

-- Statements which can be used to change the state of the program.
data Stm
    = Skip
    | Assign Ident AExp
    | Comp Stm Stm
    | If BExp Stm Stm
    | While BExp Stm
    | Export AExp
    | Call Ident
    | Block VarDecls ProcDecls Stm
    deriving (Eq, Show)

instance (A.IVarExp :<: f, A.AExp :<: f)
        => Progable AExp f g where
    prog (Num n)   = A.num n
    prog (Ident v) = A.getIVar v
    prog (Add x y) = A.add (prog x) (prog y)
    prog (Sub x y) = A.sub (prog x) (prog y)
    prog (Mul x y) = A.mul (prog x) (prog y)

instance (A.IVarExp :<: f, A.AExp :<: f, A.BExp :<: f)
        => Progable BExp f g where
    prog (T)       = A.true
    prog (F)       = A.false
    prog (Equ x y) = A.equ  (prog x) (prog y)
    prog (LEq x y) = A.leq  (prog x) (prog y)
    prog (And x y) = A.andB (prog x) (prog y)
    prog (Not x)   = A.notB (prog x)

instance ( A.IVarExp :<: f, A.AExp :<: f, A.BExp :<: f
         , A.IVarStm :<: f, A.IProcStm :<: f, A.Stm :<: f
         , A.ScopeStm :<: g, A.IBlockStm :<: g )
         => Progable Stm f g where
    prog (Skip)          = A.skip
    prog (Assign v x)    = A.setVar v (prog x)
    prog (Comp s1 s2)    = do prog s1; prog s2
    prog (If b t e)      = A.ifElse (prog b) (prog t) (prog e)
    prog (While b s)     = A.while (prog b) (prog s)
    prog (Export x)      = A.export (prog x)
    prog (Call fname)    = A.call fname
    prog (Block vs ps s) = A.block vs' ps' (prog s) where
        vs' = A.mapVs prog vs
        ps' = A.mapPs prog ps