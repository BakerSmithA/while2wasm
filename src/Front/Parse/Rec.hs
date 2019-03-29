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

instance (A.AExp :<: f) => Progable AExp f g where
    prog = undefined
