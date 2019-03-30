
-- While AST represented using Prog tree.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}

module Front.AST where

import Helper.Co

-- Type of variables.
type Ident = String

-- Expressions over variables (i.e. produces a value), where variable names are
-- represented as strings.
data IVarExp k
    = IGetVar Ident
    deriving Functor

data AExp k
    = Num Integer
    | Add k k
    | Sub k k
    | Mul k k
    deriving Functor

data BExp k
    = T
    | F
    | Equ k k
    | LEq k k
    | And k k
    | Not k
    deriving Functor

type VarDecls  v k = [(v, k)]
type ProcDecls v k = [(v, k)]

mapVs :: (k -> b) -> VarDecls v k -> VarDecls v b
mapVs f = map (\(v, x) -> (v, f x))

mapPs :: (k -> b) -> ProcDecls v k -> ProcDecls v b
mapPs f = map (\(v, s) -> (v, f s))

-- Statements involving variables, where variable names are represented as strings.
data IVarStm k
    = ISetVar Ident k k
    deriving Functor

-- Statements regarding procedures, where procedure names are represented
-- as strings.
data IProcStm k
    = ICall Ident k
    deriving Functor

-- Instructions with continuations.
data Stm k
    = Skip k
    | Export k k -- Export a variable to calling JS.
    deriving Functor

-- Syntax which contains other syntax.
data ScopeStm k
    = If k k k
    | While k k
    deriving Functor

-- Block with local variable and procedure declarations, where the names of
-- variables and procedures are represented as strings.
data IBlockStm k
    = IBlock (VarDecls Ident k) (ProcDecls Ident k) k
    deriving Functor

-- Smart Constructors

-- IVarExp

getIVar :: IVarExp :<: f => Ident -> Prog f g a
getIVar v = inject (IGetVar v)

-- AExp

num :: AExp :<: f => Integer -> Prog f g a
num n = inject (Num n)

add :: AExp :<: f => Prog f g a -> Prog f g a -> Prog f g a
add x y = inject (Add x y)

sub :: AExp :<: f => Prog f g a -> Prog f g a -> Prog f g a
sub x y = inject (Sub x y)

mul :: AExp :<: f => Prog f g a -> Prog f g a -> Prog f g a
mul x y = inject (Mul x y)

-- BExp

true :: BExp :<: f => Prog f g a
true = inject T

false :: BExp :<: f => Prog f g a
false = inject F

equ :: BExp :<: f => Prog f g a -> Prog f g a -> Prog f g a
equ x y = inject (Equ x y)

leq :: BExp :<: f => Prog f g a -> Prog f g a -> Prog f g a
leq x y = inject (LEq x y)

andB :: BExp :<: f => Prog f g a -> Prog f g a -> Prog f g a
andB x y = inject (And x y)

notB :: BExp :<: f => Prog f g a -> Prog f g a
notB x = inject (Not x)

-- IVarStm

setIVar :: IVarStm :<: f => Ident -> Prog f g () -> Prog f g ()
setIVar v x = inject (ISetVar v x (Var ()))

-- IProcStm

call :: IProcStm :<: f => Ident -> Prog f g ()
call func = inject (ICall func (Var ()))

-- Stm

skip :: Stm :<: f => Prog f g ()
skip = inject (Skip (Var ()))

export :: Stm :<: f => Prog f g () -> Prog f g ()
export x = inject (Export x (Var ()))

-- ScopeStm

ifElse :: (Functor f, ScopeStm :<: g) => Prog f g () -> Prog f g () -> Prog f g () -> Prog f g ()
ifElse b t e = injectS (fmap (fmap return) (If b t e))

while :: (Functor f, ScopeStm :<: g) => Prog f g () -> Prog f g () -> Prog f g ()
while b s = injectS (fmap (fmap return) (While b s))

-- IIBlockStm

block :: (Functor f, IBlockStm :<: g) => [(Ident, Prog f g ())] -> [(Ident, Prog f g ())] -> Prog f g () -> Prog f g ()
block vs ps b = injectS (fmap (fmap return) (IBlock vs ps b))
