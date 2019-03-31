
-- While AST represented using Prog tree.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}

module Front.AST where

import Helper.Co

-- Type of variables.
type Ident = String

-- Expressions over variables (i.e. produces a value), where variable names are
-- represented as strings.
data VarExp v k
    = GetVar v
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

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f = map (\(x, y) -> (x, f y))

map2M :: Monad m => (a -> m c) -> (b -> m d) -> [(a, b)] -> m [(c, d)]
map2M f g = mapM $ \(x, y) -> do
    x' <- f x
    y' <- g y
    return (x', y')

-- Statements involving variables, where variable names are represented as strings.
data VarStm v k
    = SetVar v k k
    deriving Functor

-- Statements regarding procedures, where procedure names are represented
-- as strings.
data ProcStm p k
    = Call p k
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
data BlockStm v p k
    = Block (VarDecls v k) (ProcDecls p k) k
    deriving Functor

-- Smart Constructors

-- IVarExp

getVar :: VarExp v :<: f => v -> Prog f g a
getVar v = inject (GetVar v)

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

setVar :: VarStm v :<: f => v -> Prog f g () -> Prog f g ()
setVar v x = inject (SetVar v x (Var ()))

-- IProcStm

call :: ProcStm p :<: f => p -> Prog f g ()
call func = inject (Call func (Var ()))

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

block :: (Functor f, BlockStm v p :<: g) => [(v, Prog f g ())] -> [(p, Prog f g ())] -> Prog f g () -> Prog f g ()
block vs ps b = injectS (fmap (fmap return) (Block vs ps b))
