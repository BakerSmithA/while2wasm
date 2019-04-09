
-- While AST represented using Prog tree.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}

module Front.AST where

import Helper.Free.Free
import Helper.Co

-- Type of variables.
type Ident = String

-- Expressions over variables (i.e. produces a value), where variable names are
-- represented as strings.
data VarExp v k
    = GetVar v
    deriving (Functor, Show)

data AExp k
    = Num Integer
    | Add k k
    | Sub k k
    | Mul k k
    deriving (Functor, Show)

data BExp k
    = T
    | F
    | Equ k k
    | LEq k k
    | And k k
    | Not k
    deriving (Functor, Show)

type VarDecls  v k = [(v, k)]
type ProcDecls v k = [(v, k)]

fsts :: [(a, b)] -> [a]
fsts = fst . unzip

mapSnd :: (b -> c) -> [(a, b)] -> [(a, c)]
mapSnd f = map (\(x, y) -> (x, f y))

map2M :: Monad m => (a -> m c) -> (b -> m d) -> [(a, b)] -> m [(c, d)]
map2M f g = mapM $ \(x, y) -> do
    x' <- f x
    y' <- g y
    return (x', y')

-- Statements involving variables, where variable names are represented as strings.
-- Compared to when using Prog, statements no longer have their own continuation.
-- Instead, Comp is used to compose statements
data VarStm v k
    = SetVar v k
    deriving (Functor, Show)

-- Statements regarding procedures, where procedure names are represented
-- as strings.
data ProcStm p k
    = Call p
    deriving (Functor, Show)

data Stm k
    = Skip
    | Export k -- Export a variable to calling JS.
    | If k k k
    | While k k
    | Comp k k
    deriving (Functor, Show)

-- Block with local variable and procedure declarations, where the names of
-- variables and procedures are represented as strings.
data BlockStm v p k
    = Block (VarDecls v k) (ProcDecls p k) k
    deriving (Functor, Show)

-- Smart Constructors

-- IVarExp

getVar :: VarExp v :<: f => v -> Free f a
getVar v = injectF (GetVar v)

-- AExp

num :: AExp :<: f => Integer -> Free f a
num n = injectF (Num n)

add :: AExp :<: f => Free f a -> Free f a -> Free f a
add x y = injectF (Add x y)

sub :: AExp :<: f => Free f a -> Free f a -> Free f a
sub x y = injectF (Sub x y)

mul :: AExp :<: f => Free f a -> Free f a -> Free f a
mul x y = injectF (Mul x y)

-- BExp

true :: BExp :<: f => Free f a
true = injectF T

false :: BExp :<: f => Free f a
false = injectF F

equ :: BExp :<: f => Free f a -> Free f a -> Free f a
equ x y = injectF (Equ x y)

leq :: BExp :<: f => Free f a -> Free f a -> Free f a
leq x y = injectF (LEq x y)

andB :: BExp :<: f => Free f a -> Free f a -> Free f a
andB x y = injectF (And x y)

notB :: BExp :<: f => Free f a -> Free f a
notB x = injectF (Not x)

-- IVarStm

setVar :: VarStm v :<: f => v -> Free f a -> Free f a
setVar v x = injectF (SetVar v x)

-- IProcStm

call :: ProcStm p :<: f => p -> Free f a
call func = injectF (Call func)

-- Stm

skip = injectF Skip
skip :: Stm :<: f => Free f a

export :: Stm :<: f => Free f a -> Free f a
export x = injectF (Export x)

comp :: Stm :<: f => Free f a -> Free f a -> Free f a
comp s1 s2 = injectF (Comp s1 s2)

ifElse :: Stm :<: f => Free f a -> Free f a -> Free f a -> Free f a
ifElse b t e = injectF (If b t e)

while :: Stm :<: f => Free f a -> Free f a -> Free f a
while b s = injectF (While b s)

-- IIBlockStm

block ::  BlockStm v p :<: f => [(v, Free f a)] -> [(p, Free f a)] -> Free f a -> Free f a
block varDecls procDecls body = injectF (Block varDecls procDecls body)
