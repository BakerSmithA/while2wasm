
-- Pretty printing While AST using Datatypes a la Carte methods.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE InstanceSigs #-}

module Front.Pretty where

import Front.AST
import Helper.Pretty
import Helper.Alg
import Helper.Co

type DocCarrier = CarrierId (Doc ())

instance OpAlg IVarExp DocCarrier where
    alg (GetVar v) = Id (text v)

instance OpAlg AExp DocCarrier where
    alg (Num n)             = Id $ showable n
    alg (Add (Id x) (Id y)) = Id $ parens (do x; text " + "; y)
    alg (Sub (Id x) (Id y)) = Id $ parens (do x; text " - "; y)
    alg (Mul (Id x) (Id y)) = Id $ parens (do x; text " * "; y)

instance OpAlg BExp DocCarrier where
    alg (T)                 = Id $ text "true"
    alg (F)                 = Id $ text "false"
    alg (Equ (Id x) (Id y)) = Id $ parens (do x; text " = ";  y)
    alg (LEq (Id x) (Id y)) = Id $ parens (do x; text " <= "; y)
    alg (And (Id x) (Id y)) = Id $ parens (do x; text " && "; y)
    alg (Not (Id x))        = Id $ do text "!"; parens x

instance OpAlg IVarStm DocCarrier where
    alg (SetVar v (Id x) (Id k)) = Id (do text v; text " := "; x; nl; k)

instance OpAlg IProcStm DocCarrier where
    alg (Call fname (Id k)) = Id $ do text "call "; text fname; nl; k

instance OpAlg Stm DocCarrier where
    alg (Skip (Id k))          = Id $ do text "skip"; nl; k
    alg (Export (Id x) (Id k)) = Id $ do text "export "; x; nl; k

instance ScopeAlg ScopeStm DocCarrier where
    dem (If (Id b) (Id t) (Id e))
        = Id (do
            parens (do
                text "if "; b; text " then"; nl
                indented t
                line "else"
                indented e)
            nl)

    dem (While (Id b) (Id s))
        = Id (do
            parens (do
                text "while "; b; text " do"; nl
                indented s)
            nl)

instance ScopeAlg IBlockStm DocCarrier where
    dem (Block vs ps (Id b)) =
        let docVs = map docVarDecl  vs `sepByEnd` nl
            docPs = map docProcDecl ps `sepByEnd` nl
        in Id $ do
            text "begin"; nl
            indented (do
                docVs
                docPs
                b)
            line "end"

docVarDecl :: (Ident, DocCarrier ('S n)) -> Doc ()
docVarDecl (v, (Id x)) = do text "var "; text v; text " := "; x; text ";"

docProcDecl :: (Ident, DocCarrier ('S n)) -> Doc ()
docProcDecl (f, (Id body)) = do
    text "proc "; text f; text " is ";
    parens (do nl; indented body)
    text ";"

type X = IVarExp :+: AExp :+: BExp :+: IVarStm :+: IProcStm :+: Stm
type Y = ScopeStm :+: IBlockStm

docAST :: Prog X Y () -> Doc ()
docAST = evalId gen where
    gen x = Id (return x)

test :: Prog X Y ()
test = do
    block
        [("x", num 1), ("y", getIVar "z")]
        [("f", call "f")]
        (while (notB false) (
            ifElse (true)
                (call "f")
                (call "g")))

runTest :: IO ()
runTest = do
    let doc = docAST test
    putStrLn (toString 0 doc)
