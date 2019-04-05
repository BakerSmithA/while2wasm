
-- Pretty printing While AST using Datatypes a la Carte methods.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Front.Pretty
( pretty
) where

import Front.AST
import Helper.Pretty
import Helper.Alg
import Helper.Co

type DocCarrier = CarrierId (Doc ())

instance Pretty Ident where
    pretty = text

instance Pretty v => OpAlg (VarExp v) DocCarrier where
    alg (GetVar v) = Id (pretty v)

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

instance Pretty v => OpAlg (VarStm v) DocCarrier where
    alg (SetVar v (Id x) (Id k)) = Id (do pretty v; text " := "; x; nl; k)

instance Pretty p => OpAlg (ProcStm p) DocCarrier where
    alg (Call fname (Id k)) = Id $ do text "call "; pretty fname; nl; k

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

instance (Pretty v, Pretty p) => ScopeAlg (BlockStm v p) DocCarrier where
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

docVarDecl :: Pretty v => (v, DocCarrier ('S n)) -> Doc ()
docVarDecl (v, (Id x)) = do text "var "; pretty v; text " := "; x; text ";"

docProcDecl :: Pretty p => (p, DocCarrier ('S n)) -> Doc ()
docProcDecl (f, (Id body)) = do
    text "proc "; pretty f; text " is ";
    parens (do nl; indented body)
    text ";"

type While v p
    = Prog (VarExp v :+: AExp :+: BExp :+: VarStm v :+: ProcStm p :+: Stm) (ScopeStm :+: BlockStm v p) ()

evalId' :: (OpAlg f DocCarrier, ScopeAlg g DocCarrier) => (r -> DocCarrier 'Z) -> Prog f g r -> Doc ()
evalId' gen = runId gen alg' where
    alg' = A alg dem pro
    pro :: CarrierId (Doc ()) n -> CarrierId (Doc ()) ('S n)
    pro (Id x) = Id (do text " ... "; x)

docAST :: (Pretty v, Pretty p) => While v p -> Doc ()
docAST = evalId' gen where
    gen x = Id (return x)

instance (Pretty v, Pretty p) => Show (While v p) where
    show = toString 0 . docAST


-- TODO: Remove example

test :: While Ident Ident
test = do
    while true (setVar "v" (num 1))
    export (num 2)

runTest :: IO ()
runTest = do
    putStrLn (show test)
