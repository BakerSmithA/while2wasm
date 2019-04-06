
-- Pretty printing While AST using Datatypes a la Carte methods.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators, DataKinds #-}
{-# LANGUAGE FlexibleContexts, KindSignatures, GADTs #-}

module Front.Pretty
( pretty
) where

import Front.AST
import Helper.Pretty
import Helper.Alg
import Helper.Co

data Carrier n = D (Doc (Carrier' n))

data Carrier' :: Nat -> * where
    CZ :: Carrier' 'Z
    CS :: Doc (Carrier' n) -> Carrier' ('S n)
    -- Required because not all parts of AST have continuations. But, they still
    -- have to produce a value of type Carrier' a n. CN acts as empty continuation
    -- which does not change nesting level.
    CN :: Carrier' n

instance Pretty Ident where
    pretty = text

instance Pretty v => OpAlg (VarExp v) Carrier where
    -- TODO: Investigate more
    -- Because GetVar has no continuation, CN is returned. This is an empty
    -- carrier which does not change the 'level' of nesting. `text v` is
    -- not returned in the CN constructor because this would cause the text
    -- to render in the wrong place.
    --
    -- CN is used as the continuation, but because GetVar has no continuation
    -- then CN is empty.
    alg (GetVar v) = D $ do pretty v; return CN

instance OpAlg AExp Carrier where
    alg (Num n)           = D $ do showable n; return CN
    alg (Add (D x) (D y)) = D $ parens (do x; text " + "; y)
    alg (Sub (D x) (D y)) = D $ parens (do x; text " - "; y)
    alg (Mul (D x) (D y)) = D $ parens (do x; text " * "; y)

instance OpAlg BExp Carrier where
    alg (T)               = D $ do text "true"; return CN
    alg (F)               = D $ do text "false"; return CN
    alg (Equ (D x) (D y)) = D $ parens (do x; text " = ";  y)
    alg (LEq (D x) (D y)) = D $ parens (do x; text " <= "; y)
    alg (And (D x) (D y)) = D $ parens (do x; text " && "; y)
    alg (Not (D x))       = D $ do text "!"; parens x

instance Pretty v => OpAlg (VarStm v) Carrier where
    alg (SetVar v (D x) (D k)) = D (do pretty v; text " := "; x; nl; k)

instance Pretty p => OpAlg (ProcStm p) Carrier where
    alg (Call fname (D k)) = D $ do text "call "; pretty fname; nl; k

instance OpAlg Stm Carrier where
    alg (Skip (D k))         = D $ do text "skip"; nl; k
    alg (Export (D x) (D k)) = D $ do text "export "; x; nl; k

demDoc :: Carrier' ('S n) -> Doc (Carrier' n)
demDoc (CS doc) = doc
demDoc (CN)     = return CN

instance ScopeAlg ScopeStm Carrier where
    dem (If (D b) (D t) (D e)) = D (do
        k <- parens (do
            text "if "; b; text " then"; nl
            indented t
            line "else"
            k <- indented e
            -- Return the demoted document, If use `demDoc k` without
            -- returning the continuation is written inside the parenthesis.
            -- By using return, it can be given to outside the parenthesis,
            -- and then written out.
            return (demDoc k))
        nl
        k)

    dem (While (D b) (D s)) = D (do
        k <- parens (do
            text "while "; b; text " do"; nl
            k <- indented s
            return (demDoc k))
        nl
        k)

instance (Pretty v, Pretty p) => ScopeAlg (BlockStm v p) Carrier where
    dem (Block vs ps (D body)) = D (do
        let docVs = map docVarDecl  vs `sepByEnd` nl
            docPs = map docProcDecl ps `sepByEnd` nl

        text "begin"; nl
        k <- indented (do
            docVs
            docPs
            k <- body
            return (demDoc k))
        line "end"
        k)

-- Throws away continuation because the continuation is the same as that of the
-- procedure body. Therefore, do not need to use `demDoc` which unwraps the
-- continuation after the nested continuation so it can be run.
docVarDecl :: Pretty v => (v, Carrier ('S n)) -> Doc ()
docVarDecl (v, D x) = do text "var "; pretty v; text " := "; x; text ";"

docProcDecl :: Pretty p => (p, Carrier ('S n)) -> Doc ()
docProcDecl (f, (D body)) = do
    text "proc "; pretty f; text " is ";
    parens (do nl; indented body)
    text ";"

docAST :: (OpAlg f Carrier, ScopeAlg g Carrier) => Prog f g () -> Doc ()
docAST prog =
    let gen = const (D (return CZ))
        pro (D doc) = D (return (CS doc))
    in case eval gen pro prog of
        (D doc) -> doc >> return ()

instance (OpAlg f Carrier, ScopeAlg g Carrier) => Show (Prog f g ()) where
    show = toString 0 . docAST
