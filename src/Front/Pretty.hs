
-- Pretty printing While AST using Datatypes a la Carte methods.
-- Uses continuation passing style to describe pretty printing in imperative style.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}

module Front.Pretty (docAST) where

import Front.AST
import Helper.Pretty
import Helper.Free.Free
import Helper.Free.Alg

instance Pretty Ident where
    pretty = text

instance Pretty v => FreeAlg (VarExp v) (Doc ()) where
    alg (GetVar v)    = pretty v
    alg (GetElem v i) = pretty v >> brackets i

instance FreeAlg AExp (Doc ()) where
    alg (Num n)    = showable n
    alg (Add x y)  = parens (do x; text " + "; y)
    alg (Sub x y)  = parens (do x; text " - "; y)
    alg (Mul x y)  = parens (do x; text " * "; y)

instance FreeAlg BExp (Doc ()) where
    alg (T)       = text "true"
    alg (F)       = text "false"
    alg (Equ x y) = parens (do x; text " = "; y)
    alg (LEq x y) = parens (do x; text " <= "; y)
    alg (And x y) = parens (do x; text " & "; y)
    alg (Not x)   = parens (do text "!"; x)

instance FreeAlg Assign (Doc ()) where
    alg (AssignAExp x) = x
    alg (AssignArr xs) = brackets (xs `sepBy` text ", ")

instance Pretty v => FreeAlg (VarStm v) (Doc ()) where
    alg (SetVar var val)      = do pretty var; text " := "; val
    alg (SetElem var idx val) = do pretty var; brackets idx; text " := "; val

instance Pretty p => FreeAlg (ProcStm p) (Doc ()) where
    alg (Call pname) = do text "call "; pretty pname

instance FreeAlg Stm (Doc ()) where
    alg (Skip)     = text "skip"
    alg (Export val) = do text "export "; val
    alg (If cond thenStm elseStm) = parens (do
        text "if "; cond; text " then"; nl
        indented thenStm; nl
        line "else"
        indented elseStm; nl)
    alg (While cond body) = parens (do
        text "while "; cond; text " do"; nl
        indented body; nl)
    -- Because printing is a side-effect, this produces correct result.
    alg (Comp s1 s2) = do s1; text ";"; nl; s2

instance (Pretty v, Pretty p) => FreeAlg (BlockStm v p) (Doc ()) where
    alg (Block varDecls procDecls body) = do
        let docVs = map docVarDecl  varDecls  `sepByEnd` nl
            docPs = map docProcDecl procDecls `sepByEnd` nl

        line "begin"
        indented (do
            docVs
            docPs
            body
            nl)
        text "end"

docVarDecl :: Pretty v => (v, Doc ()) -> Doc ()
docVarDecl (v, x) = do text "var "; pretty v; text " := "; x; text ";"

docProcDecl :: Pretty p => (p, Doc ()) -> Doc ()
docProcDecl (pname, body) = do
    text "proc "; pretty pname; text " is ";
    parens (do nl; indented body; nl)
    text ";"

docAST :: FreeAlg f (Doc ()) => Free f a -> Doc ()
docAST = evalF (const (return ()))
