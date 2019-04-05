
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

data DocCarrier n = D (Doc (Carrier' n))

data Carrier' :: Nat -> * where
    CZ :: Carrier' 'Z
    CS :: Doc (Carrier'  n) -> Carrier' ('S n)
    -- Required because not all parts of AST have continuations. Therefore,
    -- they have nothing to produce a value of type Carrier' a n.
    CN :: Doc () -> Carrier' n

instance Pretty Ident where
    pretty = text

instance OpAlg (VarExp Ident) DocCarrier where
    alg (GetVar v) = D (return $ CN (text v))

instance OpAlg AExp DocCarrier where
    alg (Num n)           = D $ return $ CN (showable n)
    alg (Add (D x) (D y)) = D $ parens (do x; text " + "; y)
    alg (Sub (D x) (D y)) = D $ parens (do x; text " - "; y)
    alg (Mul (D x) (D y)) = D $ parens (do x; text " * "; y)

instance OpAlg BExp DocCarrier where
    alg (T)               = D $ return $ CN (text "true")
    alg (F)               = D $ return $ CN (text "false")
    alg (Equ (D x) (D y)) = D $ parens (do x; text " = ";  y)
    alg (LEq (D x) (D y)) = D $ parens (do x; text " <= "; y)
    alg (And (D x) (D y)) = D $ parens (do x; text " && "; y)
    alg (Not (D x))       = D $ do text "!"; parens x

instance Pretty v => OpAlg (VarStm v) DocCarrier where
    alg (SetVar v (D x) (D k)) = D (do pretty v; text " := "; x; nl; k)

instance Pretty p => OpAlg (ProcStm p) DocCarrier where
    alg (Call fname (D k)) = D $ do text "call "; pretty fname; nl; k

instance OpAlg Stm DocCarrier where
    alg (Skip (D k))         = D $ do text "skip"; nl; k
    alg (Export (D x) (D k)) = D $ do text "export "; x; nl; k

instance ScopeAlg ScopeStm DocCarrier where
    dem (If (D b) (D t) (D e))
        = D (do
            x <- parens (do
                text "if "; b; text " then"; nl
                x <- indented t
                line "else"
                indented e
                case x of
                    (CS doc) -> return doc
                    (CN doc) -> doc >> return (return (CN empty)))
            nl
            x)

    -- dem (While (D b) (D s))
    --     = D (do
    --         parens (do
    --             text "while "; b; text " do"; nl
    --             indented s)
    --         nl)

instance (Pretty v, Pretty p) => ScopeAlg (BlockStm v p) DocCarrier where
    dem = undefined

    -- dem (Block vs ps (D b)) =
    --     let docVs = map docVarDecl  vs `sepByEnd` nl
    --         docPs = map docProcDecl ps `sepByEnd` nl
    --     in D $ do
    --         text "begin"; nl
    --         indented (do
    --             docVs
    --             docPs
    --             b)
    --         line "end"

docAST :: (OpAlg f DocCarrier, ScopeAlg g DocCarrier) => Prog f g () -> Doc ()
docAST prog =
    let gen = const (D (return CZ))
        pro (D doc) = D (return (CS doc))
    in case eval gen pro prog of
        (D doc) -> doc >> return ()

instance (OpAlg f DocCarrier, ScopeAlg g DocCarrier) => Show (Prog f g ()) where
    show = toString 0 . docAST

-- TODO: Remove example

type While v p
    = Prog (VarExp v :+: AExp :+: BExp :+: VarStm v :+: ProcStm p :+: Stm) (ScopeStm :+: BlockStm v p) ()

test :: While Ident Ident
test = do
    ifElse true (setVar "v" (num 1)) (setVar "y" (num 2))
    export (num 2)

runTest :: IO ()
runTest = do
    putStrLn (show test)
