
-- Convert WebAssembly to pretty printed WebAssembly-Text-Format (WAT).
-- This also provides an example of using Datatypes a la Carte with
-- scoped Prog. This is different from previous Effect Handlers, as using
-- typeclasses below allows easy extension of converting WASM to WAT.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, GADTs, DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Back.WAT
( docModule
) where

import Back.WASM
import Helper.Pretty
import Helper.Scope.Nest
import Helper.Scope.Alg

type Carrier = Nest Doc

docBinOp :: BinOp -> Doc ()
docBinOp (ADD) = text "i32.add"
docBinOp (SUB) = text "i32.sub"
docBinOp (MUL) = text "i32.mul"
docBinOp (AND) = text "i32.and"

docRelOp :: RelOp -> Doc ()
docRelOp (EQU) = text "i32.eq"
docRelOp (LEQ) = text "i32.le_s"

docVar :: String -> Doc ()
docVar i = text ("$" ++ i)

instance OpAlg Instr Carrier where
    alg (NOP (Nest k)) = Nest $ line "nop" >> k

instance OpAlg ArithInstr Carrier where
    alg (CONST  n  (Nest k)) = Nest $ line' (text "i32.const " >> showable n) >> k
    alg (BIN_OP op (Nest k)) = Nest $ line' (docBinOp op) >> k
    alg (REL_OP op (Nest k)) = Nest $ line' (docRelOp op) >> k

instance OpAlg VarInstr Carrier where
    alg (GET_LOCAL  name (Nest k)) = Nest $ line' (text "get_local "  >> docVar name) >> k
    alg (SET_LOCAL  name (Nest k)) = Nest $ line' (text "set_local "  >> docVar name) >> k
    alg (GET_GLOBAL name (Nest k)) = Nest $ line' (text "get_global " >> docVar name) >> k
    alg (SET_GLOBAL name (Nest k)) = Nest $ line' (text "set_global " >> docVar name) >> k

instance OpAlg MemInstr Carrier where
    alg (LOAD  offset (Nest k)) = Nest $ line' (text "i32.load offset="  >> showable offset) >> k
    alg (STORE offset (Nest k)) = Nest $ line' (text "i32.store offset=" >> showable offset) >> k

instance OpAlg BranchInstr Carrier where
    alg (BR    label (Nest k)) = Nest $ line' (text "br "    >> showable label) >> k
    alg (BR_IF label (Nest k)) = Nest $ line' (text "br_if " >> showable label) >> k
    alg (CALL  fname (Nest k)) = Nest $ line' (text "call "  >> docVar fname)   >> k
    alg (RET         (Nest k)) = Nest $ line' (text "return") >> k

instance ScopeAlg ControlInstr Carrier where
    dem (BLOCK (Nest body)) = Nest $ do
        line "block"
        (NS k) <- indented body
        line "end"
        k

    -- NOTE, there is some redundancy here as the continuation k could be
    -- taken from either the 'then' or 'else' branch.
    dem (IF (Nest t) (Nest e)) = Nest $ do
        line "if"
        indented t
        line "else"
        (NS k) <- indented e
        line "end"
        k

    dem (LOOP (Nest body)) = Nest $ do
        line "loop"
        (NS k) <- indented body
        line "end"
        k

docInstr' :: WASM -> Carrier 'Z
docInstr' wasm = eval gen pro wasm where
    gen :: () -> Carrier 'Z
    gen _ = Nest (return (NZ ()))

    pro :: Carrier n -> Carrier ('S n)
    pro (Nest prog) = Nest (return (NS prog))

docInstr :: WASM -> Doc ()
docInstr wasm = case docInstr' wasm of
    (Nest doc) -> fmap (const ()) doc

docLocal :: LocalName -> Doc ()
docLocal name = parens (do text "local "; docVar name; text " i32")

docParam :: LocalName -> Doc ()
docParam name = parens (do text "param "; docVar name; text " i32")

docRet :: Bool -> Doc ()
docRet True  = do parens (text "result i32"); sp
docRet False = empty

docGlobal :: Global -> Doc ()
docGlobal (Global name mut initial)
    = parens (do text "global "; docVar name; sp; mutDoc mut; sp; mutInitial initial) where
        mutDoc Mut   = parens (text "mut i32")
        mutInitial i = parens (do text "i32.const "; showable i)

docMem :: Memory -> Doc ()
docMem (Memory name n) = parens (do text "memory "; docVar name; sp; showable n)

docFunc :: Func -> Doc ()
docFunc func =
    parens (do
        -- Header
        text "func "; docVar (name func); sp
        docRet (doesRet func)
        nonEmpty (params func) $ \ps -> map docParam ps `sepBy` sp; sp
        nonEmpty (locals func) $ \ls -> map docLocal ls `sepBy` sp
        nl
        -- Body
        indented (docInstr (body func)))

docExport :: Export -> Doc ()
docExport (Export exportName e) = parens (do text "export "; quoted (text exportName); sp; export e) where
    export (ExportFunc name) = parens (do text "func "; docVar name)
    export (ExportMem  name) = parens (do text "memory "; docVar name)

docModule :: Module -> Doc ()
docModule modu =
    parens (do
        line "module"
        indented (do
            map docGlobal (globals modu)  `sepByEnd` nl
            map docMem    (memories modu) `sepByEnd` nl
            map docFunc   (funcs modu)    `sepByEnd` nl
            map docExport (exports modu)  `sepByEnd` nl))
