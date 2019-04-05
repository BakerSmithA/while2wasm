
-- Pretty printing where document structure is described using Prog.

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Helper.Pretty
( Doc
, DocF(..)
, Pretty(..)
, empty
, text
, showable
, line'
, nl
, sp
, nonEmpty
, line
, parens
, quoted
, sepBy
, sepByEnd
, indented
, toString
) where

import Data.Word (Word)
import Helper.Prog
import Helper.Co

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

-- TODO: Rename DocF
-- Describes how to layout a document.
data DocF k
    = Text String k
    | Newline k
    deriving Functor

data Indent k
    = Indent k
    deriving Functor

-- Like the Writer monad.
type Doc = Prog DocF Indent

class Pretty a where
    pretty :: a -> Doc ()

empty :: Doc ()
empty = return ()

text :: String -> Doc ()
text s = Op (Text s (Var ()))

showable :: Show a => a -> Doc ()
showable x = text (show x)

line' :: Doc a -> Doc a
line' doc = do x <- doc; nl; return x

nl :: Doc ()
nl = Op (Newline (Var ()))

-- Single space.
sp :: Doc ()
sp = text " "

nonEmpty :: [a] -> ([a] -> Doc ()) -> Doc ()
nonEmpty [] _ = empty
nonEmpty xs f = f xs

-- Document containing text s followed by a newline.
line :: String -> Doc ()
line s = do text s; nl

-- Wraps inner document in parenthesis.
parens :: Doc a -> Doc a
parens inner = do text "("; x <- inner; text ")"; return x

-- Wrapper inner document in quotation marks.
quoted :: Doc () -> Doc ()
quoted inner = do text "\""; inner; text "\""

-- Places separator s between each Doc in ds.
sepBy :: [Doc ()] -> Doc () -> Doc ()
sepBy []     _ = empty
sepBy [x]    _ = x
sepBy (x:xs) s = do x; s; sepBy xs s

-- Places separator s between each Doc in ds, and separator at end.
sepByEnd :: [Doc ()] -> Doc () -> Doc ()
sepByEnd [] _ = return ()
sepByEnd ds s = do sepBy ds s; s

-- Indents inner by one spacing unit on top of spacing already applied.
indented :: Doc a -> Doc a
indented inner = Scope (fmap (fmap return) (Indent inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

-- How many spacing units to indented by.
type IndentLvl = Word

-- Whether the current line has been indented, used to avoid indenting the
-- current line multiple times.
type DidIndent = Bool

indentBy :: IndentLvl -> DidIndent -> String
indentBy i False = concat (replicate (fromIntegral i) "  ")
indentBy _ True  = ""

data CarrierD a n
    = D { runD :: (IndentLvl -> DidIndent -> String -> (CarrierD' a n, IndentLvl, DidIndent, String)) }

data CarrierD' a :: Nat -> * where
    CZ :: a -> CarrierD' a 'Z
    CS :: (IndentLvl -> DidIndent -> String -> (CarrierD' a n, IndentLvl, DidIndent, String)) -> CarrierD' a ('S n)

genD :: a -> CarrierD a 'Z
genD a = D (\lvl didIndent s -> (CZ a, lvl, didIndent, s))

algD :: Alg DocF Indent (CarrierD a)
algD = A a d p where
    a :: DocF (CarrierD a n) -> CarrierD a n
    -- runD is like the continuation (?)
    a (Text s  (D runD)) = D $ \lvl didIndent acc -> runD lvl True  (acc ++ indentBy lvl didIndent ++ s)
    a (Newline (D runD)) = D $ \lvl _         acc -> runD lvl False (acc ++ "\n")

    -- Exit one level of scope (?)
    d :: Indent (CarrierD a ('S n)) -> CarrierD a n
    d (Indent (D runD)) = D $ \lvl didIndent acc ->
        -- Indent by 1 level
        case runD (lvl+1) didIndent acc of
            -- Dedent by 1 level
            (CS runD', lvl', didIndent', acc') -> runD' (lvl'-1) didIndent' acc'

    p :: CarrierD a n -> CarrierD a ('S n)
    p carrier = D $ \lvl didIndent acc -> (CS (runD carrier), lvl, didIndent, acc)

toString :: IndentLvl -> Doc () -> String
toString lvl doc = case (runD (run genD algD doc) lvl False "") of
    (_, _, _, str) -> str
