
-- Pretty printing where document structure is described using Prog.

{-# LANGUAGE MultiParamTypeClasses, DeriveFunctor, TypeOperators #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}

module Helper.Pretty
( Doc
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
import Helper.Scope.Prog
import Helper.Scope.Alg
import Helper.Co

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data Text k
    = Text String k
    | Newline k
    deriving Functor

data Indent k
    = Indent k
    deriving Functor

type Doc = Prog Text Indent

class Pretty a where
    pretty :: a -> Doc ()

empty :: (Functor f, Functor g) => Prog f g ()
empty = return ()

text :: Text :<: f => String -> Prog f g ()
text s = injectP (Text s (Var ()))

showable :: (Show a, Text :<: f) => a -> Prog f g ()
showable x = text (show x)

nl :: Text :<: f => Prog f g ()
nl = injectP (Newline (Var ()))

-- Single space.
sp :: Text :<: f => Prog f g ()
sp = text " "

-- Document containing text s followed by a newline.
line :: (Text :<: f, Functor g) => String -> Prog f g ()
line s = text s >> nl

line' :: (Text :<: f, Functor g) => Prog f g () -> Prog f g ()
line' doc = doc >> nl

-- Wraps document inside entry and exit text.
between :: (Text :<: f, Functor g) => String -> String -> Prog f g () -> Prog f g ()
between start end inner = text start >> inner >> text end

-- Wraps inner document in parenthesis.
parens :: (Text :<: f, Functor g) => Prog f g () -> Prog f g ()
parens = between "(" ")"

-- Wrapper inner document in quotation marks.
quoted :: (Text :<: f, Functor g) => Prog f g () -> Prog f g ()
quoted = between "\"" "\""

nonEmpty :: (Functor f, Functor g) => [a] -> ([a] -> Prog f g ()) -> Prog f g ()
nonEmpty [] _ = empty
nonEmpty xs f = f xs

-- Places separator s between each Doc in ds.
sepBy :: (Functor f, Functor g) => [Prog f g ()] -> Prog f g () -> Prog f g ()
sepBy []     _ = empty
sepBy [x]    _ = x
sepBy (x:xs) s = x >> s >> sepBy xs s

-- Places separator s between each Doc in ds, and separator at end.
sepByEnd :: (Functor f, Functor g) => [Prog f g ()] -> Prog f g () -> Prog f g ()
sepByEnd [] _ = return ()
sepByEnd ds s = sepBy ds s >> s

indented :: (Indent :<: g, Functor f) => Prog f g a -> Prog f g a
indented inner = injectPSc (fmap (fmap return) (Indent inner))

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

type IndentLvl    = Word
type ShouldIndent = Bool

indentBy :: IndentLvl -> ShouldIndent -> String
indentBy _ False = ""
indentBy i True  = replicate (fromIntegral i*2) ' '

data Carrier a n = C { runC :: IndentLvl -> ShouldIndent -> String -> (String, Carrier' a n) }

data Carrier' a :: Nat -> * where
    CZ :: a -> Carrier' a 'Z
    CS :: (IndentLvl -> ShouldIndent -> String -> (String, Carrier' a n)) -> Carrier' a ('S n)

instance OpAlg Text (Carrier a) where
    alg (Text str (C runK)) = C $ \lvl shouldIndent acc -> runK lvl False (acc ++ indentBy lvl shouldIndent ++ str)
    alg (Newline  (C runK)) = C $ \lvl _            acc -> runK lvl True  (acc ++ "\n")

instance ScopeAlg Indent (Carrier a) where
    dem (Indent (C runK)) = C $ \lvl shouldIndent acc ->
        -- Run inner continuation indented one level.
        case runK (lvl+1) shouldIndent acc of
            -- Run remaining continuation at original level.
            (acc', CS runK') -> runK' lvl shouldIndent acc'

pro :: Carrier a n -> Carrier a ('S n)
pro (C runC) = C $ \_ _ acc -> (acc, CS runC)

gen :: a -> Carrier a 'Z
gen x = C $ \_ _ acc -> (acc, CZ x)

toString :: (OpAlg f (Carrier a), ScopeAlg g (Carrier a)) => IndentLvl -> Prog f g a -> String
toString lvl prog = fst (runC (eval gen pro prog) lvl True "")
