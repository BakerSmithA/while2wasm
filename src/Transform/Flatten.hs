
-- Removes blocks containing local variables, transforming local variable
-- declarations into variable assgniments. This assumes all variables have
-- unique names.
--
-- Also produces a mapping from procedure names meta data about the procedure,
-- including the foreign variables used in the function, or any nested functions.
-- This is used to decide what arguments functions have in outputted WebAssembly.
-- The meta data also includes a mapping from each variable to whether they
-- are local or foreign. Used to retrieve the 'type' of a variable from its name.

{-# LANGUAGE DeriveFunctor #-}

module Transform.Flatten where

import Data.Set (Set)
import qualified Data.Set as Set

-- When AST is flatten, Blocks are removed turning variable declarations into
-- assignment, and procedure declarations into closures.
data Closure v k
    = Closure v k
    deriving Functor

-- Meta-data associated with each closure. Used when calling functions, or
-- generating function definition.
data ClosureMeta v = ClosureMeta {
     local   :: Set v
   , foreign :: Set v
}
