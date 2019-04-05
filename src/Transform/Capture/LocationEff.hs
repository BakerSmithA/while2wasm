
-- Produces mapping from variable name to whether it is local or foreign to a
-- scope. This is used to decide whether a WebAssembly function owns a variable.
--
-- WARNING: Assumes all variable names are unique.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms #-}

module Transform.Capture.LocationEff where

import Data.Map (Map)
import qualified Data.Map as Map
import Helper.Prog
import Helper.Co

data Location
    -- Variable local to a level of scope, analagous to a variable decalared
    -- inside a function.
    = Local
    -- Variable used outside the scope it was declared, analagous to an argument
    -- passed into a function.
    | Foreign
    deriving (Eq, Show)

data LocOp v k
    -- Tells the environment that variable v was seen at the current scope.
    = Seen' v k
    -- Retrieves the mapping from variable names to locations in the curret scope.
    | GetLocations' (Map v Location -> k)
    deriving Functor

data Add v k
    -- Tells environment that variables inside continuation are local.
    -- This does not change whether other variables are local or foreign.
    = Add' [v] k
    deriving Functor

-- Separated from Add type because no v parameter, so to avoid ambiguous types.
data Discard k
    -- Tells environment that any variables inside continuation should be
    -- treated as foreign, unless overwritten by nested Locals.
    = Discard' k
    deriving Functor

pattern Seen v k <- (prj -> Just (Seen' v k))
seen :: (Functor f, Functor g, LocOp v :<: f) => v -> Prog f g ()
seen v = inject (Seen' v (Var ()))

pattern GetLocations fk <- (prj -> Just (GetLocations' fk))
getLocations :: (Functor f, Functor g, LocOp v :<: f) => Prog f g (Map v Location)
getLocations = inject (GetLocations' Var)

pattern Add vs k <- (prj -> Just (Add' vs k))
addLocals :: (Functor f, Functor g, Add v :<: g) => [v] -> Prog f g a -> Prog f g a
addLocals vs inner = injectS (fmap (fmap return) (Add' vs inner))

pattern Discard k <- (prj -> Just (Discard' k))
-- Returns the result of the scoped continuation, as well as the mapping
-- from variables to their location inside the scope.
discardLocals :: (Functor f, Functor g, Discard :<: g, LocOp v :<: f) => Prog f g a -> Prog f g (a, Map v Location)
discardLocals inner = injectS (fmap (fmap return) (Discard' f)) where
    f = do
        x  <- inner
        ls <- getLocations
        return (x, ls)
