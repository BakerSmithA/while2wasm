
-- Produces mapping from variable name to whether it is local or foreign to a
-- scope. This is used to decide whether a WebAssembly function owns a variable.
--
-- WARNING: Assumes all variable names are unique.

{-# LANGUAGE DeriveFunctor, TypeOperators, FlexibleContexts #-}
{-# LANGUAGE ViewPatterns, PatternSynonyms, DataKinds, KindSignatures, GADTs #-}

module Transform.Capture.LocationEff
( LocalVars
, ForeignVars
, Locations
, LocOp
, AddLocals
, DiscardLocals
, seen
, getLocations
, addLocals
, discardLocals
, handleLocs
) where

import Data.Set (Set)
import qualified Data.Set as Set
import Transform.Rename.Rename (FreshName)
import Helper.Scope.Prog
import Helper.Scope.Nest
import Helper.Co
import Helper.Eff.Reader
import Helper.Eff.State
import Helper.Eff

type LocalVars   = Set FreshName
type ForeignVars = Set FreshName
type Locations   = (LocalVars, ForeignVars)

emptyLocations :: Locations
emptyLocations = (Set.empty, Set.empty)

--------------------------------------------------------------------------------
-- Syntax
--------------------------------------------------------------------------------

data LocOp k
    -- Tells the environment that variable v was seen at the current scope.
    = Seen' FreshName k
    -- Retrieves the mapping from variable names to locations in the curret scope.
    | GetLocations' (Locations -> k)
    deriving Functor

data AddLocals k
    -- Tells environment that variables inside continuation are local.
    -- Also modifies mapping from variables to location to make variables local
    -- at current scope.
    = AddLocals' [FreshName] k
    deriving Functor

-- Separated from AddLocals type because no v parameter, so to avoid ambiguous types.
data DiscardLocals k
    -- Tells environment that any variables inside continuation should be
    -- treated as foreign, unless overwritten by nested Locals.
    = DiscardLocals' k
    deriving Functor

pattern Seen v k <- (prj -> Just (Seen' v k))
seen :: (Functor f, Functor g, LocOp :<: f) => FreshName -> Prog f g ()
seen v = injectP (Seen' v (Var ()))

pattern GetLocations fk <- (prj -> Just (GetLocations' fk))
getLocations :: (Functor f, Functor g, LocOp :<: f) => Prog f g Locations
getLocations = injectP (GetLocations' Var)

pattern AddLocals vs k <- (prj -> Just (AddLocals' vs k))
addLocals :: (Functor f, Functor g, AddLocals :<: g) => [FreshName] -> Prog f g a -> Prog f g a
addLocals vs inner = injectPSc (fmap (fmap return) (AddLocals' vs inner))

pattern DiscardLocals k <- (prj -> Just (DiscardLocals' k))
discardLocals :: (Functor f, Functor g, DiscardLocals :<: g) => Prog f g a -> Prog f g a
discardLocals inner = injectPSc (fmap (fmap return) (DiscardLocals' inner))

--------------------------------------------------------------------------------
-- Aux Semantics
--------------------------------------------------------------------------------

-- Whether a variable was declared at the current scope.
type IsLocal = FreshName -> Bool

-- Simple shallow DSL to describe which variables are local.
allLocal :: IsLocal
allLocal = const True

noneLocal :: IsLocal
noneLocal = const False

isLocalFromList :: [FreshName] -> IsLocal
isLocalFromList vs v = v `Set.member` Set.fromList vs

orLocal :: IsLocal -> IsLocal -> IsLocal
orLocal x y v = x v || y v

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

type Op  f   = Ask    IsLocal :+: State   Locations :+: f
type Sc  g   = LocalR IsLocal :+: LocalSt Locations :+: g
type Ctx f g = Prog (Op f) (Sc g)

type Carrier f g = Nest (Ctx f g)

askIsLocal :: (Functor f, Functor g) => Ctx f g IsLocal
askIsLocal = ask

getLocs :: (Functor f, Functor g) => Ctx f g Locations
getLocs = get

putLocs :: (Functor f, Functor g) => Locations -> Ctx f g ()
putLocs = put

gen :: (Functor f, Functor g) => a -> Carrier f g a 'Z
gen x = Nest (return (NZ x))

alg :: (Functor f, Functor g) => Alg (LocOp :+: f) (AddLocals :+: DiscardLocals :+: g) (Carrier f g a)
alg = A a d p where
    a :: (Functor f, Functor g) => (LocOp :+: f) (Carrier f g a n) -> Carrier f g a n
    -- Insert a seen variable into either the local or foreign set.
    a (Seen v k) = Nest $ do
        isLocal <- askIsLocal
        (locals, fors) <- getLocs
        if isLocal v
            then putLocs (Set.insert v locals, fors)
            else putLocs (locals, Set.insert v fors)
        runNest k

    -- Supply the current local and foreign variables to the continuation.
    a (GetLocations fk) = Nest $ do
        locs <- getLocs
        runNest (fk locs)

    -- Inject op into `f` in `Op f`. Cannot use `injectP` because the types
    -- cannot be deduced.
    a (Other op) = Nest (Op (fmap runNest (R $ R op)))

    d :: (Functor f, Functor g) => (AddLocals :+: DiscardLocals :+: g) (Carrier f g a ('S n)) -> Carrier f g a n
    -- Inside scope, local variables are added to set of local variables.
    -- Querying whether one of the variables is local (i.e. using IsLocal) will
    -- also return true.
    d (AddLocals vs k) = Nest $ do
        isLocal <- askIsLocal
        let isLocal' = isLocal `orLocal` isLocalFromList vs

        (locals, fors) <- getLocs
        let locals'   = Set.union (Set.fromList vs) locals
            locations = (locals', fors)

        (NS runK', innerForeigns) <- localR isLocal' (do
            localSt locations (do
                k' <- runNest k
                (_, fors) <- getLocs
                return (k', fors)))

        -- Any variables foreign to the inner scope, are also foreign to this
        -- scope, unless local to this scope. This is because, variables need
        -- to be 'passed through' scopes to be made avaiable to lower scopes.
        let fors' = fors `Set.union` (Set.difference innerForeigns locals)
        putLocs (locals, fors')

        runK'

    d (DiscardLocals k) = Nest $ do
        NS runK' <- localR noneLocal (do
            localSt emptyLocations (
                runNest k))
        runK'

    d (Other (Other op)) = Nest (Scope (fmap (\(Nest prog) -> undefined) (R $ R op))) where
        f :: (Functor f, Functor g) => Nest' (Ctx f g) a ('S n) -> Ctx f g (Nest' (Ctx f g) a n)
        f (NS prog) = prog

    p :: (Functor f, Functor g) => Carrier f g a n -> Carrier f g a ('S n)
    p (Nest prog) = Nest (return (NS prog))

makeLoc :: (Functor f, Functor g) => Prog (LocOp :+: f) (AddLocals :+: DiscardLocals :+: g) a -> Ctx f g a
makeLoc prog = case run gen alg prog of
    (Nest prog') -> fmap (\(NZ x) -> x) prog'

handleLocs :: (Functor f, Functor g) => Prog (LocOp :+: f) (AddLocals :+: DiscardLocals :+: g) a -> Prog f g a
handleLocs = fmap fst . handleState emptyLocations . handleReader allLocal . makeLoc
