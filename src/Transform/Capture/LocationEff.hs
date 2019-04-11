
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
, DiscardLocals
, seen
, getLocations
, addLocal
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
    -- Adds a local variable to the environment. Will show up in the set of
    -- local variables for this scope.
    | AddLocal' FreshName k
    -- Retrieves the mapping from variable names to locations in the curret scope.
    | GetLocations' (Locations -> k)
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

pattern AddLocal v k <- (prj -> Just (AddLocal' v k))
addLocal :: (Functor f, Functor g, LocOp :<: f) => FreshName -> Prog f g ()
addLocal v = injectP (AddLocal' v (Var ()))

pattern GetLocations fk <- (prj -> Just (GetLocations' fk))
getLocations :: (Functor f, Functor g, LocOp :<: f) => Prog f g Locations
getLocations = injectP (GetLocations' Var)

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

addLocalVar :: FreshName -> IsLocal -> IsLocal
addLocalVar fresh isLocal v = fresh == v || isLocal v

--------------------------------------------------------------------------------
-- Semantics
--------------------------------------------------------------------------------

type Op  f   = State   IsLocal :+: State   Locations :+: f
type Sc  g   = LocalSt IsLocal :+: LocalSt Locations :+: g
type Ctx f g = Prog (Op f) (Sc g)

type Carrier f g = Nest1 (Ctx f g)

getIsLocal :: (Functor f, Functor g) => Ctx f g IsLocal
getIsLocal = get

putIsLocal :: (Functor f, Functor g) => IsLocal -> Ctx f g ()
putIsLocal = put

getLocs :: (Functor f, Functor g) => Ctx f g Locations
getLocs = get

putLocs :: (Functor f, Functor g) => Locations -> Ctx f g ()
putLocs = put

gen :: (Functor f, Functor g) => a -> Carrier f g a 'Z
gen x = Nest1 (return (NZ1 x))

alg :: (Functor f, Functor g) => Alg (LocOp :+: f) (DiscardLocals :+: g) (Carrier f g a)
alg = A a d p where
    a :: (Functor f, Functor g) => (LocOp :+: f) (Carrier f g a n) -> Carrier f g a n
    -- Insert a seen variable into either the local or foreign set depending on
    -- whether it is local to the current scope.
    a (Seen v k) = Nest1 $ do
        isLocal <- getIsLocal
        (locals, fors) <- getLocs
        -- Cannot only insert into foreign set, because IsLocal returns True
        -- for all variables at top level. Therefore, this would leave the top
        -- level locals-set empty.
        if isLocal v
            then putLocs (Set.insert v locals, fors)
            else putLocs (locals, Set.insert v fors)
        runNest1 k

    a (AddLocal v k) = Nest1 $ do
        isLocal <- getIsLocal
        (locals, fors) <- getLocs

        let isLocal' = addLocalVar v isLocal
            locals'  = Set.insert v locals

        putIsLocal isLocal'
        putLocs (locals', fors)

        runNest1 k

    -- Supply the current local and foreign variables to the continuation.
    a (GetLocations fk) = Nest1 $ do
        locs <- getLocs
        runNest1 (fk locs)

    -- Manually inject op into `f` in `Op f`. Cannot use `injectP` because the
    -- types cannot be deduced.
    a (Other op) = Nest1 (Op (fmap runNest1 (R $ R op)))

    d :: (Functor f, Functor g) => (DiscardLocals :+: g) (Carrier f g a ('S n)) -> Carrier f g a n
    -- Inside scope there are no local or foreign variables.
    d (DiscardLocals k) = Nest1 $ do
        (NS1 runK', innerForeigns) <- localSt noneLocal (do
            localSt emptyLocations (do
                k' <- runNest1 k
                (_, fors) <- getLocs
                return (k', fors)))

        -- Add inner foreign variables as foreign variables of outer scope,
        -- unless outer scope already contains variables as local.
        --
        -- This is to allow foreign variables to be passed through different
        -- scopes. This is required because outputted WASM functions may need to
        -- pass variables through them. E.g. in example below variable x needs
        -- to be passed from main WASM function, through f, and into g.
        --
        -- begin
        --   proc f is (
        --     begin
        --       prog g is x := 1;
        --       skip
        --     end
        --   );
        --   skip
        -- end

        (locals, fors) <- getLocs
        let fors' = fors `Set.union` (Set.difference innerForeigns locals)
        putLocs (locals, fors')

        runK'

    d (Other op) = Nest1 (Scope (fmap (\(Nest1 prog) -> fmap f prog) (R $ R op))) where
        f :: (Functor f, Functor g) => Nest1' (Ctx f g) a ('S n) -> Ctx f g (Nest1' (Ctx f g) a n)
        f (NS1 prog) = prog

    p :: (Functor f, Functor g) => Carrier f g a n -> Carrier f g a ('S n)
    p (Nest1 prog) = Nest1 (return (NS1 prog))

makeLoc :: (Functor f, Functor g) => Prog (LocOp :+: f) (DiscardLocals :+: g) a -> Ctx f g a
makeLoc prog = case run gen alg prog of
    (Nest1 prog') -> fmap (\(NZ1 x) -> x) prog'

handleLocs :: (Functor f, Functor g) => Prog (LocOp :+: f) (DiscardLocals :+: g) a -> Prog f g (a, Locations)
handleLocs prog = do
    ((prog', isLocal), locations) <- (handleState emptyLocations . handleState allLocal . makeLoc) prog
    return (prog', locations)
