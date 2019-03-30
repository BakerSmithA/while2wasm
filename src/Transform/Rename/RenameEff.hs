
-- Composite scoped effect handler to perform renaming, implemented using
-- extensible effects methods. Allows other effects to be added, e.g. exceptions
-- to ensure procedures have been defined before being used.

{-# LANGUAGE ViewPatterns, PatternSynonyms, TypeOperators, DeriveFunctor #-}
{-# LANGUAGE DataKinds, KindSignatures, GADTs #-}
{-# LANGUAGE FlexibleContexts #-}

module Transform.Rename.RenameEff where

import Data.Word (Word)
import Data.Map (Map)
import qualified Data.Map as Map
import Front.AST
import Helper.Prog
import Helper.Co
import Helper.Eff

type Fresh = Word

data Rename k
    -- Takes original name and returns fresh name.
    = Name' Ident (Fresh -> k)
    deriving Functor

data Local k
    -- Assigns fresh names to all supplied names, and uses these new names
    -- inside continuation. After local, original names are restored.
    = Local' [Ident] k
    deriving Functor

-- Smart constructors

pattern Name v fk <- (prj -> Just (Name' v fk))
name :: (Functor f, Functor g) => Rename :<: f => Ident -> Prog f g Fresh
name v = inject (Name' v Var)

pattern Local vs k <- (prj -> Just (Local' vs k))
local :: (Functor f, Functor g) => Local :<: g => [Ident] -> Prog f g a -> Prog f g a
local vs inner = injectS (fmap (fmap return) (Local' vs inner))

-- Keeps track of mappings from original to fresh names, as well as the next
-- avaiable fresh name.
data Names = Names {
    seen    :: Map Ident Fresh
  , next    :: Fresh
}

emptyNames :: Names
emptyNames = Names Map.empty 0

-- Creates a fresh name and assigns a mapping from ident to the fresh name in
-- the current scope.
addFresh :: Ident -> Names -> (Fresh, Names)
addFresh ident names = (fresh, names') where
    names' = names { next=fresh+1, seen=seen' }
    seen'  = Map.insert ident fresh (seen names)
    fresh  = next names

-- Returns the mapping from ident to its fresh name, if one exists. Otherwise,
-- creates a new mapping.
getFresh :: Ident -> Names -> (Fresh, Names)
getFresh ident names =
    case ident `Map.lookup` (seen names) of
        Just fresh -> (fresh, names)
        Nothing    -> addFresh ident names

-- Descend into scope, and create new mappings for ids at this inner scope.
addManyFresh :: [Ident] -> Names -> Names
addManyFresh ids names = foldr f names ids where
    f ident ns = snd (addFresh ident ns)

-- Restore names to use old mappings, or new mappings if a mapping was not
-- present in the old mapping (e.g. a variable was defined at inner scope).
restoreNames :: Names -> Names -> Names
restoreNames old new = Names seen' next' where
    seen' = seen old `Map.union` seen new
    next' = next new -- Don't want to restore next fresh name

-- Same as State from EffectHandler.hs
-- I.e. compositional effect handler, allowing renaming syntax to be mixed
-- with AST syntax.
data CarrierRN f g a n
    = RN { runRN :: Names -> (Prog f g (CarrierRN' f g a n, Names)) }

data CarrierRN' f g a :: Nat -> * where
    CZRN :: a -> CarrierRN' f g a 'Z
    CSRN :: (Names -> (Prog f g (CarrierRN' f g a n, Names))) -> CarrierRN' f g a ('S n)

genRN :: (Functor f, Functor g) => a -> CarrierRN f g a 'Z
genRN x = RN $ \ns -> (return (CZRN x, ns))

algRN :: (Functor f, Functor g) => Alg (Rename :+: f) (Local :+: g) (CarrierRN f g a)
algRN = A a d p where
    a :: (Functor f, Functor g) => (Rename :+: f) (CarrierRN f g a n) -> CarrierRN f g a n
    a (Name v fk) = RN $ \ns ->
        let (fresh, ns') = getFresh v ns
        in runRN (fk fresh) ns'
    a (Other op) = RN $ \ns -> Op (fmap (\(RN runRN) -> runRN ns) op) where

    d :: (Functor f, Functor g) => (Local :+: g) (CarrierRN f g a ('S n)) -> CarrierRN f g a n
    d (Local vs k) = RN $ \ns -> do
        (CSRN runRN', ns') <- runRN k (addManyFresh vs ns)
        runRN' (restoreNames ns ns')

    d (Other op) = RN $ \ns -> Scope (fmap (f ns) op) where
        f :: (Functor f, Functor g) => Names -> CarrierRN f g a ('S n) -> Prog f g (Prog f g (CarrierRN' f g a n, Names))
        f ns l = fmap g (runRN l ns) where
            g :: (CarrierRN' f g a ('S n), Names) -> Prog f g (CarrierRN' f g a n, Names)
            g (CSRN runRN', ns') = runRN' ns'

    p :: (Functor f, Functor g) => CarrierRN f g a n -> CarrierRN f g a ('S n)
    p (RN runRN) = RN $ \ns -> return (CSRN runRN, ns)

handleRename :: (Functor f, Functor g) => Prog (Rename :+: f) (Local :+: g) a -> Prog f g a
handleRename prog = do
    (CZRN prog', _) <- runRN (run genRN algRN prog) emptyNames
    return prog'
