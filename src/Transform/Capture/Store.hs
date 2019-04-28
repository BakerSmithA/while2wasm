
{-# LANGUAGE TypeOperators, DataKinds, KindSignatures, GADTs #-}

module Transform.Capture.Store where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Front.AST
import Transform.Capture.Annotate
import Helper.Free.Free
import Helper.Free.Alg
import Helper.Co
import Helper.Inj
import Helper.Scope.Prog
import Helper.Eff.Reader
import Helper.Eff.Writer

type LocalVars        v   = Set v
type ForeignVars      v   = Set v
type ProcVarLocations v p = Map p (LocalVars v, ForeignVars v)

type IsLocal v = v -> Bool

type Ctx v p = Ask (IsLocal v) :+: Tell (ProcVarLocations v p)

data Carrier f g v p a n = C { runC :: Ctx v p (Prog f g (Carrier' f g v p a n)) }

data Carrier' f g v p a :: Nat -> * where
    CZ :: a -> Carrier' f g v p a 'Z
    CS :: Ctx v p (Prog f g (Carrier' f g v p a n)) -> Carrier' f g v p a ('S n)
