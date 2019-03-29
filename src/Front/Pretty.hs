
-- Pretty printing While AST using Datatypes a la Carte methods.

{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators, DataKinds #-}

module Front.Pretty where

import Front.AST
import Helper.Pretty
import Helper.Alg
import Helper.Co

type DocCarrier = CarrierM Doc ()

instance OpAlg IVarExp DocCarrier where
    alg = undefined

instance OpAlg AExp DocCarrier where
    alg = undefined

instance OpAlg BExp DocCarrier where
    alg = undefined

instance OpAlg IVarStm DocCarrier where
    alg = undefined

instance OpAlg IProcStm DocCarrier where
    alg = undefined

instance OpAlg Stm DocCarrier where
    alg = undefined

instance ScopeAlg ScopeStm DocCarrier where
    dem = undefined

instance ScopeAlg IBlockStm DocCarrier where
    dem = undefined

type X = IVarExp :+: AExp :+: BExp :+: IVarStm :+: IProcStm :+: Stm
type Y = ScopeStm :+: IBlockStm

docAST :: Prog X Y () -> Doc ()
docAST = evalM pro where
    pro :: CarrierM Doc a n -> CarrierM Doc a ('S n)
    pro (M x) = M (return (CSM x))
