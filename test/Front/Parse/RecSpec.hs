{-# LANGUAGE TypeOperators #-}

module Front.Parse.RecSpec where

import Test.Hspec
import Front.AST
import Front.Pretty()
import Front.Eq()
import qualified Front.Parse.Rec as R
import Helper.Prog
import Helper.Co

type Op    v p = VarExp v :+: AExp :+: BExp :+: VarStm v :+: ProcStm p :+: Stm
type Scope v p = ScopeStm :+: BlockStm v p
type While v p = Prog (Op v p) (Scope v p) ()
type IWhile    = While Ident Ident

recSpec :: Spec
recSpec = do
    describe "Recursive AST" $ do
        context "prog" $ do
            it "converts to Prog AST" $ do
                let r = R.Comp (R.If R.T R.Skip R.Skip) R.Skip
                    a = do ifElse true skip skip; skip :: IWhile
                prog r `shouldBe` a
