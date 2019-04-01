{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module Transform.RenameSpec where

import Test.Hspec
import Front.AST
import Front.Pretty()
import Front.Eq()
import Helper.Co
import Transform.Rename

type Op    v p = VarExp v :+: AExp :+: BExp :+: VarStm v :+: ProcStm p :+: Stm
type Scope v p = ScopeStm :+: BlockStm v p
type While v p = Prog (Op v p) (Scope v p) ()
type IWhile    = While Ident Ident
type FWhile    = While Fresh Fresh

getIVar :: VarExp Ident :<: f => Ident -> Prog f g a
getIVar = getVar

getFVar :: VarExp Fresh :<: f => Fresh -> Prog f g a
getFVar = getVar

renameSpec :: Spec
renameSpec = do
    describe "renaming" $ do
        context "variables" $ do
            it "renames getting variables" $ do
                let a = getIVar "x" :: IWhile
                    e = getFVar (Fresh "v" 0) :: FWhile
                rename a `shouldBe` e
