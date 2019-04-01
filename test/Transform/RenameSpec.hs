{-# LANGUAGE TypeOperators #-}

module Transform.RenameSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Front.AST
import Front.Pretty
import Front.Eq
import Helper.Co
import Transform.Rename

type Op    v p = VarExp v :+: AExp :+: BExp :+: VarStm v :+: ProcStm p :+: Stm
type Scope v p = ScopeStm :+: BlockStm v p
type While v p = Prog (Op v p) (Scope v p) ()
type IWhile    = While Ident Ident
type FWhile    = While Fresh Fresh

renameSpec :: Spec
renameSpec = do
    describe "renaming" $ do
        context "variables" $ do
            it "renames getting variables" $ do
                let a = getVar "x" :: IWhile
                    e = getVar "x" :: IWhile
                a `shouldBe` e
