{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module Transform.RenameSpec where

import Test.Hspec
import Front.AST
import Front.Pretty()
import Front.Eq()
import Helper.Prog hiding (p)
import Helper.Co
import Transform.Rename

type Op    v p = VarExp v :+: AExp :+: BExp :+: VarStm v :+: ProcStm p :+: Stm
type Scope v p = ScopeStm :+: BlockStm v p
type While v p = Prog (Op v p) (Scope v p) ()
type IWhile    = While Ident Ident
type FWhile    = While FreshName FreshName

getIVar :: VarExp Ident :<: f => Ident -> Prog f g a
getIVar = getVar

getFVar :: VarExp FreshName :<: f => FreshName -> Prog f g a
getFVar = getVar

setIVar :: VarStm Ident :<: f => Ident -> Prog f g () -> Prog f g ()
setIVar = setVar

setFVar :: VarStm FreshName :<: f => FreshName -> Prog f g () -> Prog f g ()
setFVar = setVar

callI :: ProcStm Ident :<: f => Ident -> Prog f g ()
callI = call

callF :: ProcStm FreshName :<: f => FreshName -> Prog f g ()
callF = call

blockI :: (Functor f, BlockStm Ident Ident :<: g)
       => [(Ident, Prog f g ())] -> [(Ident, Prog f g ())]
       -> Prog f g () -> Prog f g ()
blockI = block

blockF :: (Functor f, BlockStm FreshName FreshName :<: g)
       => [(FreshName, Prog f g ())] -> [(FreshName, Prog f g ())]
       -> Prog f g () -> Prog f g ()
blockF = block

v :: Word -> FreshName
v = FreshName "v"

p :: Word -> FreshName
p = FreshName "p"

renameSpec :: Spec
renameSpec = do
    describe "renaming" $ do
        it "renames getting variables" $ do
            let a = getIVar "x" :: IWhile
                e = getFVar (v 0) :: FWhile
            rename a `shouldBe` Right e

        it "renames variables in arithmetic expressions" $ do
            let a = add (getIVar "x") (getIVar "y") :: IWhile
                e = add (getFVar (v 0)) (getFVar (v 1)) :: FWhile
            rename a `shouldBe` Right e

        it "renames variables in boolean expressions" $ do
            let a = equ (getIVar "x") (getIVar "y") :: IWhile
                e = equ (getFVar (v 0)) (getFVar (v 1)) :: FWhile
            rename a `shouldBe` Right e

        it "renames setting variables" $ do
            let a = setIVar "x"   (num 1) :: IWhile
                e = setFVar (v 0) (num 1) :: FWhile
            rename a `shouldBe` Right e

        it "renames procedures" $ do
            let a = do blockI [] [("p", skip)] (callI "p")   :: IWhile
                e = do blockF [] [(p 0, skip)] (callF (p 0)) :: FWhile
            rename a `shouldBe` Right e

        it "fails if calling procedure which has been been declared yet" $ do
            let a = callI "p" :: IWhile
                r = rename a  :: Either String FWhile
            r `shouldBe` Left "No procedure declared"

        it "uses existing fresh name" $ do
            let a = do export (getIVar "x");   export (getIVar "x") :: IWhile
                e = do export (getFVar (v 0)); export (getFVar (v 0)) :: FWhile
            rename a `shouldBe` Right e

        it "renames variables in exports" $ do
            let a = export (getIVar "x") :: IWhile
                e = export (getFVar (v 0)) :: FWhile
            rename a `shouldBe` Right e

        it "renames variables in if statements" $ do
            let a = ifElse (equ (getIVar "x") (num 1))
                        (setIVar "x" (num 1))
                        (setIVar "y" (num 2)) :: IWhile
                e = ifElse (equ (getFVar (v 0)) (num 1))
                        (setFVar (v 0) (num 1))
                        (setFVar (v 1) (num 2)) :: FWhile
            rename a `shouldBe` Right e

        it "renames variables in while statements" $ do
            let a = while (equ (getIVar "x") (num 1))
                        (setIVar "y" (num 1)) :: IWhile
                e = while (equ (getFVar (v 0)) (num 1))
                        (setFVar (v 1) (num 1)) :: FWhile
            rename a `shouldBe` Right e

        it "renames variables in block body" $ do
            let a = do setIVar "x" (num 1)
                       blockI [] []
                           (setIVar "x" (num 1)) :: IWhile

                e = do setFVar (v 0) (num 1)
                       blockF [] []
                           (setFVar (v 0) (num 1)) :: FWhile

            rename a `shouldBe` Right e

        it "gives new names to local variables in block" $ do
            let a = do setIVar "x" (num 1)
                       blockI [("x", num 1)] []
                           (setIVar "x"   (num 1)) :: IWhile

                e = do setFVar (v 0) (num 1)
                       blockF [(v 1, num 1)] []
                           (setFVar (v 1) (num 1)) :: FWhile

            rename a `shouldBe` Right e

        it "uses local names inside procedures" $ do
            let a = do setIVar "x" (num 1)
                       blockI
                           [("x", num 1)]
                           [("p", setIVar "x" (num 1))]
                           (setIVar "x"   (num 1)) :: IWhile

                e = do setFVar (v 0) (num 1)
                       blockF
                           [(v 1, num 1)]
                           [(p 0, setFVar (v 1) (num 1))]
                           (setFVar (v 1) (num 1)) :: FWhile

            rename a `shouldBe` Right e
