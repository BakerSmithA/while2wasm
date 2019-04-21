{-# LANGUAGE TypeOperators, FlexibleContexts #-}

module Transform.Rename.RenameSpec where

import Test.Hspec
import Front.AST
import Helper.Free.Free
import Helper.Co
import Transform.Rename.Rename

type IWhile    = While Ident Ident
type FWhile    = While FreshName FreshName

getIVar :: VarExp Ident :<: f => Ident -> Free f a
getIVar = getVar

getFVar :: VarExp FreshName :<: f => FreshName -> Free f a
getFVar = getVar

setIVar :: VarStm Ident :<: f => Ident -> Free f () -> Free f ()
setIVar = setVar

setFVar :: VarStm FreshName :<: f => FreshName -> Free f () -> Free f ()
setFVar = setVar

callI :: ProcStm Ident :<: f => Ident -> Free f ()
callI = call

callF :: ProcStm FreshName :<: f => FreshName -> Free f ()
callF = call

blockI :: (BlockStm Ident Ident :<: f)
       => [(Ident, Free f ())] -> [(Ident, Free f ())]
       -> Free f () -> Free f ()
blockI = block

blockF :: (BlockStm FreshName FreshName :<: f)
       => [(FreshName, Free f ())] -> [(FreshName, Free f ())]
       -> Free f () -> Free f ()
blockF = block

v :: Word -> FreshName
v = id

p :: Word -> FreshName
p = id

renameAST' = fmap fst . renameAST

renameSpec :: Spec
renameSpec = do
    describe "renaming" $ do
        it "renames getting variables" $ do
            let a = getIVar "x"   :: IWhile
                e = getFVar (v 0) :: FWhile
            renameAST' a `shouldBe` Right e

        it "renames variables in arithmetic expressions" $ do
            let a = add (getIVar "x") (getIVar "y") :: IWhile
                e = add (getFVar (v 0)) (getFVar (v 1)) :: FWhile
            renameAST' a `shouldBe` Right e

        it "renames variables in boolean expressions" $ do
            let a = equ (getIVar "x") (getIVar "y") :: IWhile
                e = equ (getFVar (v 0)) (getFVar (v 1)) :: FWhile
            renameAST' a `shouldBe` Right e

        it "renames setting variables" $ do
            let a = setIVar "x"   (num 1) :: IWhile
                e = setFVar (v 0) (num 1) :: FWhile
            renameAST' a `shouldBe` Right e

        it "renames procedures" $ do
            let a = blockI [] [("p", skip)] (callI "p")   :: IWhile
                e = blockF [] [(p 0, skip)] (callF (p 0)) :: FWhile
            renameAST' a `shouldBe` Right e

        it "fails if calling procedure which has been been declared yet" $ do
            let a = callI "p" :: IWhile
                r = renameAST' a  :: Either RenameErr FWhile
            r `shouldBe` Left (UndefinedProc "p")

        it "uses existing fresh name" $ do
            let a = export (getIVar "x")   `comp` export (getIVar "x")   :: IWhile
                e = export (getFVar (v 0)) `comp` export (getFVar (v 0)) :: FWhile
            renameAST' a `shouldBe` Right e

        it "renames variables in exports" $ do
            let a = export (getIVar "x") :: IWhile
                e = export (getFVar (v 0)) :: FWhile
            renameAST' a `shouldBe` Right e

        it "renames variables in if statements" $ do
            let a = ifElse (equ (getIVar "x") (num 1))
                        (setIVar "x" (num 1))
                        (setIVar "y" (num 2)) :: IWhile
                e = ifElse (equ (getFVar (v 0)) (num 1))
                        (setFVar (v 0) (num 1))
                        (setFVar (v 1) (num 2)) :: FWhile
            renameAST' a `shouldBe` Right e

        it "renames variables in while statements" $ do
            let a = while (equ (getIVar "x") (num 1))
                        (setIVar "y" (num 1)) :: IWhile
                e = while (equ (getFVar (v 0)) (num 1))
                        (setFVar (v 1) (num 1)) :: FWhile
            renameAST' a `shouldBe` Right e

        it "renames variables in block body" $ do
            let a = setIVar "x" (num 1) `comp`
                        blockI [] [] (setIVar "x" (num 1)) :: IWhile

                e = setFVar (v 0) (num 1) `comp`
                        blockF [] [] (setFVar (v 0) (num 1)) :: FWhile

            renameAST' a `shouldBe` Right e

        it "gives new names to local variables in block" $ do
            let a = setIVar "x" (num 1) `comp`
                        blockI [("x", num 1)] [] (setIVar "x"   (num 1)) :: IWhile

                e = setFVar (v 0) (num 1) `comp`
                        blockF [(v 1, num 1)] [] (setFVar (v 1) (num 1)) :: FWhile

            renameAST' a `shouldBe` Right e

        it "uses local names inside procedures" $ do
            let a = setIVar "x" (num 1) `comp`
                        blockI
                           [("x", num 1)]
                           [("p", setIVar "x" (num 1))]
                           (setIVar "x"   (num 1)) :: IWhile

                e = setFVar (v 0) (num 1) `comp`
                        blockF
                           [(v 1, num 1)]
                           [(p 0, setFVar (v 1) (num 1))]
                           (setFVar (v 1) (num 1)) :: FWhile

            renameAST' a `shouldBe` Right e

        it "restores names after block" $ do
            let a = setIVar "x"   (num 1) `comp` blockI [("x", num 2)] [] skip `comp` setIVar "x"   (num 3) :: IWhile
                e = setFVar (v 0) (num 1) `comp` blockF [(v 1, num 2)] [] skip `comp` setFVar (v 0) (num 3) :: FWhile

            renameAST' a `shouldBe` Right e

        it "returns 0 as the next free procedure name if no procedures were created" $ do
            let a = setIVar "x"   (num 1) :: IWhile
                e = setFVar (v 0) (num 1) :: FWhile
            renameAST a `shouldBe` Right (e, 0)

        it "returns n+1 as the next free procedure name, if n procedures were created" $ do
            let a = blockI [] [("f", skip), ("g", skip)] skip :: IWhile
                e = blockF [] [(p 0, skip), (p 1, skip)] skip :: FWhile
            renameAST a `shouldBe` Right (e, 2)
