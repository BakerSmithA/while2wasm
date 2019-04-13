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

getIElem :: VarExp Ident :<: f => Ident -> Free f a -> Free f a
getIElem = getElem

getFElem :: VarExp FreshName :<: f => FreshName -> Free f a -> Free f a
getFElem = getElem

setIVar :: VarStm Ident :<: f => Ident -> Free f () -> Free f ()
setIVar = setVar

setFVar :: VarStm FreshName :<: f => FreshName -> Free f () -> Free f ()
setFVar = setVar

setIElem :: VarStm Ident :<: f => Ident -> Free f () -> Free f () -> Free f ()
setIElem = setElem

setFElem :: VarStm FreshName :<: f => FreshName ->  Free f () -> Free f () -> Free f ()
setFElem = setElem

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

renameSpec :: Spec
renameSpec = do
    describe "renaming" $ do
        it "renames getting variables" $ do
            let a = getIVar "x"   :: IWhile
                e = getFVar (v 0) :: FWhile
            renameAST a `shouldBe` Right e

        it "renames array subscript" $ do
            let a = getIElem "x"   (getIVar "i")   :: IWhile
                e = getFElem (v 0) (getFVar (v 1)) :: FWhile
            renameAST a `shouldBe` Right e

        it "renames variables in arithmetic expressions" $ do
            let a = add (getIVar "x") (getIVar "y") :: IWhile
                e = add (getFVar (v 0)) (getFVar (v 1)) :: FWhile
            renameAST a `shouldBe` Right e

        it "renames variables in boolean expressions" $ do
            let a = equ (getIVar "x") (getIVar "y") :: IWhile
                e = equ (getFVar (v 0)) (getFVar (v 1)) :: FWhile
            renameAST a `shouldBe` Right e

        it "renames assigning variables to aexp" $ do
            let a = setIVar "x"   (assignAExp (num 1)) :: IWhile
                e = setFVar (v 0) (assignAExp (num 1)) :: FWhile
            renameAST a `shouldBe` Right e

        it "renames assigning variables to array" $ do
            let a = setIVar "x"   (assignArr [getVar "y", num 1]) :: IWhile
                e = setFVar (v 0) (assignArr [getVar (v 1), num 1]) :: FWhile
            renameAST a `shouldBe` Right e

        it "renames setting array value" $ do
            let a = setIElem "x"   (getVar "i")   (num 1) :: IWhile
                e = setFElem (v 0) (getVar (v 1)) (num 1) :: FWhile
            renameAST a `shouldBe` Right e

        it "renames procedures" $ do
            let a = blockI [] [("p", skip)] (callI "p")   :: IWhile
                e = blockF [] [(p 0, skip)] (callF (p 0)) :: FWhile
            renameAST a `shouldBe` Right e

        it "fails if calling procedure which has been been declared yet" $ do
            let a = callI "p" :: IWhile
                r = renameAST a  :: Either RenameErr FWhile
            r `shouldBe` Left (UndefinedProc "p")

        it "uses existing fresh name" $ do
            let a = export (getIVar "x")   `comp` export (getIVar "x")   :: IWhile
                e = export (getFVar (v 0)) `comp` export (getFVar (v 0)) :: FWhile
            renameAST a `shouldBe` Right e

        it "renames variables in exports" $ do
            let a = export (getIVar "x") :: IWhile
                e = export (getFVar (v 0)) :: FWhile
            renameAST a `shouldBe` Right e

        it "renames variables in if statements" $ do
            let a = ifElse (equ (getIVar "x") (num 1))
                        (setIVar "x" (assignAExp (num 1)))
                        (setIVar "y" (assignAExp (num 2))) :: IWhile
                e = ifElse (equ (getFVar (v 0)) (num 1))
                        (setFVar (v 0) (assignAExp (num 1)))
                        (setFVar (v 1) (assignAExp (num 2))) :: FWhile
            renameAST a `shouldBe` Right e

        it "renames variables in while statements" $ do
            let a = while (equ (getIVar "x") (num 1))
                        (setIVar "y" (assignAExp (num 1))) :: IWhile
                e = while (equ (getFVar (v 0)) (num 1))
                        (setFVar (v 1) (assignAExp (num 1))) :: FWhile
            renameAST a `shouldBe` Right e

        it "renames variables in block body" $ do
            let a = setIVar "x" (assignAExp (num 1)) `comp`
                        blockI [] [] (setIVar "x" (assignAExp (num 1))) :: IWhile

                e = setFVar (v 0) (assignAExp (num 1)) `comp`
                        blockF [] [] (setFVar (v 0) (assignAExp (num 1))) :: FWhile

            renameAST a `shouldBe` Right e

        it "gives new names to local variables in block" $ do
            let a = setIVar "x" (assignAExp (num 1)) `comp`
                        blockI [("x", num 1)] [] (setIVar "x" (assignAExp (num 1))) :: IWhile

                e = setFVar (v 0) (assignAExp (num 1)) `comp`
                        blockF [(v 1, num 1)] [] (setFVar (v 1) (assignAExp (num 1))) :: FWhile

            renameAST a `shouldBe` Right e

        it "uses local names inside procedures" $ do
            let a = setIVar "x" (assignAExp (num 1)) `comp`
                        blockI
                           [("x", assignAExp (num 1))]
                           [("p", setIVar "x" (assignAExp (num 1)))]
                           (setIVar "x"   (assignAExp (num 1))) :: IWhile

                e = setFVar (v 0) (assignAExp (num 1)) `comp`
                        blockF
                           [(v 1, assignAExp (num 1))]
                           [(p 0, setFVar (v 1) (assignAExp (num 1)))]
                           (setFVar (v 1) (assignAExp (num 1))) :: FWhile

            renameAST a `shouldBe` Right e

        it "restores names after block" $ do
            let a = setIVar "x"   (assignAExp (num 1)) `comp` blockI [("x", num 2)] [] skip `comp` setIVar "x"   (assignAExp (num 3)) :: IWhile
                e = setFVar (v 0) (assignAExp (num 1)) `comp` blockF [(v 1, num 2)] [] skip `comp` setFVar (v 0) (assignAExp (num 3)) :: FWhile

            renameAST a `shouldBe` Right e
