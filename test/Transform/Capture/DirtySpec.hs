{-# LANGUAGE TypeOperators #-}

module Transform.Capture.DirtySpec where

import Data.Set as Set
import Test.Hspec
import Front.AST
import Transform.Capture.Dirty
import Helper.Free.Free
import Helper.Co

type IWhile = While Ident Ident

block' :: [(Ident, IWhile)] -> [(Ident, IWhile)] -> IWhile -> IWhile
block' = block

dirtySpec :: Spec
dirtySpec = do
    describe "dirty vars" $ do
        it "does not include variables that are only modified at one scope" $ do
            let a = setVar "x" (num 1) `comp` while true (setVar "x" (num 2)):: IWhile
            dirtyVars a `shouldBe` (Set.empty :: Set Ident)

        it "includes variables modified in proc and outside block" $ do
            let p = ("p", setVar "x" (num 2))
                a = setVar "x" (num 1) `comp` block' [] [p] skip :: IWhile
            dirtyVars a `shouldBe` (Set.singleton "x" :: Set Ident)

        it "incldues variables modified in proc and declared in var decls" $ do
            let p = ("p", setVar "x" (num 2))
                a = block [("x", num 1)] [p] skip :: IWhile
            dirtyVars a `shouldBe` (Set.singleton "x" :: Set Ident)
