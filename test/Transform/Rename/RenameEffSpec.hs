{-# LANGUAGE TypeOperators #-}

module Transform.Rename.RenameEffSpec where

import Test.Hspec
import Transform.Rename.RenameEff
import Helper.Prog
import Helper.Co
import Helper.Eff.Void

type P = Prog (Rename :+: Void) (LocalName :+: Void)

runP :: P a -> a
runP = handleVoid . handleRename

renameEffSpec :: Spec
renameEffSpec = do
    describe "rename effect handler" $ do
        -- it "generates a fresh name" $ do
        --     let p = do v <- name "x"; return v :: P FreshName
        --     runP p `shouldBe` FreshName "v" 0
        --
        -- it "generates different fresh names for different variables" $ do
        --     let p = do v1 <- name "x"; v2 <- name "y"; return (v1, v2) :: P (FreshName, FreshName)
        --     runP p `shouldBe` (FreshName "v" 0, FreshName "v" 1)
        --
        -- it "uses fresh name if same variable encountered at same scope" $ do
        --     let p = do v1 <- name "x"; v2 <- name "x"; return (v1, v2) :: P (FreshName, FreshName)
        --     runP p `shouldBe` (FreshName "v" 0, FreshName "v" 0)
        --
        -- it "gives fresh names to existing varibles in local scope" $ do
        --     let p = do v1 <- name "x"; v2 <- localNames ["x"] (name "x"); return (v1, v2) :: P (FreshName, FreshName)
        --     runP p `shouldBe` (FreshName "v" 0, FreshName "v" 1)

        it "does not persist existing mappings outside of local scope" $ do
            let p = do v1 <- name "x"
                       localNames ["x"] (name "x")
                       v2 <- name "x"
                       return (v1, v2) :: P (FreshName, FreshName)

            runP p `shouldBe` (FreshName "v" 0, FreshName "v" 0)

        it "persists new mappings outside scope" $ do
            pending
