{-# LANGUAGE TypeOperators #-}

module Transform.Rename.RenameEffSpec where

import Test.Hspec
import Transform.Rename.RenameEff
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Void

type P = Prog (Fresh String :+: Void) (Rename String :+: Void)

runP :: P a -> a
runP = handleVoid . handleRename

renameEffSpec :: Spec
renameEffSpec = do
    describe "rename effect handler" $ do
        it "generates a fresh fresh" $ do
            let p = do v <- fresh "x"; return v :: P FreshName
            runP p `shouldBe` 0

        it "generates different fresh names for different variables" $ do
            let p = do v1 <- fresh "x"; v2 <- fresh "y"; return (v1, v2) :: P (FreshName, FreshName)
            runP p `shouldBe` (0, 1)

        it "uses fresh fresh if same variable encountered at same scope" $ do
            let p = do v1 <- fresh "x"; v2 <- fresh "x"; return (v1, v2) :: P (FreshName, FreshName)
            runP p `shouldBe` (0, 0)

        it "returns true if a variable exists" $ do
            let p = do fresh "x"; ex <- exists "x"; return ex :: P Bool
            runP p `shouldBe` True

        it "returns false if a variable does not exist" $ do
            let p = do ex <- exists "x"; return ex :: P Bool
            runP p `shouldBe` False

        it "gives fresh names to existing varibles in local scope" $ do
            let p = do v1 <- fresh "x"; v2 <- rename ["x"] (fresh "x"); return (v1, v2) :: P (FreshName, FreshName)
            runP p `shouldBe` (0, 1)

        it "does not persist existing mappings outside of local scope" $ do
            let p = do v1 <- fresh "x"
                       rename ["x"] (fresh "x")
                       v2 <- fresh "x"
                       return (v1, v2) :: P (FreshName, FreshName)

            runP p `shouldBe` (0, 0)

        it "persists new mappings outside scope" $ do
            let p = do fresh "x"
                       rename ([] :: [String]) (fresh "y")
                       v <- fresh "y"
                       return v :: P FreshName

            runP p `shouldBe` 1
