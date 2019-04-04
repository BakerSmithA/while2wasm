{-# LANGUAGE TypeOperators #-}

module Transform.Capture.DirtyEffSpec where

import Data.Set as Set
import Test.Hspec
import Transform.Capture.DirtyEff
import Helper.Prog
import Helper.Co
import Helper.Eff.Void

type P = Prog (Modified String :+: Void) (ModScope :+: Void) ()

runP :: P -> DirtyVars String
runP = snd . handleVoid . handleDirtyVars

dirtyEffSpec :: Spec
dirtyEffSpec = do
    describe "dirty effect handler" $ do
        it "does not include variables that are only modified at one scope 1" $ do
            let p = modified "x" :: P
            runP p `shouldBe` Set.empty

        it "does not include variables that are only modified at one scope 2" $ do
            let p = do modified "x"; modScope (return ()) :: P
            runP p `shouldBe` Set.empty

        it "does not include variables that are only modified at one scope 3" $ do
            let p = do modified "x"; modScope (modified "y") :: P
            runP p `shouldBe` Set.empty

        it "makes variables dirty if modified in multiple scopes 1" $ do
            let p = do modified "x"; modScope (modified "x") :: P
            runP p `shouldBe` Set.singleton "x"

        it "makes variables dirty if modified in multiple scopes 2" $ do
            let p = do modScope (modified "x"); modified "x" :: P
            runP p `shouldBe` Set.singleton "x"

        it "makes variables dirty if modified in multiple nested scopes 2" $ do
            let p = do modified "x"; modScope (modScope (modified "x")) :: P
            runP p `shouldBe` Set.singleton "x"

        it "makes variables dirty if modified in multiple nested scopes 2" $ do
            let p = do modScope (modScope (modified "x")); modified "x" :: P
            runP p `shouldBe` Set.singleton "x"

        it "keeps variable dirty if seen modified multiple times in different scopes" $ do
            let p = do modified "x"; modScope (modified "x"); modified "x" :: P
            runP p `shouldBe` Set.singleton "x"

        it "keeps track of multiple dirty variables" $ do
            let p = do modified "y"; modScope (do modified "y"; modified "x"); modified "x" :: P
            runP p `shouldBe` Set.fromList ["x", "y"]
