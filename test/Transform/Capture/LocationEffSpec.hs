{-# LANGUAGE TypeOperators #-}

module Transform.Capture.LocationEffSpec where

import Test.Hspec
import Data.Set as Set
import Transform.Capture.LocationEff
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Void

type P = Prog (LocOp :+: Void) (DiscardLocals :+: Void)

runP :: P a -> a
runP = handleVoid . handleLocs

locationEffSpec :: Spec
locationEffSpec = do
    describe "location effect handler" $ do
        it "returns variable as local if seen at top-level" $ do
            let p = do seen 0; seen 1; getLocations :: P Locations
            runP p `shouldBe` (Set.fromList [0, 1], Set.empty)

        it "returns variable as foreign if seen is discard block" $ do
            let p = discardLocals (do seen 0; seen 1; getLocations) :: P Locations
            runP p `shouldBe` (Set.empty, Set.fromList [0, 1])

        it "returns variable as local if add local inside discard" $ do
            let p = discardLocals (do addLocal 0; seen 1; getLocations) :: P Locations
            runP p `shouldBe` (Set.fromList [0], Set.fromList [1])
