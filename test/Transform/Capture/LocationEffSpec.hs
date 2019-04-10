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
runP = fst . handleVoid . handleLocs

runPTopLevel :: P a -> Locations
runPTopLevel = snd . handleVoid . handleLocs

locationEffSpec :: Spec
locationEffSpec = do
    describe "location effect handler" $ do
        it "returns variable as local if seen at top-level" $ do
            let p = do seen 0; seen 1; getLocations :: P Locations
            runP p `shouldBe` (Set.fromList [0, 1], Set.empty)

        it "returns variable as foreign if seen inside discard block" $ do
            let p = discardLocals (do seen 0; seen 1; getLocations) :: P Locations
            runP p `shouldBe` (Set.empty, Set.fromList [0, 1])

        it "returns variable as local if add local inside discard" $ do
            let p = discardLocals (do addLocal 0; seen 1; getLocations) :: P Locations
            runP p `shouldBe` (Set.fromList [0], Set.fromList [1])

        it "does nothing if a seen variable has already been added as local" $ do
            let p = discardLocals (do addLocal 0; seen 0; getLocations) :: P Locations
            runP p `shouldBe` (Set.fromList [0], Set.empty)

        it "after discard block, foreign variables are added to foreigns of above scope" $ do
            let p = do discardLocals (seen 0); getLocations :: P Locations
            runP p `shouldBe` (Set.empty, Set.fromList [0])

        it "after discard block, foreign variables are not added to foreigns if already seen local" $ do
            let p = do addLocal 0; discardLocals (do seen 0; seen 1); getLocations :: P Locations
            runP p `shouldBe` (Set.fromList [0], Set.fromList [1])

        it "also returns locations of variables at top-level" $ do
            let p = do seen 0; discardLocals (seen 1); seen 2 :: P ()
            runPTopLevel p `shouldBe` (Set.fromList [0, 2], Set.fromList [1])
