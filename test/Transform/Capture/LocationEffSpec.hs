{-# LANGUAGE TypeOperators #-}

module Transform.Capture.LocationEffSpec where

import Test.Hspec
import Data.Set as Set
import Transform.Capture.LocationEff
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Void

type P = Prog (LocOp :+: Void) (AddLocals :+: DiscardLocals :+: Void)

runP :: P a -> a
runP = handleVoid . handleLocs

locationEffSpec :: Spec
locationEffSpec = do
    describe "location effect handler" $ do
        it "makes all variables local at top-level" $ do
            let p = do seen 0; seen 1; getLocations :: P Locations
            runP p `shouldBe` (Set.fromList [0, 1], Set.empty)

        it "makes variables added using addLocals local to scope" $ do
            let p = do seen 0; addLocals [1, 2] (return ()); getLocations :: P Locations
            runP p `shouldBe` (Set.fromList [0, 1, 2], Set.empty)

        -- it "does not include variables inside discard block" $ do
        --     let p = do seen "x"; discardLocals (seen "y"); seen "z"; getLocations :: P (Map String Location)
        --     runP p `shouldBe` Map.fromList [("x", Local), ("z", Local)]
        --
        -- it "makes discarded variables foreign" $ do
        --     let p = discardLocals (do seen "x"; getLocations) :: P (Map String Location)
        --     runP p `shouldBe` Map.fromList [("x", Foreign)]
        --
        -- it "addLocal overwrites discard" $ do
        --     let p = discardLocals (addLocals ["x"] (do seen "x"; getLocations)) :: P (Map String Location)
        --     runP p `shouldBe` Map.fromList [("x", Local)]
        --
        -- it "addLocal only overwrites discard for specific variables" $ do
        --     let p = discardLocals (addLocals ["x"] (do seen "y"; getLocations)) :: P (Map String Location)
        --     runP p `shouldBe` Map.fromList [("x", Local), ("y", Foreign)]
        --
        -- it "variables at lower scopes are propagated to upper scopes" $ do
        --     -- In example below, x is used in top-most scope, and in bottom-most
        --     -- scope but not directly in middle scope. However, WASM code will
        --     -- be required to pass x through middle scope into bottom scope.
        --     -- Therefore, x must have a mapping to a location in the middle scope.
        --     --
        --     -- begin
        --     --   var x := 1;
        --     --   proc f is (
        --     --     begin
        --     --       proc g is x := 2;
        --     --       skip
        --     --     end
        --     --   );
        --     --   skip
        --     -- end
        --     let p = discardLocals (do discardLocals (seen "x"); getLocations) :: P (Map String Location)
        --     runP p `shouldBe` Map.fromList [("x", Foreign)]
