{-# LANGUAGE TypeOperators #-}

module Transform.Capture.LocationEffSpec where

import Test.Hspec
import Data.Map as Map
import Transform.Capture.LocationEff
import Helper.Prog
import Helper.Co
import Helper.Eff.Void

type P = Prog (LocOp String :+: Void) (Add String :+: Discard :+: Void)

discard :: P () -> P (Map String Location)
discard inner = do (x, ls) <- discardLocals inner; return ls

runP :: P a -> (a, Map String Location)
runP = handleVoid . handleLoc

runPTop :: P a -> Map String Location
runPTop = snd . runP

runPInner :: P (Map String Location) -> Map String Location
runPInner = fst . runP

locationEffSpec :: Spec
locationEffSpec = do
    describe "location effect handler" $ do
        it "makes all variables are local at top level of scope" $ do
            let p = do seen "x"; seen "y" :: P ()
            runPTop p `shouldBe` Map.fromList [("x", Local), ("y", Local)]

        it "does not include scoped variables at top level" $ do
            let p = do discard (seen "x"); seen "y" :: P ()
            runPTop p `shouldBe` Map.fromList [("y", Local)]

        it "discards local variables" $ do
            let p   = do seen "x"; discard (seen "y") :: P (Map String Location)
                top = Map.fromList [("x", Local)]
                inn = Map.fromList [("y", Foreign)]
            runP p `shouldBe` (inn, top)

        it "adds local variables" $ do
            let p = discard (addLocals ["x"] (return ())) :: P (Map String Location)
            runPInner p `shouldBe` Map.fromList [("x", Local)]

        it "adding local variables accumulates" $ do
            let p = discard (addLocals ["x"] (addLocals ["y"] (return ()))) :: P (Map String Location)
            runPInner p `shouldBe` Map.fromList [("x", Local), ("y", Local)]

        it "does not make local variables foreign if seen" $ do
            let p = discard (addLocals ["x"] (seen "x")) :: P (Map String Location)
            runPInner p `shouldBe` Map.fromList [("x", Local)]

        it "discard makes any added local variables foreign" $ do
            let p = discardLocals (addLocals ["x"] (discard (seen "x"))) :: P (Map String Location, Map String Location)
                top = Map.fromList [("x", Local)]
                inn = Map.fromList [("x", Foreign)]
            fst (runP p) `shouldBe` (inn, top)
