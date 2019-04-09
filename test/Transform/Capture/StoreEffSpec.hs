{-# LANGUAGE TypeOperators #-}

module Transform.Capture.StoreEffSpec where

import Test.Hspec
import Transform.Capture.StoreEff
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Void

type P = Prog (StoreType String :+: Void) (Add String :+: Discard :+: Void)

runP :: P a -> a
runP = handleVoid . handleStore

storeEffSpec :: Spec
storeEffSpec = do
    describe "store effect handler" $ do
        it "all variables are local at 'top level'" $ do
            let p = storeType "x" :: P (Store String)
            runP p `shouldBe` (Local "x")

        it "discards local variables" $ do
            let p = discardLocals (storeType "x") :: P (Store String)
            runP p `shouldBe` (Foreign "x")

        it "adds local variables" $ do
            let p = discardLocals (addLocals ["x"] (storeType "x")) :: P (Store String)
            runP p `shouldBe` (Local "x")

        it "adding local variables accumulates outer" $ do
            let p = discardLocals (addLocals ["y"] (addLocals ["x"] (storeType "y"))) :: P (Store String)
            runP p `shouldBe` (Local "y")

        it "adding local variables accumulates inner" $ do
            let p = discardLocals (addLocals ["y"] (addLocals ["x"] (storeType "x"))) :: P (Store String)
            runP p `shouldBe` (Local "x")

        it "does not leak discarded variables" $ do
            let p = do discardLocals (return ()); storeType "x" :: P (Store String)
            runP p `shouldBe` (Local "x")

        it "does not leak added variables" $ do
            let p = discardLocals (do addLocals ["x"] (return ()); storeType "x") :: P (Store String)
            runP p `shouldBe` (Foreign "x")
