{-# LANGUAGE TypeOperators #-}

module Helper.Eff.WriterSpec where

import Test.Hspec
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Writer
import Helper.Eff.Void

type P = Prog (Tell String :+: Void) Void

runP :: P a -> (a, String)
runP = handleVoid . handleWriter

writerSpec :: Spec
writerSpec = do
    describe "writer effect handler" $ do
        it "no telling leaves empty environment" $ do
            let p = return 1 :: P Int
            runP p  `shouldBe` (1, "")

        it "telling writes to environment" $ do
            let p = do tell "a"; tell "b"; tell "c"; return 1 :: P Int
            runP p `shouldBe` (1, "abc")
