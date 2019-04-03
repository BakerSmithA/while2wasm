{-# LANGUAGE TypeOperators #-}

module Helper.Eff.StateSpec where

import Test.Hspec
import Helper.Prog
import Helper.Co
import Helper.Eff.State
import Helper.Eff.Void

type P = Prog (State Int :+: Void) (LocalSt Int :+: Void)

runP :: P a -> Int -> (a, Int)
runP p s = (handleVoid . handleState s) p

stateSpec :: Spec
stateSpec = do
    describe "state effect handler" $ do
        it "gets current state" $ do
            let p = do env <- get; return env :: P Int
            runP p 1 `shouldBe` (1, 1)

        it "puts updated state" $ do
            let p = do put (2 :: Int); env <- get; return env :: P Int
            runP p 1 `shouldBe` (2, 2)

        it "creates local state" $ do
            let p = do r <- local (100 :: Int) get; return r :: P Int
            runP p 1 `shouldBe` (100, 1)
