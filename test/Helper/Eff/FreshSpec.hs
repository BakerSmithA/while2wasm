{-# LANGUAGE TypeOperators #-}

module Helper.Eff.FreshSpec where

import Data.Word (Word)
import Test.Hspec
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Fresh
import Helper.Eff.Void

type P = Prog (Fresh :+: Void) Void

runP :: P a -> (a, Word)
runP = handleVoid . handleFresh 0

freshSpec :: Spec
freshSpec = do
    describe "state effect handler" $ do
        it "gives fresh number" $ do
            let p = do f <- fresh; return f :: P Word
            runP p `shouldBe` (0, 1)

        it "increments fresh number" $ do
            let p = do _ <- fresh; f2 <- fresh; return f2 :: P Word
            runP p `shouldBe` (1, 2)
