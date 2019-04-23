{-# LANGUAGE TypeOperators #-}

module Helper.Eff.NewSpec where

import Data.Word (Word)
import Test.Hspec
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.New
import Helper.Eff.Void

type P = Prog (New Word :+: Void) Void

runP :: P a -> (a, Word)
runP = handleVoid . handleNew 0

new' :: Prog (New Word :+: Void) Void Word
new' = new

newSpec :: Spec
newSpec = do
    describe "new effect handler" $ do
        it "gives new number" $ do
            let p = do x <- new; return x :: P Word
            runP p `shouldBe` (0, 1)

        it "increments new number" $ do
            let p = do _ <- new'; x <- new'; return x :: P Word
            runP p `shouldBe` (1, 2)
