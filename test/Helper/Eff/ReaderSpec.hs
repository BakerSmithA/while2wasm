{-# LANGUAGE TypeOperators #-}

module Helper.Eff.ReaderSpec where

import Test.Hspec
import Helper.Scope.Prog
import Helper.Co
import Helper.Eff.Reader
import Helper.Eff.Void

type P = Prog (Ask String :+: Void) (LocalR String :+: Void)

runP :: P a -> String -> a
runP p s = (handleVoid . handleReader s) p

readerSpec :: Spec
readerSpec = do
    describe "reader effect handler" $ do
        it "asks for environment" $ do
            let p = ask :: P String
            runP p "hello" `shouldBe` "hello"

        it "gets nested environment" $ do
            let p = localR "world" ask :: P String
            runP p "hello" `shouldBe` "world"

        it "restores environment after exiting scope" $ do
            let p = do localR "world" (return ()); ask :: P String
            runP p "hello" `shouldBe` "hello"
