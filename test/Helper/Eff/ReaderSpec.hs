{-# LANGUAGE TypeOperators #-}

module Helper.Eff.ReaderSpec where

import Test.Hspec
import Helper.Prog
import Helper.Co
import Helper.Eff.Reader
import Helper.Eff.Void

type P = Prog (Ask String :+: Void) Void

runP :: P a -> String -> a
runP p s = (handleVoid . handleReader s) p

readerSpec :: Spec
readerSpec = do
    describe "reader effect handler" $ do
        it "asks for environment" $ do
            let p = do env <- ask; return env :: P String
            runP p "hello" `shouldBe` "hello"
