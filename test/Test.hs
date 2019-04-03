module Main where

import Test.Hspec
import Front.Parse.ParserSpec
import Helper.Eff.StateSpec
import Helper.Eff.FreshSpec
import Helper.Eff.ReaderSpec
import Transform.Rename.RenameEffSpec
import Transform.RenameSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        stateSpec
        freshSpec
        readerSpec
        renameEffSpec
        renameSpec
