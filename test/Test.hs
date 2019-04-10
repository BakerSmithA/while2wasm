module Main where

import Test.Hspec
import Front.Parse.ParserSpec
import Helper.Eff.StateSpec
import Helper.Eff.FreshSpec
import Helper.Eff.ReaderSpec
import Helper.Eff.WriterSpec
import Transform.Rename.RenameEffSpec
import Transform.Rename.RenameSpec
import Transform.Capture.LocationEffSpec
import Transform.Capture.LocationSpec
import Transform.Capture.DirtyEffSpec
import Transform.Capture.DirtySpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        stateSpec
        freshSpec
        readerSpec
        writerSpec
        renameEffSpec
        renameSpec
        locationEffSpec
        locationSpec
        dirtyEffSpec
        dirtySpec
