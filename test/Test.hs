module Main where

import Test.Hspec
import Front.Parse.ParserSpec
import Helper.Eff.StateSpec
import Helper.Eff.FreshSpec
import Helper.Eff.ReaderSpec
import Helper.Eff.WriterSpec
import Transform.Rename.RenameEffSpec
import Transform.Rename.RenameSpec
-- import Transform.Capture.LocationEffSpec
-- import Transform.Capture.DirtyEffSpec
-- import Transform.RenameSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        stateSpec
        freshSpec
        readerSpec
        renameEffSpec
        renameSpec
        -- writerSpec
        -- locationEffSpec
        -- dirtyEffSpec
