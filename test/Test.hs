module Main where

import Test.Hspec
import Front.Parse.ParserSpec
import Helper.Eff.StateSpec
-- import Transform.RenameSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        stateSpec
        -- renameSpec
