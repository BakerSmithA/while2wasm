module Main where

import Test.Hspec
import Front.Parse.ParserSpec
import Transform.RenameSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
        renameSpec
