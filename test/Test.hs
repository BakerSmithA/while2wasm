module Main where

import Test.Hspec
import Front.Parse.ParserSpec

main :: IO ()
main = hspec specs where
    specs = do
        parserSpec
