{-# LANGUAGE TypeOperators #-}

module Transform.Capture.LocationSpec where

import Data.Set as Set
import Test.Hspec
import Front.AST
import Transform.Capture.Dirty
import Helper.Free.Free
import Helper.Co

type IWhile = While Ident Ident

block' :: [(Ident, IWhile)] -> [(Ident, IWhile)] -> IWhile -> IWhile
block' = block

locationSpec :: Spec
locationSpec = do
    describe "procVarLocations" $ do
        it "returns local variables of top-level" $ do
            pending

        it "returns mapping from procedures to local variables" $ do
            pending

        it "returns mapping from procedures to foreign variables" $ do
            pending

        it "makes foreign variables part of procedure if passed though" $ do
            pending
