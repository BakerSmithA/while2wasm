{-# LANGUAGE TypeOperators #-}

module Transform.Capture.LocationSpec where

import Data.Map as Map
import Data.Set as Set
import Test.Hspec
import Front.AST
import Transform.Rename.Rename (FreshName)
import Transform.Capture.Location

type FWhile = While FreshName FreshName

block' :: [(FreshName, FWhile)] -> [(FreshName, FWhile)] -> FWhile -> FWhile
block' = block

v :: Word -> FreshName
v = id

p :: Word -> FreshName
p = id

locationSpec :: Spec
locationSpec = do
    describe "procVarLocations" $ do
        it "returns local variables of top-level" $ do
            let a = setVar (v 0) (num 1) `comp` block' [(v 1, num 2)] [] skip :: FWhile
                e = (Set.fromList [0, 1], Map.empty)
            procVarLocations a `shouldBe` e

        it "returns mapping from procedures to local variables" $ do
            let p0Body = block' [(v 0, num 0), (v 1, num 1)] [] skip
                p1Body = block' [(v 2, num 2), (v 3, num 3)] [] skip
                a      = block' [] [(p 0, p0Body), (p 1, p1Body)] skip

                p0Locals = Set.fromList [v 0, v 1]
                p1Locals = Set.fromList [v 2, v 3]
                expMap   = Map.fromList [(p 0, (p0Locals, Set.empty)), (p 1, (p1Locals, Set.empty))]
                e        = (Set.empty, expMap)

            procVarLocations a `shouldBe` e

        it "returns mapping from procedures to foreign variables" $ do
            let p0Body = setVar (v 0) (num 0)
                p1Body = setVar (v 1) (num 1)
                a      = block' [] [(p 0, p0Body), (p 1, p1Body)] skip

                p0Foreigns = Set.fromList [v 0]
                p1Foreigns = Set.fromList [v 1]
                expMap     = Map.fromList [(p 0, (Set.empty, p0Foreigns)), (p 1, (Set.empty, p1Foreigns))]

            snd (procVarLocations a) `shouldBe` expMap

        it "makes foreign variables local to top-level" $ do
            let p0Body = setVar (v 0) (num 0)
                p1Body = setVar (v 1) (num 1)
                a      = block' [] [(p 0, p0Body), (p 1, p1Body)] skip
                e      = Set.fromList [v 0, v 1]

            fst (procVarLocations a) `shouldBe` e

        it "makes foreign variables part of procedure if passed though" $ do
            -- Required because variables may need to be passed through WASM
            -- functions. E.g. in example below variable x needs to be passed
            -- from main WASM function, through f, and into g.
            --
            -- begin
            --   proc f is (
            --     begin
            --       prog g is x := 1;
            --       skip
            --     end
            --   );
            --   skip
            -- end
            let g = (p 1, setVar (v 0) (num 1))
                f = (p 0, block' [] [g] skip)
                a = block' [] [f] skip

                gForeigns = Set.fromList [v 0]
                fForeigns = Set.fromList [v 0]
                expMap    = Map.fromList [(p 0, (Set.empty, fForeigns)), (p 1, (Set.empty, gForeigns))]

            snd (procVarLocations a) `shouldBe` expMap
