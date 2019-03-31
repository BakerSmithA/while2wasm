module Transform.RenameSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Front.AST
import Transform.Rename

renameSpec :: Spec
renameSpec = do
    describe "renaming" $ do
        context "variables" $ do
            it "renames getting variables" $ do
                let a = getVar "x" (num 1)
                    e = getVar 0 (num 1)
                rename a `shouldBe` e
