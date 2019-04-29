{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Set as Set (Set, elems)
import System.Environment
-- import Text.Megaparsec (runParser, errorBundlePretty)
-- import Front.Parse.Parser
import qualified Front.Parse.Rec as Rec
import Front.AST
import Front.Pretty
import Transform.Rename.Rename
import Transform.Capture.Dirty
import Transform.Capture.Location
import Back.Compile
import Back.WAT
import Helper.Free.Free
import Helper.Co
import Helper.Pretty as Pretty

import Gen

tryRename :: While Ident Ident -> IO (While FreshName FreshName, FreshName)
tryRename ast = do
    case renameAST ast of
        Left err               -> ioError (userError (show err))
        Right (ast', nextProc) -> return (ast', nextProc)

runComp' :: Rec.Stm -> FilePath -> IO ()
runComp' parsed outPath = do
    let ast =  free parsed   :: While Ident Ident
    (renamed, nextProc) <- tryRename ast

    let dirty = dirtyVars renamed :: Set FreshName
        (mainVars, funcVars) = procVarLocations renamed

    let wasmModule = compile nextProc mainVars funcVars dirty renamed
        wat        = docModule wasmModule

    writeFile outPath (Pretty.toString 0 wat)

main :: IO ()
main = do
    args <- getArgs
    let usageMsg = "Useage example: w2w <depth> <out_file>"
    case args of
        [depth, outPath] -> runComp' (prog (read depth)) outPath
        _                -> error usageMsg
