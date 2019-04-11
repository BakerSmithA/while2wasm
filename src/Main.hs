{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Set (Set)
import System.Environment
import Text.Megaparsec (runParser, errorBundlePretty)
import Front.Parse.Parser
import Front.AST
import Front.Pretty
import Transform.Rename.Rename
import Transform.Capture.Dirty
import Transform.Capture.Location
import Back.AST
import Helper.Free.Free
import Helper.Co
import Helper.Pretty as Pretty

tryRename :: While Ident Ident -> IO (While FreshName FreshName)
tryRename ast = do
    case renameAST ast of
        Left err -> ioError (userError (show err))
        Right rn -> return rn

runComp :: FilePath -> FilePath -> IO ()
runComp inPath outPath = do
    contents <- readFile inPath
    case runParser stms inPath contents of
        Left err -> putStrLn (errorBundlePretty err)
        Right parsed -> do
            let ast =  free parsed   :: While Ident Ident
            renamed <- tryRename ast

            let dirty = dirtyVars renamed :: Set FreshName
                (mainVarLocs, funcVarLocs) = procVarLocations renamed

            let wasmModule = compile mainVarLocs funcVarLocs dirty renamed

            putStrLn (Pretty.toString 0 $ docAST ast)
            putStrLn (Pretty.toString 0 $ docAST renamed)

main :: IO ()
main = do
    args <- getArgs
    let usageMsg = "Useage example: w2w <in_file> <out_file>"
    case args of
        [inPath, outPath] -> runComp inPath outPath
        _                 -> error usageMsg
