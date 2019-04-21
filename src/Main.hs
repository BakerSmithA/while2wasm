{-# LANGUAGE TypeOperators #-}

module Main where

import Data.Set as Set (Set, elems)
import System.Environment
import Text.Megaparsec (runParser, errorBundlePretty)
import Front.Parse.Parser
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

tryRename :: While Ident Ident -> IO (While FreshName FreshName, FreshName)
tryRename ast = do
    case renameAST ast of
        Left err               -> ioError (userError (show err))
        Right (ast', nextProc) -> return (ast', nextProc)

runComp :: FilePath -> FilePath -> IO ()
runComp inPath outPath = do
    contents <- readFile inPath
    case runParser stms inPath contents of
        Left err -> putStrLn (errorBundlePretty err)
        Right parsed -> do
            let ast =  free parsed   :: While Ident Ident
            (renamed, nextProc) <- tryRename ast

            let dirty = dirtyVars renamed :: Set FreshName
                (mainVars, funcVars) = procVarLocations renamed

            let wasmModule = compile mainVars funcVars dirty renamed
                wat        = docModule wasmModule

            putStrLn "-- Parsed --"
            putStrLn (Pretty.toString 1 $ docAST ast)

            putStrLn "\n-- Renamed --"
            putStrLn (Pretty.toString 1 $ docAST renamed)

            putStrLn "\n-- Analysis --"
            putStrLn $ "  Dirty vars: " ++ show (Set.elems dirty)

            putStrLn "\n-- WASM --"
            putStrLn (Pretty.toString 1 $ wat)
            putStrLn ""

            writeFile outPath (Pretty.toString 0 wat)

main :: IO ()
main = do
    args <- getArgs
    let usageMsg = "Useage example: w2w <in_file> <out_file>"
    case args of
        [inPath, outPath] -> runComp inPath outPath
        _                 -> error usageMsg
