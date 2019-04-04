{-# LANGUAGE TypeOperators #-}

module Main where

import System.Environment
import Text.Megaparsec (runParser, errorBundlePretty)
import Front.Parse.Parser
import Front.AST
import Transform.Pretty
import Transform.Rename
import Helper.Prog
import Helper.Co

type Op    v p = VarExp v :+: AExp :+: BExp :+: VarStm v :+: ProcStm p :+: Stm
type Scope v p = ScopeStm :+: BlockStm v p
type While v p = Prog (Op v p) (Scope v p) ()

tryRename :: While Ident Ident -> IO (While FreshName FreshName)
tryRename ast = do
    case rename ast of
        Left err -> ioError (userError err)
        Right rn -> return rn

runComp :: FilePath -> FilePath -> IO ()
runComp inPath outPath = do
    contents <- readFile inPath
    case runParser stms inPath contents of
        Left err -> putStrLn (errorBundlePretty err)
        Right parsed -> do
            let ast = prog parsed :: While Ident Ident
            rn  <- tryRename ast

            putStrLn (show ast)
            putStrLn (show rn)

main :: IO ()
main = do
    args <- getArgs
    let usageMsg = "Useage example: w2w <in_file> <out_file>"
    case args of
        [inPath, outPath] -> runComp inPath outPath
        _                 -> error usageMsg
