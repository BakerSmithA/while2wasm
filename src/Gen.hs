
-- Since Megaparsec is not built for profiling, need to manually generate
-- While which can be compiled.

module Gen (prog) where

import Front.Parse.Rec

block :: Stm -> Stm
block body = Block [("x", Num 1)] [("p", body)] body

prog :: Int -> Stm
prog 0 = Export (Ident "x")
prog n = block (prog (n-1))
