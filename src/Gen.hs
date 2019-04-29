
-- Since Megaparsec is not built for profiling, need to manually generate
-- While which can be compiled.

module Gen where

import Front.Parse.Rec

block :: Stm -> Stm
block body = Block [("x", Num 1)] [("p", body)] body

progRec :: Int -> Stm
progRec 0 = Export (Ident "x")
progRec n = block (progRec (n-1))

progLin :: Int -> Stm
progLin n = foldr1 Comp (replicate n (Assign "x" (Num 1)))
