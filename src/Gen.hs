
-- Since Megaparsec is not built for profiling, need to manually generate
-- While which can be compiled.

module Gen (prog) where

import Front.Parse.Rec

prog :: Int -> Stm
prog 0 = Export (Ident "x")
prog n = Assign "x" (Num 1) `Comp` prog (n-1)
