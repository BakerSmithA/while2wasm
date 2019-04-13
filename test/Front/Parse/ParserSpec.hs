module Front.Parse.ParserSpec where

import Test.Hspec
import Test.Hspec.Megaparsec
import Text.Megaparsec (runParser)
import Front.Parse.Rec
import Front.Parse.Parser

parserSpec :: Spec
parserSpec = do
    describe "parser" $ do
        context "snakeId" $ do
            it "parses snake case identifier" $ do
                runParser snakeId "" "var_name" `shouldParse` "var_name"

            it "fails if camel-case" $ do
                runParser snakeId "" `shouldFailOn` "varName"

            it "fails if identifier is reserved" $ do
                runParser snakeId "" `shouldFailOn` "skip"

        context "aexp" $ do
            it "parses numbers" $ do
                runParser aexp "" "123" `shouldParse` Num 123

            it "parses variables" $ do
                runParser aexp "" "var_name" `shouldParse` GetVar "var_name"

            it "parses array subscription" $ do
                runParser aexp "" "xs[0]" `shouldParse` GetElem "xs" (Num 0)

            it "parses multiplication" $ do
                runParser aexp "" "123 * x" `shouldParse` Mul (Num 123) (GetVar "x")

            it "parses addition" $ do
                runParser aexp "" "123 + x" `shouldParse` Add (Num 123) (GetVar "x")

            it "parses subtraction" $ do
                runParser aexp "" "123 - x" `shouldParse` Sub (Num 123) (GetVar "x")

            it "parses parenthesis" $ do
                runParser aexp "" "(x)" `shouldParse` GetVar "x"

        context "bexp" $ do
            it "parses true" $ do
                runParser bexp "" "true" `shouldParse` T

            it "parses false" $ do
                runParser bexp "" "false" `shouldParse` F

            it "parses aexp equaNumy" $ do
                let lhs = Add (Num 1) (GetVar "x")
                    rhs = GetVar "y"
                    exp = Equ lhs rhs
                runParser bexp "" "1+x = y" `shouldParse` exp

            it "parses aexp less-than-or-equal-to" $ do
                let exp = LEq (Num 7) (GetVar "y")
                runParser bexp "" "7 <= y" `shouldParse` exp

            it "parses not" $ do
                runParser bexp "" "!true" `shouldParse` Not T

            it "parses and" $ do
                runParser bexp "" "true && false" `shouldParse` And T F

        context "stms" $ do
            it "parses setting a variable to an aexp" $ do
                runParser stms "" "x := 1" `shouldParse` SetVar "x" (AssignAExp (Num 1))

            it "parses setting a variable to an array" $ do
                runParser stms "" "x := [1, y, 3]" `shouldParse` SetVar "x" (AssignArr [Num 1, GetVar "y", Num 3])

            it "parses setting an array element" $ do
                runParser stms "" "x[0] := y" `shouldParse` SetElem "x" (Num 0) (GetVar "y")

            it "parses skip" $ do
                runParser stms "" "skip" `shouldParse` Skip

            it "parses if" $ do
                let s   = "if true then x := 1 else skip"
                    exp = If T (SetVar "x" (AssignAExp (Num 1))) Skip
                runParser stms "" s `shouldParse` exp

            it "parses while" $ do
                let s   = "while true do x := 1"
                    exp = While T (SetVar "x" (AssignAExp (Num 1)))
                runParser stms "" s `shouldParse` exp

            it "parses blocks with aexp variable declarations" $ do
                let s   = "begin var x:=1; var y:=2; skip end"
                    exp = Block [("x", AssignAExp (Num 1)), ("y", AssignAExp (Num 2))] [] Skip
                runParser stms "" s `shouldParse` exp

            it "parses blocks with array variable declarations" $ do
                let s   = "begin var x:=[1, 2, 3]; var y:=[4, 5, 6]; skip end"
                    exp = Block [("x", AssignArr [Num 1, Num 2, Num 3]), ("y", AssignArr [Num 4, Num 5, Num 6])] [] Skip
                runParser stms "" s `shouldParse` exp

            it "parses blocks with procedure declarations" $ do
                let s   = "begin proc f is skip; proc h is skip; skip end"
                    exp = Block [] [("f", Skip), ("h", Skip)] Skip
                runParser stm "" s `shouldParse` exp

            it "parses blocks with variable and procedure declarations" $ do
                let s   = "begin var x:=1; proc f is skip; skip end"
                    exp = Block [("x", AssignAExp (Num 1))] [("f", Skip)] Skip
                runParser stm "" s `shouldParse` exp

            it "parses composition" $ do
                let s   = "x := 1; skip; while true do skip"
                    exp = Comp (SetVar "x" (AssignAExp (Num 1))) (Comp Skip (While T Skip))
                runParser stms "" s `shouldParse` exp

            it "parses nested composition" $ do
                let s    = "if x=0 then skip else skip; export x"
                    exp  = If (Equ (GetVar "x") (Num 0)) Skip Skip `Comp` Export (GetVar "x")
                runParser stms "" s `shouldParse` exp

            it "parses exports" $ do
                let s   = "export x"
                    exp = Export (GetVar "x")
                runParser stms "" s `shouldParse` exp

        context "parsing sample programs" $ do
            it "parses factorial" $ do
                let s = "y:=1; while !(x=1) do (y:=y*x; x:=x-1)"
                    exp = Comp (SetVar "y" (AssignAExp (Num 1))) (While (Not (Equ (GetVar "x") (Num 1))) (Comp (SetVar "y" (AssignAExp (Mul (GetVar "y") (GetVar "x")))) (SetVar "x" (AssignAExp (Sub (GetVar "x") (Num 1))))))
                runParser stms "" s `shouldParse` exp
