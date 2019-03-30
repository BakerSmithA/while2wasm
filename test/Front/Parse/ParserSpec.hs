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
                runParser aexp "" "var_name" `shouldParse` Ident "var_name"

            it "parses multiplication" $ do
                runParser aexp "" "123 * x" `shouldParse` Mul (Num 123) (Ident "x")

            it "parses addition" $ do
                runParser aexp "" "123 + x" `shouldParse` Add (Num 123) (Ident "x")

            it "parses subtraction" $ do
                runParser aexp "" "123 - x" `shouldParse` Sub (Num 123) (Ident "x")

            it "parses parenthesis" $ do
                runParser aexp "" "(x)" `shouldParse` Ident "x"

        context "bexp" $ do
            it "parses true" $ do
                runParser bexp "" "true" `shouldParse` T

            it "parses false" $ do
                runParser bexp "" "false" `shouldParse` F

            it "parses aexp equaNumy" $ do
                let lhs = Add (Num 1) (Ident "x")
                    rhs = Ident "y"
                    exp = Equ lhs rhs
                runParser bexp "" "1+x = y" `shouldParse` exp

            it "parses aexp less-than-or-equal-to" $ do
                let exp = LEq (Num 7) (Ident "y")
                runParser bexp "" "7 <= y" `shouldParse` exp

            it "parses not" $ do
                runParser bexp "" "!true" `shouldParse` Not T

            it "parses and" $ do
                runParser bexp "" "true && false" `shouldParse` And T F

        context "stms" $ do
            it "parses Assign" $ do
                runParser stms "" "x := 1" `shouldParse` Assign "x" (Num 1)

            it "parses skip" $ do
                runParser stms "" "skip" `shouldParse` Skip

            it "parses if" $ do
                let s   = "if true then x := 1 else skip"
                    exp = If T (Assign "x" (Num 1)) Skip
                runParser stms "" s `shouldParse` exp

            it "parses while" $ do
                let s   = "while true do x := 1"
                    exp = While T (Assign "x" (Num 1))
                runParser stms "" s `shouldParse` exp

            it "parses blocks with variable declarations" $ do
                let s   = "begin var x:=1; var y:=2; skip end"
                    exp = Block [("x", Num 1), ("y", Num 2)] [] Skip
                runParser stms "" s `shouldParse` exp

            it "parses blocks with procedure declarations" $ do
                let s   = "begin proc f is skip; proc h is skip; skip end"
                    exp = Block [] [("f", Skip), ("h", Skip)] Skip
                runParser stm "" s `shouldParse` exp

            it "parses blocks with variable and procedure declarations" $ do
                let s   = "begin var x:=1; proc f is skip; skip end"
                    exp = Block [("x", Num 1)] [("f", Skip)] Skip
                runParser stm "" s `shouldParse` exp

            it "parses composition" $ do
                let s   = "x := 1; skip; while true do skip"
                    exp = Comp (Assign "x" (Num 1)) (Comp Skip (While T Skip))
                runParser stms "" s `shouldParse` exp

            it "parses nested composition" $ do
                let s    = "if x=0 then skip else skip; export x"
                    exp  = If (Equ (Ident "x") (Num 0)) Skip Skip `Comp` Export (Ident "x")
                runParser stms "" s `shouldParse` exp

            it "parses exports" $ do
                let s   = "export x"
                    exp = Export (Ident "x")
                runParser stms "" s `shouldParse` exp

        context "parsing sample programs" $ do
            it "parses factorial" $ do
                let s = "y:=1; while !(x=1) do (y:=y*x; x:=x-1)"
                    exp = Comp (Assign "y" (Num 1)) (While (Not (Equ (Ident "x") (Num 1))) (Comp (Assign "y" (Mul (Ident "y") (Ident "x"))) (Assign "x" (Sub (Ident "x") (Num 1)))))
                runParser stms "" s `shouldParse` exp
