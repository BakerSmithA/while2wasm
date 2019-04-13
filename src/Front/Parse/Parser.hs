module Front.Parse.Parser where

import Front.Parse.Rec (Stm(..), AExp(..), BExp(..), Assign(..), Ident)
import Data.Void
import Control.Monad (void)
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr

type Parser = Parsec Void String

-- Consumes whitespace and comments (Muli-line and single-line).
whitespace :: Parser ()
whitespace = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

-- Succeeds if the input is prefixed with `s`, optionally followed by whitespace.
tok :: String -> Parser String
tok s = chunk s <* whitespace

-- Return parser for `a`, surrounded by parenthesis.
parens :: Parser a -> Parser a
parens = between (tok "(") (tok ")")

-- Return parser for `a`, surrounded by square brackets.
brackets :: Parser a -> Parser a
brackets = between (tok "[") (tok "]")

-- Parsers integer literals.
intLit :: Parser Integer
intLit = do
    n <- some digitChar
    (return (read n)) <* whitespace

-- Checks that the parsed identifier is not a reserved keyword.
reserveCheckedId :: Parser Ident -> Parser Ident
reserveCheckedId p = (p >>= check) <* whitespace where
    check word = if not (word `elem` reserved)
                    then return word
                    else fail $ "keyword " ++ show word ++ " cannot be an identifier"

-- Words that cannot be used as identifiers.
reserved :: [Ident]
reserved = ["true", "false", "var", "skip", "if", "then", "else", "while", "do", "begin", "end", "call", "proc", "is"]

-- Parses snake-case identifier.
snakeId :: Parser Ident
snakeId = reserveCheckedId p where
    p = (:) <$> start <*> many body
    start = lowerChar <|> char '_'
    body = start <|> digitChar

-- Parses aexpmetic expressions.
aexp :: Parser AExp
aexp = makeExprParser basis ops where
    basis = parens aexp
        <|> Num          <$> intLit
        <|> try (GetElem <$> snakeId <*> brackets aexp)
        <|> GetVar       <$> snakeId

    ops = [[InfixL (Mul <$ tok "*")],
           [InfixL (Add <$ tok "+"), InfixL (Sub <$ tok "-")]]

-- Parses bexp expressions.
bexp :: Parser BExp
bexp = makeExprParser basis ops where
    basis = parens bexp
        <|> T <$  tok "true"
        <|> F <$  tok "false"
        <|> try (Equ <$> aexp <* tok "="  <*> aexp)
        <|> LEq      <$> aexp <* tok "<=" <*> aexp

    ops = [[Prefix (Not <$ tok "!")],
           [InfixL (And <$ tok "&&")]]

-- Parses RHS of variable assignment.
assign :: Parser Assign
assign = AssignArr  <$> brackets (aexp `sepBy` tok ",")
     <|> AssignAExp <$> aexp

-- Parses variable declarations as part of a block.
varDecl :: Parser (Ident, Assign)
varDecl = do
    tok "var"; i <- snakeId; tok ":="; x <- assign; tok ";"
    return (i, x)

-- Parses a procedure declaration as part of a block.
procDecl :: Parser (Ident, Stm)
procDecl = do
    tok "proc"; i <- snakeId; tok "is"; s <- innerStm; tok ";"
    return (i, s)

-- Parses statements excluding composition.
stm :: Parser Stm
stm = try (SetVar <$> snakeId <* tok ":=" <*> assign)
  <|> try (SetElem <$> snakeId <*> brackets aexp <* tok ":=" <*> aexp)
  <|> Skip <$ tok "skip"
  <|> If <$ tok "if" <*> bexp <* tok "then" <*> innerStm <* tok "else" <*> innerStm
  <|> While <$ tok "while" <*> bexp <* tok "do" <*> innerStm
  <|> Export <$ tok "export" <*> aexp
  <|> Block <$ tok "begin" <*> many varDecl <*> many procDecl <*> stmComp <* tok "end"
  <|> Call <$ tok "call" <*> snakeId
  <|> parens stmComp

-- Parses statements composed together.
stmComp :: Parser Stm
stmComp = makeExprParser stm ops where
    ops = [[InfixR (Comp <$ tok ";")]]

-- Statement nested in another statement, e.g. body of loop, that are not
-- delimited by another token, e.g. blocks with begin and end tokens.
innerStm :: Parser Stm
innerStm = parens stmComp <|> stm

stms :: Parser Stm
stms = whitespace *> stmComp <* eof
