{-
Some references I used and mixed together:
https://www.codewars.com/kata/54f1fdb7f29358dd1f00015d/train/haskell
https://github.com/tsoding/haskell-json
http://research.nii.ac.jp/~hu/download/Parsing10.pdf
Also some Haskell base packages source code (Text.Parsec, Text.ParserCombinators.ReadP) from
https://hackage.haskell.org
-}

{-
Order of doing things
type
parse
anyChar
satisfy
char
Functor
Applicative (*> <* <$ &>)
string
Alternative (<<>> <<|)
identifierP
atomP
spaces
varP
parenP
lambdaP
exprP
-}

module Parser where

import Control.Applicative
import Data.Char
import Data.Functor

-- What is a parser?
-- String -> Tree
-- String -> a
-- String -> (String, a)
-- String -> [(String, a)]
-- There are other ways, like keeping only one answer, or adding failure information
newtype Parser a = P {runP :: String -> [(String, a)]}

-- Changing the type of a parser
instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P pa) = P $ \s -> [(s', f a) | (s', a) <- pa s]

-- Combining parsers together
instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = P $ \s -> [(s, a)]

  -- (<*>) :: Parser (a -> b) -> Parser a -> Parser b
  P sab <*> P sa = P $ \s -> [(s'', f a) | (s', f) <- sab s, (s'', a) <- sa s']

-- Choosing between parsers
instance Alternative Parser where
  -- empty :: Parser a
  empty = P $ const []

  -- (<|>) :: Parser a -> Parser a -> Parser a
  (<|>) = (<<>>)

(<<>>) :: Parser a -> Parser a -> Parser a
P a1 <<>> P a2 = P $ \s -> a1 s ++ a2 s

(<<|) :: Parser a -> Parser a -> Parser a
(P a1) <<| (P a2) = P $ \s -> case a1 s of
  [] -> a2 s
  x -> x

-- -- Monadic parsers can do more, they can check the state of parsing as it goes
-- instance Monad Parser where
--   -- return :: a -> Parser a
--   return = pure
--
--   -- (>>=) :: Parser a -> (a -> Parser b) -> Parser b
--   P p >>= f = P $ \s -> [(s'', b) | (s', a) <- p s, (s'', b) <- runP (f a) s']

parse :: Parser a -> String -> [(String, a)]
parse = runP

anyChar :: Parser Char
-- anyChar = P go
-- where
--   go "" = []
--   go (c : cs) = [(cs, c)]
anyChar = satisfy (const True)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P $ \s -> case s of
  "" -> []
  (c : cs) -> if p c then [(cs, c)] else []

char :: Char -> Parser Char
char c = satisfy (== c)

string :: String -> Parser String
--string str = foldr (\s c -> (:) <$> s <*> c) (pure "") (map char str)
string str = sequenceA (map char str)

between :: Parser b -> Parser e -> Parser m -> Parser m
between begin end middle = begin *> middle <* end

-- many :: Parser a -> Parser [a]
-- many p = (:) <$> p <*> many p <|> pure []

-- some :: Parser a -> Parser [a]
-- some p = (:) <$> p <*> many p

spaces :: Parser ()
spaces = many (char ' ') $> ()

--   PARSING A LAMBDA EXPRESSION

--  lambda  ::= '\' var '->' expr
--  var     ::= non empty string of letters
--  expr    ::= atom expr | atom
--  atom    ::= var | lambda | '(' expr ')'

-- Type of parsing
type Identifier = String

data Expr
  = Var Identifier
  | App Expr Expr
  | Lam Identifier Expr
  deriving (Show, Eq)

identifierP :: Parser Identifier
identifierP = spaces *> some (satisfy Data.Char.isLetter)

exprP :: Parser Expr
exprP = (App <$> atomP <*> exprP) <<| atomP

atomP :: Parser Expr
atomP = varP <|> lambdaP <|> paren

varP :: Parser Expr
varP = Var <$> identifierP

lambdaP :: Parser Expr
lambdaP =
  Lam <$> (spaces *> char '\\' *> identifierP)
    <*> (spaces *> string "->" *> exprP)

paren :: Parser Expr
paren = between (spaces *> char '(') (spaces *> char ')') exprP

-- Testing
main :: IO ()
main = do
  let c_s = "\\ x -> \\ y -> \\ z -> x z (y z)"
      c_k = "\\ x -> \\  y -> x"
      c_i = "\\ x -> x"
      c_b = "\\ x -> \\ y -> \\ z -> x (y z)"
      c_c = "\\ x -> \\ y -> \\ z -> x z y"
      c_fix = "(\\ x -> x x) (\\ x -> x)"
  mapM_ (print . parse exprP) [c_s, c_k, c_i, c_b, c_c, c_fix]
