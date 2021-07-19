module Parser where

import Data.Functor
import Control.Applicative
import Data.Char

newtype Parser a = P { runP :: String -> [(String, a)]}

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f (P pa) = P $ \s -> [(s', f a) | (s', a) <- pa s]

instance Applicative Parser where
  pure a = P $ \s -> [(s, a)]
  (P pab) <*> (P pa) = P $ \s -> [ (s'', f a)| (s', f) <- pab s, (s'', a) <- pa s']

instance Alternative Parser where
  empty = P $ const []
  (<|>) = (<<>>)

(<<>>) :: Parser a -> Parser a -> Parser a 
(P pa) <<>> P pb = P $ \s -> pa s ++ pb s

(<<|) :: Parser a -> Parser a -> Parser a 
P pa <<| P pb = P $ \s -> case pa s of
  [] -> pb s
  x -> x

parse :: Parser a -> String -> [(String, a)]
parse = runP

anyChar :: Parser Char
anyChar = satisfy (const True) 

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = P $ \s -> case s of
  "" -> []
  (c : cs) -> if p c then [(cs, c)] else []

char :: Char -> Parser Char
char c = satisfy (==c) 

string :: String -> Parser String
string str = sequenceA $ map char str 

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

exprP :: Parser Expr
exprP = (App <$> atomP <*> exprP) <<| atomP 

identifierP :: Parser Identifier
identifierP = spaces *> some (satisfy Data.Char.isLetter)

varP :: Parser Expr
varP = Var <$> identifierP 

parenP :: Parser Expr
parenP = (spaces *> char '(') *> exprP <* (spaces *> char ')')

lambdaP :: Parser Expr
lambdaP = Lam <$> (spaces *> char '\\' *> identifierP)
              <*> (spaces *> string "->" *> exprP)

atomP :: Parser Expr
atomP = varP <|> lambdaP <|> parenP




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
