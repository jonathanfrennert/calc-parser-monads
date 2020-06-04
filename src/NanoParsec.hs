{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec (
Expr (..),
run,
eval,
) where

import Data.Char
import Control.Monad
import Control.Applicative

------------------------------ Parser ------------------------------

newtype Parser a = Parser { parse :: String -> [(a, String)]}

runParser :: Parser a -> String -> a
runParser p s =
  case p `parse` s of
    [(result, [])]  -> result
    [(_, leftover)] -> error "Parser did not consume entire stream."
    _               -> error "Parser Error."

item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c,cs)]

-- | Left-recurse on the stream until failure.
chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = do {a <- p; rest a}
  where rest a = (do f <- op
                     b <- p
                     rest (f a b))
                 <|> return a

------------------------------ Combinators ------------------------------

data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving Show

run :: String -> Expr
run = runParser expr

eval :: Expr -> Int
eval ex = case ex of
  Add a b -> eval a + eval b
  Mul a b -> eval a * eval b
  Sub a b -> eval a - eval b
  Lit n   -> n

expr :: Parser Expr
expr = term `chainl1` addop

term :: Parser Expr
term = factor `chainl1` mulop

int :: Parser Expr
int = do
  n <- number
  return (Lit n)

factor :: Parser Expr
factor = int <|> parens expr

infixOp :: String -> (a -> a -> a) -> Parser (a -> a -> a)
infixOp x f = reserved x >> return f

addop :: Parser (Expr -> Expr -> Expr)
addop = (infixOp "+" Add) <|> (infixOp "-" Sub)

mulop :: Parser (Expr -> Expr -> Expr)
mulop = infixOp "*" Mul

------------------------------ Combinators ------------------------------

char :: Char -> Parser Char
char c = satisfy (c ==)

string :: String -> Parser String
string []     = return []
string (c:cs) = do
  char c
  string cs
  return (c:cs)

spaces :: Parser String
spaces = many $ oneOf " \n\r"

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  return a

reserved :: String -> Parser String
reserved s = token (string s)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  return n

digit :: Parser Char
digit = satisfy isDigit

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

number :: Parser Int
number = do
  s <- string "-" <|> return []
  cs <- some digit
  return $ read (s ++ cs)

------------------------------ Combinator helpers ------------------------------

satisfy :: (Char -> Bool) -> Parser Char
satisfy a = item >>= \c ->
  if a c
  then return c
  else failure

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

------------------------------ Parser Monad ------------------------------

-- | Inject a single pure value as the result.
unit :: a           -- ^ Pure value
     -> Parser a
unit a = Parser (\s -> [(a,s)])

-- | Make a composition of two parser operations.
bind :: Parser a          -- ^ Parser with the first operation
     -> (a -> Parser b)   -- ^ Parser with second operation
     -> Parser b
bind p q = Parser $ \s -> concatMap (\(a, s') -> parse (q a) s') $ parse p s

failure :: Parser a
failure = Parser (\s -> [])

option :: Parser a    -- ^ First option parser
       -> Parser a    -- ^ Second option parser
       -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    []  -> parse q s
    result -> result

combine :: Parser a   -- ^ Parser whose operation is first in the result
        -> Parser a   -- ^ Parser whose operation is second in the result
        -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

------------------------------ Parser Monad Instances ------------------------------

instance Monad Parser where
  return = unit
  (>>=)  = bind

-- | Apply a function to the first element of every tuple resulting from the parse operation.
instance Functor Parser where
  fmap f p = p >>= return . f

-- | (`<*>`) The binary associative operation applies the LHS parser operation
-- to the the RHS parser operation and retains only the RHS stream.
instance Applicative Parser where
  pure                          = return
  (Parser op1) <*> (Parser op2) = Parser (\s1 -> [(f a, s3) | (f, s2) <- op1 s1,
                                                              (a, s3) <- op2 s2])

instance Alternative Parser where
  empty = failure
  (<|>) = option

instance MonadPlus Parser where
  mzero = empty
  mplus = combine
