{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

------------------------------ Parser ------------------------------

-- | The Parser takes a character stream and yields a parse tree.
newtype Parser a = Parser { parse :: String           -- ^ Input for parser
                                  -> [(a, String)]
                          }

-- | Running the parser on a character stream yields the parse tree, or if the
-- stream could not be consumed, an error is thrown.
runParser :: Parser a   -- ^ Parser to run
          -> String     -- ^ Input for parser
          -> a
runParser p s =
  case p `parse` s of
    [(result, [])]  -> result
    [(_, leftover)] -> error "Parser did not consume entire stream."
    _               -> error "Parser Error."

-- | Extract a single character from the parser stream and return a tuple
-- containing itself and the rest of the stream.
item :: Parser Char
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c,cs)]

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

-- | Create an empty list.
failure :: Parser a
failure = Parser (\s -> [])

-- | Choose between two parser operations, switching to the second operation if
-- the first operation results in a 'failure'.
option :: Parser a    -- ^ Parser with the first operation
       -> Parser a    -- ^ Parser with the second operation
       -> Parser a
option  p q = Parser $ \s ->
  case parse p s of
    []  -> parse q s
    res -> res

-- | Apply two parser operations over the same stream and concatenate the result.
combine :: Parser a   -- ^ Parser whose operation is first in the result
        -> Parser a   -- ^ Parser whose operation is second in the result
        -> Parser a
combine p q = Parser (\s -> parse p s ++ parse q s)

-- |
-- * The identity operation is 'unit'.
-- * The binary associative binary operation is 'bind'.
instance Monad Parser where
  return = unit
  (>>=)  = bind

-- | Apply a function to the first element of every tuple resulting from the parse operation.
instance Functor Parser where
  fmap f p = p >>= return . f

-- |
-- * The identity operation is 'return'.
-- * The binary associative operation applies the LHS parser operation to the
-- the RHS parser operation and retains only the RHS stream.
instance Applicative Parser where
  pure                          = return
  (Parser op1) <*> (Parser op2) = Parser (\s1 -> [(f a, s3) | (f, s2) <- op1 s1,
                                                              (a, s3) <- op2 s2])

-- |
-- * The identity operation is 'failure'.
-- * The binary associative operation is 'option'.
instance Alternative Parser where
  empty = failure
  (<|>) = option

-- |
-- * The identity operation is 'empty'
-- * The binary associative operation is 'combine'
instance MonadPlus Parser where
  mzero = empty
  mplus = combine
