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

-- | The parser monad bind operation. Make a composition of two parser operations.
bind :: Parser a          -- ^ Parser with the first operation
     -> (a -> Parser b)   -- ^ Parser with second operation
     -> Parser b
bind fstP sndP = Parser $ \s -> concatMap (\(a, s') -> parse (sndP a) s') $ parse fstP s

-- | The parser monad identity operation. Inject a single pure value as the result.
unit :: a           -- ^ Pure value
     -> Parser a
unit a = Parser (\s -> [(a,s)])

-- | The functor parser applies a function to the first element of every tuple
-- resulting from its parse operation.
instance Functor Parser where
  fmap f (Parser cs) = Parser (\s -> [(f a, b) | (a, b) <- cs s])

-- | TODO
instance Applicative Parser where
  pure                          = return
  (Parser cs1) <*> (Parser cs2) = Parser (\s1 -> [(f a, s3) | (f, s2) <- cs1 s1,
                                                              (a, s3) <- cs2 s2])

-- | TODO
instance Monad Parser where
  return = unit
  (>>=)  = bind
