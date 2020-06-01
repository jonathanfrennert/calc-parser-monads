{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}

module NanoParsec where

import Data.Char
import Control.Monad
import Control.Applicative

-- | The Parser takes a character stream and yields a parse tree.
newtype Parser a = Parser { parse :: String         -- ^ Input for parser
                                  -> [(a, String)]
                          }

-- | Running the parser on a character stream yields the parse tree, or if the
-- stream could not be consumed, an error is thrown.
runParser :: Parser a   -- ^ Parser to run
          -> String     -- ^ Input for parser
          -> a
runParser p s =
  case parse p s of
    [(res, [])] -> res
    [(_, left)] -> error "Parser did not consume entire stream."
    _           -> error "Parser Error."

-- | Extract a single character from the parser stream and return a tuple
-- containing itself and the rest of the stream.
item :: Parser Char   -- ^ Parser to extract the character from
item = Parser $ \s ->
  case s of
    []     -> []
    (c:cs) -> [(c,cs)]
