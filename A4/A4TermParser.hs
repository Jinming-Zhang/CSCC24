module A4TermParser where

import Control.Applicative

-- More imports as you need.

import A4Term
import ParserLib

-- This can help testing by reading from a file so you can test multi-line input
-- and also have little hassle with \
parseFile :: String -> IO (Maybe Term)
parseFile filename = do
    inp <- readFile filename
    let ans = runParser termParser inp
    return ans

termParser :: Parser Term
termParser = error "TODO"
