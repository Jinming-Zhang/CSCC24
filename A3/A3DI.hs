module A3DI where

import Control.Monad (liftM, ap)

-- You can import more modules/functions here.

-- The API for reading a character with possible failure.
class Monad f => MonadGetChar f where
    -- Read and return a character. Throw EOF if no more to read.
    get :: f Char
    -- User can throw an exception manually.
    throw :: Exception -> f a
    -- Catch and handle an exception.
    catch :: f a -> (Exception -> f a) -> f a

-- Possible exceptions.
data Exception = EOF | Other String deriving (Eq, Show)

-- Example usage of the API.
albertProg :: MonadGetChar f => f String
albertProg = do
    c1 <- get
    c2 <- get
    if [c1, c2] == "AL"
        then return "good header"
        else return "wrong header"
  `catch`
      \e -> throw (Other "input too short")

-- Pick one to uncomment.
-- newtype Feeder a = FeederOf ([Char] -> Either ([Char], Exception a))
-- newtype Feeder a = FeederOf ([Char] -> ([Char], Either Exception a))
-- newtype Feeder a = FeederOf ([Char] -> Either Exception a)

-- Run the Feeder program with the string as input source.
-- Return a Left for uncaught exception, a Right for normal return.
runFeeder :: Feeder a -> String -> Either Exception a
runFeeder = error "TODO"

testAlbertProg :: String -> Either Exception String
testAlbertProg = runFeeder albertProg

test1 = testAlbertProg "ALBERT"
test2 = testAlbertProg "LABERT"
test3 = testAlbertProg "A"

instance Functor Feeder where
    fmap = liftM
instance Applicative Feeder where
    pure a = return a
    (<*>) = ap

instance Monad Feeder where
-- TODO

instance MonadGetChar Feeder where
-- TODO
