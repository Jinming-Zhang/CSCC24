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
newtype Feeder a = FeederOf {getFeeder :: [Char] -> Either Exception ([Char], a)}
-- newtype Feeder a = FeederOf ([Char] -> ([Char], Either Exception a))
-- newtype Feeder a = FeederOf ([Char] -> Either Exception a)

-- Run the Feeder program with the string as input source.
-- Return a Left for uncaught exception, a Right for normal return.
runFeeder :: Feeder a -> String -> Either Exception a
runFeeder (FeederOf f) str = case f str of Left exception -> Left exception
                                           Right (fir, sec) -> Right sec

--getB :: Feeder a -> a
--getB FeederOf f = (\x-> x)

testAlbertProg :: String -> Either Exception String
testAlbertProg = runFeeder albertProg

test1 = testAlbertProg "ALBERT"
test2 = testAlbertProg "LABERT"
test3 = testAlbertProg "A"


instance Functor Feeder where
    fmap = liftM
instance Applicative Feeder where
    pure a = return a    -- User can throw an exception manually.
    (<*>) = ap
 
instance Monad Feeder where
    -- return the feeded string
    return x = FeederOf (\str -> Right (str, x))
    -- depends on the result of the givin feeder on the string, either raise an exception or
    (>>=) (FeederOf func) f = (FeederOf (\str -> case func str of Left exception -> Left exception
                                                                  -- unwrape the feeder function out of f, and apply to the result
                                                                  Right (res, a) -> getFeeder (f a) res))

instance MonadGetChar Feeder where
    --get :: f Char
    -- Read and return a character. Throw EOF if no more to read.
    get = FeederOf (\str -> case str of [] -> Left EOF
                                        (char:rst) -> Right (rst,char))
    -- throw :: Exception -> f a
    -- User can throw an exception manually.
    throw exception = FeederOf (\str -> Left exception)

    -- Catch and handle an exception.
    -- catch :: f a -> (Exception -> f a) -> f a
                                                                                   -- apply f and get the result feeder
    catch (FeederOf func) f = FeederOf (\str -> case func str of Left exception -> getFeeder (f exception) str
                                                                 Right (res, a) -> Right (res, a))
                                                                