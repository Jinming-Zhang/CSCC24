module A3Winning where

import Data.Array

-- You can import other modules/functions here.

data XO = X | O deriving (Eq, Show)

-- A game board is represented by a 2D array.  Indexes are from (0,0)
-- to (2,2).  Each cell is Nothing, Just X, or Just O, meaning blank,
-- has X, or has O.
--
-- So the type is Array (Int, Int) (Maybe XO).
-- The (Int, Int) there means an index is like (0, 0).

-- Blank initial game board.
initBoard :: Array (Int, Int) (Maybe XO)
initBoard = listArray ((0,0), (2,2)) (repeat Nothing)

-- Example board that has a winning move.
albertBoard :: Array (Int, Int) (Maybe XO)
albertBoard = initBoard // [ ((0,1), Just X)
                           , ((1,0), Just X)
                           , ((1,1), Just O)
                           , ((1,2), Just X)
                           ]

-- Print a game board.
printBoard :: Array (Int, Int) (Maybe XO) -> IO ()
printBoard board = do
    putStrLn "---"
    mapM_ (\i -> putStrLn [toChar (board!(i,j)) | j <- [0,1,2]])
          [0,1,2]
    putStrLn "---"
  where
    toChar Nothing = ' '
    toChar (Just X) = 'X'
    toChar (Just O) = 'O'

-- Compute all legal moves for the given board.
-- In the answers, (i, j, X) means "put an X at cell (i,j)" for example.
moves :: Array (Int, Int) (Maybe XO) -> [(Int, Int, XO)]
moves = error "TODO"

-- Compute all blowing moves for the given game board.
-- In the answers, (i, j, X) means "put an X at cell (i,j)" for example.
howToWin :: Array (Int, Int) (Maybe XO) -> [(Int, Int, XO)]
howToWin = error "TODO"
