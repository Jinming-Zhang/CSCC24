module A3Winning where

import Data.Array

-- You can import other modules/functions here.
import Data.Maybe
import Data.List
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
-- call the helper function with this board, its list of indeces and start from 0
moves board 
    | winning board = []
    | drawing board = []
    | otherwise = helpMoves board list 0
    where list = indices board

-- takes a gameboard, a list of its indices and a starting index
-- return a list of all possible moves of the gameboard
helpMoves :: Array (Int, Int) (Maybe XO) -> [(Int, Int)] -> Int -> [(Int, Int, XO)]
helpMoves board list i
    | i == bound = []
    | isNothing symbol =  [(x, y, O), (x, y, X)] ++ helpMoves board list (i+1)
    | otherwise = helpMoves board list (i+1)
    where bound = length list
          x = fst (list !! i)
          y = snd (list !! i)
          symbol = board ! (x, y)

-- Compute all blowing moves for the given game board.
-- In the answers, (i, j, X) means "put an X at cell (i,j)" for example.
howToWin :: Array (Int, Int) (Maybe XO) -> [(Int, Int, XO)]
howToWin board = [mv | mv <- allMove, isBlowingMove board mv]
    where allMove = moves board

isBlowingMove :: Array (Int, Int) (Maybe XO) -> (Int, Int, XO) ->Bool
isBlowingMove board move
    | winning (next board move) = True
    | otherwise = let newBoard = next board move
                      newMoves = moves newBoard
                  in  hasBlowingMove newBoard newMoves-- 0

hasBlowingMove :: Array (Int, Int) (Maybe XO) -> [(Int, Int, XO)] -> Bool--Int -> Bool
hasBlowingMove oppBoard oppMoves --removing
    | [mv | mv <- oppMoves, winning (next oppBoard mv)] /= [] = False
    | otherwise = not (null [hasBlowingMove myBoard (moves myBoard) | myBoard <- myBoards])
    where myBoards = [ next oppBoard mv | mv <- oppMoves]

-- make a next move regarding to givin player and position and gameboard
next :: Array (Int, Int) (Maybe XO) -> (Int, Int, XO) -> Array (Int, Int) (Maybe XO)
next board (x, y, X) = board // [ ((x, y), Just X)]
next board (x, y, O) = board // [ ((x, y), Just O)]

drawing :: Array (Int, Int) (Maybe XO) -> Bool
drawing board = (not (winning board)) && isFull
    where isFull = not (elem Nothing (elems board))

-- Givin a symbol at certain position of the board
-- check if that symbol is winning in the current state
winning :: Array (Int, Int) (Maybe XO) -> Bool
winning board = helpWinning board (indices board)












helpWinning :: Array (Int, Int) (Maybe XO) -> [(Int, Int)] -> Bool
helpWinning board [] = False
helpWinning board (hd: tl) = (isWinningAt board hd) || (helpWinning board tl)

-- Check if the given position of the givin board is givin symbol
isPlayer :: Array (Int, Int) (Maybe XO) -> (Maybe XO) -> (Int, Int) -> Bool
-- if position out of bound, then false
isPlayer board player position
    | not (elem position (indices board)) = False
    | isNothing player = False
    | (board ! position) == player = True
    | otherwise = False

isWinningAt :: Array (Int, Int) (Maybe XO) -> (Int, Int) -> Bool
isWinningAt board (x, y)
    | (isPlayer board currentPlayer top) && (isPlayer board currentPlayer buttom) = True
    | (isPlayer board currentPlayer left) && (isPlayer board currentPlayer right) = True
    | (isPlayer board currentPlayer topleft) && (isPlayer board currentPlayer buttomright) = True
    | (isPlayer board currentPlayer topright) && (isPlayer board currentPlayer buttomleft) = True
    | otherwise = False
    where currentPlayer = board ! (x, y)
          top =        (x - 1, y)
          buttom =     (x + 1, y)
          left =       (x, y -1)
          right =      (x, y + 1)
          topleft =    (x - 1, y - 1)
          topright =   (x - 1, y + 1)
          buttomleft = (x + 1, y - 1)
          buttomright =(x + 1, y + 1)