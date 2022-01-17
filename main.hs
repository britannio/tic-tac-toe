import Data.Char (isDigit)
import Data.List (transpose)
import System.IO ()

-- Size of the tic-tac-toe grid.
size :: Int
size = 3

empty :: Grid
empty = replicate size (replicate size B)

full :: Grid -> Bool
full = notElem B . concat

data Player
  = O -- nought
  | B -- blank
  | X -- cross
  deriving (Eq, Ord, Show)

-- a NxN grid of Player values.
type Grid = [[Player]]

next :: Player -> Player
next O = X
next B = B
next X = O

-- The nought player starts, then it alternates.
turn :: Grid -> Player
turn g = if os <= xs then O else X
    where
        os = length (filter (==O) ps)
        xs = length (filter (==X) ps)
        ps = concat g

-- True if the player has won. To win the game, the player must occupy three
-- consecutive horizontal, vertical or diagonal positions.
wins :: Player -> Grid -> Bool
wins p g = any line (rows ++ cols ++ dias)
    where
        line = all (== p)
        rows = g
        cols = transpose g
        dias = [diag g, diag (map reverse g)]

-- The players occupying the diagonal (top left to bottom right)
diag :: Grid -> [Player]
diag g = [g !! n !! n | n <- [0..size-1]]

-- Has either player won?
won :: Grid -> Bool
won g = wins O g || wins X g

-- Output a grid to the console
putGrid :: Grid -> IO ()
putGrid =
    putStrLn . unlines . concat . interleave bar . map showRow
    where bar = [replicate ((size * 4) - 1) '-']

showRow :: [Player] -> [String]
showRow = beside . interleave bar . map showPlayer
    where
        beside = foldr1 (zipWith (++))
        bar = replicate 3 "|"

showPlayer :: Player -> [String]
showPlayer O = ["   "," O ", "   "]
showPlayer B = ["   ","   ", "   "]
showPlayer X = ["   "," X ", "   "]

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y:ys) = y : x : interleave x ys

-- A valid move is one between 0 and n-1 in a blank position
validMove :: Grid -> Int -> Bool
validMove g i = 0 <= i && i < size^2 && concat g !! i == B

move :: Grid -> Int -> Player -> Maybe Grid
move g i p
    | validMove g i = Just $ chop size (xs ++ [p] ++ ys)
    | otherwise = Nothing
    -- (concat g) converts the 2d grid into a 1d grid
    where (xs,B:ys) = splitAt i (concat g)

-- Converts a flat list into a 2d list
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
    xs <- getLine
    if xs /= "" && all isDigit xs then
        return (read xs)
    else
        do
          putStrLn "ERROR: Invalid number"
          getNat prompt
    

           
        







