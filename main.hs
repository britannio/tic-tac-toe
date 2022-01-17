import Data.Char ()
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



        
