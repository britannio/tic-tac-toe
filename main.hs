import Data.Char ()
import Data.List ()
import System.IO ()

-- Size of the tic-tac-toe grid
size :: Integer
size = 3

-- a NxN grid of Player values
type Grid = [[Player]]

data Player
  = O -- nought
  | B -- blank
  | X -- cross
  deriving (Eq, Ord, Show)

next :: Player -> Player
next O = X
next B = B
next X = O