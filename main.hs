import Data.Char (isDigit)
import Data.List (transpose, find)
import Data.Maybe (catMaybes, fromJust, fromMaybe, isJust, mapMaybe)
import Debug.Trace (trace)
import System.IO (BufferMode (NoBuffering), hSetBuffering, stdout)
import Text.Printf (printf)

-- Size of the tic-tac-toe grid.
size :: Int
size = 3

-- Max depth of the generated game tree
depth :: Int
depth = 9

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  let tree = gametree empty O
  play tree O 

cls :: IO ()
cls = putStr "\ESC[2J"

type Pos = (Int, Int)

goto :: Pos -> IO ()
goto (x, y) = putStr $ printf "\ESC[%d;%dH" y x

-- Play against the computer
play :: Tree Grid -> Player -> IO ()
play (Node g gs) p = do
  cls
  goto (1, 1)
  putGrid g
  play' (Node g gs) p

play' :: Tree Grid -> Player -> IO ()
play' x@(Node g gs) p
  | wins O g = putStrLn $ printf "Player %s wins!\n" (showPlayer O)
  | wins X g = putStrLn $ printf "Player %s wins!\n" (showPlayer X)
  | full g = putStrLn "It's a draw!\n"
  | p == O = do
    i <- getNat (prompt p)
    case move g i p of
      Nothing -> do
        putStrLn "ERROR: Invalid move"
        play' x p
      Just g' -> play (nodeTree g' gs) (next p)
  | p == X = do
    putStrLn $ printf "Player %s is thinking" (showPlayer X)
    (play $! bestmove x p) (next p)
  | otherwise = undefined
  
nodeTree :: Grid -> [Tree Grid] -> Tree Grid
nodeTree g' gs = fromJust (find (\(Node g'' _) -> g' == g'') gs)

-- Play against another person
tictactoe :: IO ()
tictactoe = run empty O

run :: Grid -> Player -> IO ()
run g p = do
  cls
  goto (1, 1)
  putGrid g
  run' g p

run' :: Grid -> Player -> IO ()
run' g p
  | wins O g = putStrLn "Player O wins!\n"
  | wins X g = putStrLn "Player X wins!\n"
  | full g = putStrLn "It's a draw!\n"
  | otherwise =
    do
      i <- getNat (prompt p)
      case move g i p of
        Nothing -> do
          putStrLn "ERROR: Invalid move"
          run' g p
        Just g' -> run g' (next p)

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
    os = length (filter (== O) ps)
    xs = length (filter (== X) ps)
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
diag g = [g !! n !! n | n <- [0 .. size -1]]

-- Has either player won?
won :: Grid -> Bool
won g = wins O g || wins X g

-- Output a grid to the console
putGrid :: Grid -> IO ()
putGrid = putStrLn . unlines . concat . interleave bar . map showRow
  where
    bar = [swap 4 '┼' $ replicate ((size * 4) - 1) '─']

swap :: Show a => Int -> a -> [a] -> [a]
swap n x = zipWith (\i x' -> if i `mod` n == 0 then x else x') [1 ..]

showRow :: [Player] -> [String]
showRow = (: []) . concat . interleave "│" . map (\x -> " " ++ showPlayer x ++ " ")

showPlayer :: Player -> String
showPlayer O = "◯"
showPlayer B = " "
showPlayer X = "×"

interleave :: a -> [a] -> [a]
interleave x [] = []
interleave x [y] = [y]
interleave x (y : ys) = y : x : interleave x ys

-- A valid move is one between 0 and n-1 in a blank position
validMove :: Grid -> Int -> Bool
validMove g i = 0 <= i && i < size ^ 2 && concat g !! i == B

move :: Grid -> Int -> Player -> Maybe Grid
move g i p
  | validMove g i = Just $ chop size (xs ++ [p] ++ ys)
  | otherwise = Nothing
  where
    -- (concat g) converts the 2d grid into a 1d grid
    (xs, B : ys) = splitAt i (concat g)

-- Converts a flat list into a 2d list
chop :: Int -> [a] -> [[a]]
chop n [] = []
chop n xs = take n xs : chop n (drop n xs)

getNat :: String -> IO Int
getNat prompt = do
  putStr prompt
  xs <- getLine
  if xs /= "" && all isDigit xs
    then return (read xs)
    else do
      putStrLn "ERROR: Invalid number"
      getNat prompt

prompt :: Player -> String
prompt p = printf "Player %s, enter your move: " (showPlayer p)

data Tree a = Node a [Tree a]
  deriving (Show)

countStates :: Tree Grid -> Int
countStates (Node _ gs) = 1 + sum (map countStates gs)

-- All possible grids that could play out
gametree :: Grid -> Player -> Tree Grid
gametree g p = Node g (withNoRotations [gametree g' (next p) | g' <- moves g p])

withNoRotations :: [Tree Grid] -> [Tree Grid]
withNoRotations [] = []
withNoRotations (g@(Node g'' _) : gs) = g : withNoRotations (filter notRotation gs)
  where
    notRotation :: Tree Grid -> Bool
    notRotation (Node g' _) = g' /= rotate 1 g'' 
                              && g' /= rotate 2 g'' 
                              && g' /= rotate 3 g''

rotate :: Int -> [[a]] -> [[a]]
rotate n xs
  | n == 0 || n == 4 = xs
  | n < 0 = rotate (4 + n) xs
  | otherwise = rotate (n-1) $ map reverse $ transpose xs

moves :: Grid -> Player -> [Grid]
moves g p
  -- A player has won or all spaces are filled
  | won g || full g = []
  | otherwise = mapMaybe (\i -> move g i p) [0 .. ((size ^ 2) - 1)]

-- Limit a tree to a given depth
prune :: Int -> Tree a -> Tree a
prune 0 (Node x _) = Node x []
prune n (Node x ts) = Node x $ map (prune (n -1)) ts

-- Label a game tree
minimax :: Tree Grid -> Tree (Grid, Player)
minimax (Node g [])
  | wins O g = Node (g, O) []
  | wins X g = Node (g, X) []
  | otherwise = Node (g, B) []
minimax (Node g ts)
  | turn g == O = Node (g, minimum ps) ts'
  | turn g == X = Node (g, maximum ps) ts'
  | otherwise = undefined
  where
    ts' = map minimax ts
    ps = [p | Node (_, p) _ <- ts']

bestmove :: Tree Grid -> Player -> Tree Grid
bestmove (Node g gs) p = nodeTree (head [g' | Node (g', p') _ <- ts, p' == best]) gs
  where
    -- tree = prune depth (gametree g p)
    -- tree = gametree g p
    Node (_, best) ts = minimax (Node g gs)
