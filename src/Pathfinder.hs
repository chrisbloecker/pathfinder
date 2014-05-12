module Main
  where

-------------------------------------------------------------------------------

import System.Environment (getArgs)
import Data.Function      (on)
import Data.List          (minimumBy)
import Data.Vector        (Vector, (!), update, fromList)

-------------------------------------------------------------------------------

type Width     = Int
type Height    = Int
type Position  = (Int, Int)
type Matrix a  = Vector (Vector a)
type Path      = [Position]

data Dimension = Dimension Width Height
data Cell      = Empty | X | S | E                deriving (Eq)
data Board     = Board Dimension (Matrix Cell)

-------------------------------------------------------------------------------

neighbours :: Dimension -> Position -> [Position]
neighbours (Dimension w h) (x,y) = filter valid [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
  where
    valid :: Position -> Bool
    valid (x,y) = and [x >= 0, x < w, y >= 0, y < h]

readBoard :: String -> Board
readBoard str =
  let rows   = lines str
      width  = length . head $ rows
      height = length rows
      board  = fromList . map (fromList . map readCell) $ rows
  in Board (Dimension width height) board
    where
      readCell :: Char -> Cell
      readCell ' ' = Empty
      readCell 'X' = X
      readCell 'S' = S
      readCell 'E' = E
      readCell  x  = error $ "No valid cell value: " ++ (show x)

findCells :: Cell -> Board -> [Position]
findCells c (Board (Dimension w h) cells) = [(x,y) | x <- [0..w-1], y <- [0..h-1], cells `at` (x,y) == c]

at :: Matrix a -> Position -> a
at m (x, y) = m ! y ! x

set :: Matrix a -> Position -> a -> Matrix a
set m (x,y) c = update m (fromList [(y, update (m!y) (fromList [(x, c)]))])

findWay :: Board -> Position -> Position -> [Path]
findWay (Board dim board) start end
  | start == end  = [[end]]
  | otherwise     = map (start :)
                  . concatMap (\start' -> findWay (Board dim board') start' end)
                  $ starts
    where
      board' = set board start X
      starts = filter (\p -> (board' `at` p) `elem` [Empty, E]) $ neighbours dim start

-------------------------------------------------------------------------------

printSolution :: [Path] -> IO ()
printSolution [] = return ()
printSolution ps = mapM_ printPosition . minimumBy (compare `on` length) $ ps
  where
    printPosition :: Position -> IO ()
    printPosition (x,y) = putStrLn $ show x ++ "," ++ show y

printHelp :: IO ()
printHelp = putStrLn "Usage: pathfinder /path/to/board"

main :: IO ()
main = do
  args <- getArgs
  
  case args of
    (    ("-h"):_) -> printHelp
    (("--help"):_) -> printHelp
    
    [filename] -> do
      file <- readFile filename
      let board     = readBoard file
      let start     = head . findCells S $ board
      let ends      = findCells E board
      let solutions = concatMap (findWay board start) ends
      
      printSolution solutions
    
    other -> do
      putStrLn $ "Your arguments are invalid: " ++ show other
      printHelp