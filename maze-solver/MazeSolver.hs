module MazeSolver where

import Control.Concurrent
import Data.List
import System.Environment
import System.Exit

type Path = [Pos]
type Pos  = (Row, Col)
type Row  = Int
type Col  = Int
type Continuation = Either Pos Bool
type MaybePath = (Row, Col, Direction, Direction, Direction, Direction)
type Direction = Bool
type Maze = [String]

getMaze :: FilePath -> IO (Maze)
getMaze file = do
  rows <- readFile file
  let maze = splitOn '\n' rows
  if last maze == ""
    then return (removeLast maze)
  else return (maze)

splitOn :: Char -> String -> Maze
splitOn delim str =
  splitHelper delim str "" where 
    splitHelper = (\del strin temp -> 
      if length strin == 0
        then [temp]
      else if head strin == delim 
        then [temp] ++ splitHelper del (tail strin) ""
      else splitHelper del (tail strin) (temp++[head strin]))

isValidSpace :: Pos -> Maze -> [MaybePath] -> [MaybePath] -> Bool
isValidSpace (x, y) maze path wrongPath = 
  if x < 0 || x > rows - 1 || y < 0 || y > columns - 1
    then False
  else if isPosInMaybePath (x, y) path || isPosInMaybePath (x, y) wrongPath
    then False
  else if (maze !! x) !! y == ' ' || (maze !! x) !! y == 'F'
    then True
  else False where
    rows = length maze
    columns = length $ head maze

isFinish :: Pos -> Maze -> Bool
isFinish (x, y) maze = 
  if x < 0 || x > rows - 1 || y < 0 || y > columns - 1
    then False
  else if (maze !! x) !! y == 'F'
    then True
  else False where
    rows = length maze
    columns = length $ head maze

getStart :: Maze -> MaybePath
getStart maze = 
  grabStart maze (0, 0) where
    grabStart [] _ = (0, 0, False, False, False, False)
    grabStart (x:xs) (a, b) = if elemIndices 'S' x == []
                                then grabStart xs (a+1, b)
                              else (a, head $ elemIndices 'S' x, False, False, False, False)

isPosInMaybePath :: Pos -> [MaybePath] -> Bool
isPosInMaybePath pos path = (elem pos $ map (\(x, y, _, _, _, _) -> (x, y)) path)

-- The logic essentially makes the pathfinder follow the left wall around the maze
-- If it hits a deadend, it goes backwards and can't go back to that position again
-- If it ends up going all the way back to the start, that means there is no valid path
movePosition :: MaybePath -> Maze -> [MaybePath] -> [MaybePath]-> Maybe (Path)
movePosition (x, y, left, right, up, down) maze currentPath wrongPath =
  if isFinish (x, y) maze
    then (Just (map (\(x, y, _, _, _, _) -> (x, y)) (currentPath ++ [(x, y, left, right, up, down)])))
  else if isValidSpace (x, y-1) maze currentPath wrongPath && not left
    then movePosition (x, y-1, False, True, False, False) maze (currentPath ++ [(x, y, True, right, up, down)]) wrongPath
  else if isValidSpace (x+1, y) maze currentPath wrongPath && not down
    then movePosition (x+1, y, False, False, True, False) maze (currentPath ++ [(x, y, True, right, up, True)]) wrongPath
  else if isValidSpace (x, y+1) maze currentPath wrongPath && not right
    then movePosition (x, y+1, True, False, False, False) maze (currentPath ++ [(x, y, True, True, up, True)]) wrongPath
  else if isValidSpace (x-1, y) maze currentPath wrongPath && not up
    then movePosition (x-1, y, False, False, False, True) maze (currentPath ++ [(x, y, True, True, True, True)]) wrongPath
  else if length currentPath == 0
    then Nothing
  else movePosition (last currentPath) maze (removeLast currentPath) (wrongPath ++ [(x, y, left, right, up, down)])

removeLast :: [a] -> [a]
removeLast lst = reverse $ tail $ reverse lst

addPathStep :: Pos -> Maze -> Maze
addPathStep (x, y) maze = 
  (fst splitMaze) ++ [((fst splitMazeRow) ++ "." ++ 
  if length (snd splitMazeRow) > 0 
    then (tail (snd splitMazeRow)) 
  else "")] ++ 
  if length (snd splitMaze) > 0 
    then (tail (snd splitMaze)) 
  else []  where
  splitMaze = splitAt x maze
  splitMazeRow = splitAt y (maze !! x)

showMaze :: Maze -> IO ()
showMaze [] = do
  return ()
showMaze (x:xs) = do
  putStrLn x
  showMaze xs

animatePath :: Maybe (Path) -> Maze -> IO ()
animatePath Nothing _ = do
  putStrLn "Nothing"
  return ()
animatePath (Just []) _ = do
  putStrLn "Congrats! You made it to the end :)"
  return ()
animatePath (Just (x:xs)) maze = do
  let newMaze = addPathStep x maze
  showMaze newMaze
  threadDelay (100000*4)
  animatePath (Just xs) newMaze

main :: IO ()
main = do
  args <- getArgs
  maze <- getMaze (head args)
  let animate = length args == 2 && (head $ tail args) == "animate"
  let path = movePosition (getStart maze) maze [] []
  if animate
    then animatePath path maze
  else putStrLn $ show path
  return ()
