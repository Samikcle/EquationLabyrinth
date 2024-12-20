import Text.CSV
import Data.Maybe
import Data.Char
import Data.List

newtype MazeLine = MazeLine [Int]
newtype Maze = Maze [MazeLine]

processLine :: MazeLine -> String
processLine (MazeLine []) = "" 
processLine (MazeLine (x:xs))
    | x == 0    = " " ++ processLine (MazeLine xs) 
    | x == 1    = "█" ++ processLine (MazeLine xs)
    | otherwise = "?" ++ processLine (MazeLine xs) 

drawMaze :: Maze -> IO ()
drawMaze (Maze []) = return ()
drawMaze (Maze (x:xs)) = putStrLn (processLine x) >> drawMaze (Maze xs)

parseMazeLine :: Record -> Maybe MazeLine
parseMazeLine r =
    if null validInts then Nothing else Just (MazeLine validInts)
  where
    validInts = mapMaybe parseCell r
    parseCell c = case removeSpaces c of
        "0" -> Just 0
        "1" -> Just 1
        _    -> Nothing
    removeSpaces = dropWhileEnd isSpace . dropWhile isSpace

rowsToMaze :: [Record] -> Maybe Maze
rowsToMaze r =
    let mazeLines = mapMaybe parseMazeLine r
    in if null mazeLines then Nothing else Just (Maze mazeLines)

readMazeFromCSV :: FilePath -> IO (Maybe Maze)
readMazeFromCSV filePath = do
    result <- parseCSVFromFile filePath
    case result of
        Left _     -> return Nothing
        Right r -> return $ rowsToMaze r

main :: IO ()
main = do
    putStrLn "Hello, Haskell Test!"
    maze <- readMazeFromCSV "mazebinary.csv"
    case maze of
        Nothing    -> putStrLn "Failed to read maze from CSV file."
        Just m -> do
            putStrLn "Maze successfully loaded!"
            drawMaze m



