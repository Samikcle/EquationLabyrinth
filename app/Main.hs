import Text.CSV
import Data.Maybe
import Data.Char
import Data.List

newtype MazeLine = MazeLine [Int]
newtype Maze = Maze [MazeLine]
data Coord = Coord {
    posX :: Int,
    posY :: Int
}

processLine :: MazeLine -> Coord -> String
processLine (MazeLine []) _ = ""
processLine (MazeLine (x:xs)) coord
    | posX coord == 0 && posY coord == 0 = "o" ++ processLine (MazeLine xs) (coord { posX = posX coord - 1 })
    | x == 0                             = " " ++ processLine (MazeLine xs) (coord { posX = posX coord - 1 })
    | x == 1                             = "█" ++ processLine (MazeLine xs) (coord { posX = posX coord - 1 })
    | otherwise                          = "?" ++ processLine (MazeLine xs) (coord { posX = posX coord - 1 })

drawMaze :: Maze -> Coord -> IO ()
drawMaze (Maze []) _ = return ()
drawMaze (Maze (x:xs)) coord = putStrLn (processLine x coord) >> drawMaze (Maze xs) (coord { posY = posY coord - 1 })

parseMazeLine :: Record -> Maybe MazeLine
parseMazeLine r =
    if null validInts then Nothing else Just (MazeLine validInts)
  where
    validInts = mapMaybe parseCell r
    parseCell c = case removeSpaces c of
        "0" -> Just 0
        "1" -> Just 1
        _   -> Nothing
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
        Nothing -> putStrLn "Failed to read maze from CSV file."
        Just m -> do
            putStrLn "Maze successfully loaded!"
            drawMaze m (Coord 39 39)



