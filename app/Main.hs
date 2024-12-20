import Text.CSV

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

toMazeLine :: Record -> Maybe MazeLine
toMazeLine record = MazeLine <$> traverse toInt record
  where
    toInt str = case reads str :: [(Int, String)] of
                  [(n, "")] | n == 0 || n == 1 -> Just n
                  _                           -> Nothing

csvToMaze :: CSV -> Maybe Maze
csvToMaze x = Maze <$> traverse toMazeLine x

readMazeFromCSV :: FilePath -> IO (Maybe Maze)
readMazeFromCSV filePath = do
    result <- parseCSVFromFile filePath
    return $ case result of
        Left _    -> Nothing 
        Right x -> csvToMaze x

main :: IO ()
main = do
    putStrLn "Hello, Haskell Test!"
    maze <- readMazeFromCSV "mazebinary.csv"
    case maze of
        Nothing   -> putStrLn "Failed to read maze from CSV file."
        Just m    -> do
            putStrLn "Maze successfully loaded!"
            drawMaze m



