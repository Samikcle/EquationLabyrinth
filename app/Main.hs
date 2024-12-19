module Main where

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

main :: IO ()
main = putStrLn "Hello, Haskell Test!"
