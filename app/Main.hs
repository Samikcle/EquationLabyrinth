module Main where

type Maze = [[Int]]

processLine :: [Int] -> String
processLine [] = "" 
processLine (x:xs)
    | x == 0    = " " ++ processLine xs 
    | x == 1    = "█" ++ processLine xs 
    | otherwise = "?" ++ processLine xs 

drawMaze :: Maze -> IO ()
drawMaze [] = return ()
drawMaze (x:xs) = do
    putStrLn (processLine x)
    drawMaze xs

main :: IO ()
main = putStrLn "Hello, Haskell Test!"
