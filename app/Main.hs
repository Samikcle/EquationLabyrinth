import Text.CSV
import Data.Maybe
import Data.Char
import Data.List
import System.Console.ANSI
import System.Random

newtype MazeLine = MazeLine [Int]
newtype Maze = Maze [MazeLine]
data Coord = Coord {
    posX :: Int,
    posY :: Int
} deriving (Show)
data Direction = UpM | DownM | LeftM | RightM deriving (Eq,Read)
data MathEqs = PlusN | MinusN | NMinus | TimesN | DivideN | NDivide | ModN | NMod | SquareN | RootN deriving (Enum, Bounded, Show)

opposite :: Direction -> Direction
opposite UpM = DownM
opposite DownM = UpM
opposite LeftM = RightM
opposite RightM = LeftM

processLine :: MazeLine -> Coord -> String
processLine (MazeLine []) _ = ""
processLine (MazeLine (x:xs)) coord
    | posX coord == 0 && posY coord == 0 = setSGRCode [SetColor Foreground Vivid Red] ++ "■" ++ setSGRCode [Reset] ++ processLine (MazeLine xs) (coord { posX = posX coord - 1 })
    | x == 0                             = "·" ++ processLine (MazeLine xs) (coord { posX = posX coord - 1 })
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

getCell :: Maze -> Coord -> Maybe Int
getCell (Maze ls) (Coord x y)
  | y < 0 || y >= length ls = Nothing
  | x < 0 || x >= length l = Nothing
  | otherwise = Just (l !! x)
  where
    MazeLine l = ls !! y

calculateDistance :: Maze -> Coord -> Direction -> Int
calculateDistance maze (Coord x y) direction =
  case getCell maze (Coord x y) of
    Just 1 -> 0
    Just 0 -> case direction of
      UpM    -> 1 + calculateDistance maze (Coord x (y - 1)) direction
      DownM  -> 1 + calculateDistance maze (Coord x (y + 1)) direction
      LeftM  -> 1 + calculateDistance maze (Coord (x - 1) y) direction
      RightM -> 1 + calculateDistance maze (Coord (x + 1) y) direction
    _ -> 0

distanceToWall :: Maze -> Coord -> Direction -> Int
distanceToWall maze coord direction = calculateDistance maze coord direction -1

move :: Coord -> Direction -> Int-> Coord
move (Coord x y) d i
    | d == UpM      = Coord x (y - i)
    | d == DownM    = Coord x (y + i)
    | d == LeftM    = Coord (x - i) y
    | d == RightM   = Coord (x + i) y
    | otherwise     = Coord x y

run :: Maze -> Coord -> IO()
run m player = do
    mdir <- getLine
    mdist <- getLine
    let player2 = checkMovement m player (read mdir::Direction) (read mdist::Int)
    gen <- newStdGen
    gen2 <- newStdGen
    drawMaze m player2
    print $ distanceToWall m player2 UpM
    print $ distanceToWall m player2 DownM
    print $ distanceToWall m player2 LeftM
    print $ distanceToWall m player2 RightM
    print $ player2
    printEq (randomMathEq gen)
    printEq (randomMathEq gen2)
    run m player2

checkMovement :: Maze -> Coord -> Direction -> Int -> Coord
checkMovement m c d x
    | distanceToWall m c d == 0 && distanceToWall m c (opposite d) == 0 = c
    | distanceToWall m c d == 0                                         = checkMovement m c (opposite d) x
    | distanceToWall m c d >= x                                         = move c d x 
    | otherwise                                                         = checkMovement m (move c d (distanceToWall m c d)) (opposite d) (x-distanceToWall m c d)

randomMathEq :: StdGen -> MathEqs
randomMathEq gen = toEnum randomIndex
  where
    (randomIndex, _) = randomR (fromEnum (minBound :: MathEqs), fromEnum (maxBound :: MathEqs)) gen

randomInterger :: StdGen -> Int -> Int -> Int
randomInterger gen minI maxI = randomValue
  where
    (randomValue, _) = randomR (minI, maxI) gen

printEq :: MathEqs -> IO ()
printEq x = putStrLn $ show x

main :: IO ()
main = do
    putStrLn "Hello, Haskell Test!"
    maze <- readMazeFromCSV "mazebinary.csv"
    let player = (Coord 1 1)
    
    case maze of
        Nothing -> putStrLn "Failed to read maze from CSV file."
        Just m -> do
            putStrLn "Maze successfully loaded!"
            drawMaze m player
            print $ distanceToWall m player UpM
            print $ distanceToWall m player DownM
            print $ distanceToWall m player LeftM
            print $ distanceToWall m player RightM
            print $ player
            run m player





