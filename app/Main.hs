import Text.CSV
import Data.Maybe
import Data.Char
import Data.List
import System.Console.ANSI
import System.Random
import Control.Monad
import Text.Read
import System.IO


newtype MazeLine = MazeLine [Int]
newtype Maze = Maze [MazeLine]
data Coord = Coord {
    posX :: Int,
    posY :: Int
} deriving (Show,Eq)
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
parseMazeLine row =
    if null validInts then Nothing else Just (MazeLine validInts)
  where
    validInts = mapMaybe parseCell row
    parseCell cell = case removeSpaces cell of
        "0" -> Just 0
        "1" -> Just 1
        _   -> Nothing
    removeSpaces = dropWhileEnd isSpace . dropWhile isSpace

rowsToMaze :: [Record] -> Maybe Maze
rowsToMaze file =
    let mazeLines = mapMaybe parseMazeLine file
    in if null mazeLines then Nothing else Just (Maze mazeLines)

readMazeFromCSV :: FilePath -> IO (Maybe Maze)
readMazeFromCSV filePath = do
    result <- parseCSVFromFile filePath
    case result of
        Left _     -> return Nothing
        Right file -> return $ rowsToMaze file

getCell :: Maze -> Coord -> Maybe Int
getCell (Maze mazelines) (Coord x y)
  | y < 0 || y >= length mazelines = Nothing
  | x < 0 || x >= length mazeline = Nothing
  | otherwise = Just (mazeline !! x)
  where
    MazeLine mazeline = mazelines !! y

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
move (Coord x y) direction distance
    | direction == UpM      = Coord x (y - distance)
    | direction == DownM    = Coord x (y + distance)
    | direction == LeftM    = Coord (x - distance) y
    | direction == RightM   = Coord (x + distance) y
    | otherwise             = Coord x y

run :: Maze -> Coord -> Int -> IO ()
run maze player turn =
  if player == getExit maze then do
    clearScreen
    drawMaze maze player
    putStrLn "Congratulation for beating the maze!"
    putStrLn $ "You took " ++ show turn ++ " turn to beat the maze"
    putStrLn "Press Enter to return to menu"
    continue <- getLine
    menu
  else do
    clearScreen
    (equation, eqNum) <- chooseMathEq maze player
    nValue <- obtainNValue maze player equation eqNum
    let distance = calculateMovement equation eqNum nValue
    intDirection <- getDirection maze player equation eqNum nValue distance
    let direction = intToDir intDirection
    let player2 = checkMovement maze player direction distance
    run maze player2 (turn+1)

getExit :: Maze -> Coord
getExit (Maze mazeLines) =
    let
        height = length mazeLines
        width = case mazeLines of
            [] -> 0
            (MazeLine line : _) -> length line
    in Coord (width - 1) (height - 2)

checkMovement :: Maze -> Coord -> Direction -> Int -> Coord
checkMovement maze coord direction distance
    | distance < 0                                                                = checkMovement maze coord (opposite direction) (-distance)
    | distanceToWall maze coord direction == 0 && distanceToWall maze coord (opposite direction) == 0 = coord
    | distanceToWall maze coord direction == 0                                    = checkMovement maze coord (opposite direction) distance
    | distanceToWall maze coord direction >= distance                             = move coord direction distance
    | otherwise                                                                   = checkMovement maze (move coord direction (distanceToWall maze coord direction)) (opposite direction) (distance - distanceToWall maze coord direction)


randomMathEqs :: StdGen -> [MathEqs]
randomMathEqs gen = map toEnum randomIndices
  where
    randomIndices = take 4 $ randomRs (fromEnum (minBound :: MathEqs), fromEnum (maxBound :: MathEqs)) gen

randomInteger :: StdGen -> Int -> Int -> Int
randomInteger gen minInt maxInt = randomValue
  where
    (randomValue, _) = randomR (minInt, maxInt) gen

genNumFromEq :: MathEqs -> IO Int
genNumFromEq equation = do
  gen <- newStdGen
  case equation of
    PlusN   -> let x = randomInteger gen 1 10 in return x
    MinusN  -> let x = randomInteger gen 1 10 in return x
    NMinus  -> let x = randomInteger gen 1 25 in return x
    TimesN  -> let x = randomInteger gen 1 5 in return x
    DivideN -> let x = randomInteger gen 1 10 in return x
    NDivide -> let x = randomInteger gen 10 50 in return x
    ModN    -> let x = randomInteger gen 1 50 in return x
    NMod    -> let x = randomInteger gen 1 25 in return x
    SquareN -> return 0
    RootN   -> return 0

chooseMathEq :: Maze -> Coord -> IO (MathEqs, Int)
chooseMathEq maze coord = do
  clearScreen
  drawMaze maze coord
  gen <- newStdGen
  let mathEqs = randomMathEqs gen
  eqInts <- mapM genNumFromEq mathEqs
  let eqStrings = zipWith (\eq x -> stringSelectedEq eq x "n") mathEqs eqInts
  printDistanceToWall maze coord
  putStrLn "Equations:"

  forM_ (zip [1..] eqStrings) $ \(i, str) ->
    putStrLn $ show i ++ ". " ++ str
  putStrLn "Select an option (1-4):"
  selection <- getInput 1 4

  let selectedEq = mathEqs !! (selection - 1)
  let x = eqInts !! (selection - 1)

  return (selectedEq, x)

getInput :: Int -> Int -> IO Int
getInput minInt maxInt = do
  input <- getLine
  case readMaybe (removeSpaces input) :: Maybe Int of
      Just n | n >= minInt && n <= maxInt-> return n
      _ -> do
            putStrLn $ "Invalid input. Please enter a number between " ++ show minInt ++ " to " ++ show maxInt ++ "."
            putStrLn "Enter input again:"
            getInput minInt maxInt
  where
    removeSpaces = dropWhileEnd isSpace . dropWhile isSpace

stringSelectedEq :: MathEqs -> Int -> String -> String
stringSelectedEq PlusN eqNumber input  = input ++ " + " ++ show eqNumber
stringSelectedEq MinusN eqNumber input = input ++ " - " ++ show eqNumber
stringSelectedEq NMinus eqNumber input = show eqNumber ++ " - " ++ input
stringSelectedEq TimesN eqNumber input = input ++ " * " ++ show eqNumber
stringSelectedEq DivideN eqNumber input = input ++ " ÷ " ++ show eqNumber
stringSelectedEq NDivide eqNumber input = show eqNumber ++ " ÷ " ++ input
stringSelectedEq ModN eqNumber input   = input ++ " mod(%) " ++ show eqNumber
stringSelectedEq NMod eqNumber input   = show eqNumber ++ " mod(%) " ++ input
stringSelectedEq SquareN _ input = input ++ "²"
stringSelectedEq RootN _ input  = "√" ++ input

intToDir :: Int -> Direction
intToDir input
    | input == 1 = UpM
    | input == 2 = DownM
    | input == 3 = LeftM
    | otherwise = RightM

calculateMovement :: MathEqs -> Int -> Int -> Int
calculateMovement PlusN x y   = y + x
calculateMovement MinusN x y  = y - x
calculateMovement NMinus x y  = x - y
calculateMovement TimesN x y  = y * x
calculateMovement DivideN x y = div y x
calculateMovement NDivide x y = div x y
calculateMovement ModN x y    = mod y x
calculateMovement NMod x y    = mod x y
calculateMovement SquareN _ y = y^2
calculateMovement RootN _ y   = round (sqrt (fromIntegral y))

obtainNValue :: Maze -> Coord -> MathEqs -> Int -> IO Int
obtainNValue maze coord equation eqNumber = do
  clearScreen
  drawMaze maze coord
  printDistanceToWall maze coord
  putStrLn "Selected Equation:"
  putStrLn $ stringSelectedEq equation eqNumber "n"
  putStrLn "Enter a value for n:"
  getInput 0 100

getDirection :: Maze -> Coord -> MathEqs -> Int -> Int -> Int -> IO Int
getDirection maze coord equation eqNumber input answer = do
  clearScreen
  drawMaze maze coord
  printDistanceToWall maze coord
  putStrLn "Final Equation:"
  putStrLn $ stringSelectedEq equation eqNumber (show input) ++ " = " ++ show answer
  putStrLn $ "Select a direction to move " ++ show answer ++ " spaces in:"
  putStrLn "1. Up"
  putStrLn "2. Down"
  putStrLn "3. Left"
  putStrLn "4. Right"
  putStrLn "Select an option (1-4):"
  getInput 1 4

printDistanceToWall :: Maze -> Coord -> IO()
printDistanceToWall maze coord = putStrLn $ "Current distance to each wall: Up: " ++ show (distanceToWall maze coord UpM) ++ " Down: " ++ show (distanceToWall maze coord DownM) ++ " Left: " ++ show (distanceToWall maze coord LeftM) ++ " Right: " ++ show (distanceToWall maze coord RightM)

menu :: IO()
menu = do
  clearScreen
  putStrLn "Welcome to "
  putStrLn " ___  __            ___    __"
  putStrLn "|__  /  \\ |  |  /\\   |  | /  \\ |\\ |"
  putStrLn "|___ \\__X \\__/ /~~\\  |  | \\__/ | \\|"
  putStrLn "           __       __         ___     "
  putStrLn "|     /\\  |__) \\ / |__) | |\\ |  |  |__| "
  putStrLn "|___ /~~\\ |__)  |  |  \\ | | \\|  |  |  |"
  putStrLn ""
  putStrLn "1. Play"
  putStrLn "2. How to play"
  putStrLn "3. Quit"
  input <- getInput 1 3
  case input of 
    1 -> selectStage
    2 -> howToPlay
    3 -> putStrLn "Goodbye!"

selectStage :: IO()
selectStage = do
  clearScreen
  putStrLn "Levels: "
  putStrLn " 1. Level 1"
  putStrLn " 2. Level 2"
  putStrLn " 3. Level 3"
  putStrLn " 4. Level 4"
  putStrLn " 5. Level 5"
  putStrLn " 6. Level 6"
  putStrLn " 7. Level 7"
  putStrLn " 8. Level 8"
  putStrLn " 9. Level 9"
  putStrLn "10. Level 10"
  putStrLn "0 to go Back"
  putStrLn "Your selection: "
  level <- getInput 0 10
  if level == 0 then menu
  else do
    maze <- readMazeFromCSV ("maze levels/level " ++ show level ++ ".csv")
    case maze of
        Nothing -> putStrLn "Failed to read maze from CSV file."
        Just mazeLevel -> do
            putStrLn "Maze successfully loaded!"
            run mazeLevel (Coord 0 1) 0

howToPlay :: IO()
howToPlay = do
  clearScreen
  withFile "How to play.txt" ReadMode $ \handle -> do
        hSetEncoding handle utf8
        guide <- hGetContents handle
        putStrLn guide
  continue <- getLine
  menu

main :: IO ()
main = menu





