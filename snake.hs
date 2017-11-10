import Data.Maybe
import System.IO
import System.Console.ANSI

data Direction = North
               | South
               | East
               | West
               deriving (Show, Eq)

data Command = Quit | Go Direction deriving (Show, Eq)

type Position = (Int, Int)
type Snake = [Position]

data World = World { snake :: Snake, direction :: Direction } deriving (Show)

opposite :: Direction -> Direction
opposite d = case d of
    North -> South
    South -> North
    East -> West
    West -> East

move :: Direction -> Position -> Position
move d (r, c) = case d of
    North -> (r - 1, c)
    South -> (r + 1, c)
    East -> (r, c + 1)
    West -> (r, c - 1)

slither :: Snake -> Direction -> Snake
slither s d = (move d $ head s):(init s)


advance :: World -> Direction -> World
advance w newDir
    | newDir == opposite (direction w) = w
    | otherwise = World { snake = slither (snake w) newDir
                        , direction = newDir
                        }

-- User input

parseCommand :: Char -> Maybe Command
parseCommand c = case c of
    'q' -> Just Quit
    'w' -> Just $ Go North
    'a' -> Just $ Go West
    's' -> Just $ Go South
    'd' -> Just $ Go East
    _   -> Nothing

parseInput :: [Char] -> [Direction]
parseInput i =
    map fromCommand $ takeWhile (/= Quit) $ mapMaybe parseCommand i
    where fromCommand (Go x) = x


-- Viewing

initScreen = do
    hSetBuffering stdin NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho stdin False
    clearScreen

draw :: Char -> Position -> IO ()
draw char (row, col) = do
    setCursorPosition row col
    putChar char

renderWorld :: Char -> World -> IO ()
renderWorld s w = do
    mapM_ (draw s) (reverse $ snake w)
    cursorBackward 1

drawWorld = renderWorld '@'
clearWorld = renderWorld ' '

drawUpdate :: (World, World) -> IO ()
drawUpdate (old, new) = clearWorld old >> drawWorld new

initialWorld = World { snake = [(1, x)| x <- [1..3]]
                     , direction = West
                     }

main = do
    initScreen
    input <- getContents
    let states = scanl advance initialWorld (parseInput input)
    drawWorld initialWorld
    mapM_ drawUpdate $ zip states (tail states)
