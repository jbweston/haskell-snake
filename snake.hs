import Data.Maybe
import qualified System.Random as R
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

data World = World { snake :: Snake
                   , food :: Position
                   , direction :: Direction
                   , rand :: R.StdGen
                   , limits :: (Int, Int)
                   } deriving (Show)

data GameState = Playing World
               | GameOver
               deriving (Show)


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

eat :: Snake -> Direction -> Snake
eat s d = (move d $ head s):s

randomPosition :: R.RandomGen g => (Int, Int) -> g -> (Position, g)
randomPosition (maxr, maxc) g =
    let (r, g1) = R.randomR (1, maxr) g
        (c, g2) = R.randomR (1, maxc) g1
    in ((r, c), g2)

randomFreePosition :: R.RandomGen g => (Int, Int) -> g -> Snake -> (Position, g)
randomFreePosition lim g s =
    head $ dropWhile inSnake (randomPositions g)
    where inSnake (x, _) = x `elem` s
          randomPositions h = r:randomPositions g'
              where r@(_, g') = randomPosition lim h


advance :: World -> Direction -> World
advance w newDir
    | newDir == opposite (direction w) = w
    | (move newDir $ head $ snake w) == (food w) = eaten
    | otherwise = slithered
    where slithered = w { snake = slither (snake w) newDir
                        , direction = newDir
                        }
          eaten = w { snake = eat (snake w) newDir
                    , direction = newDir
                    , food = newFood
                    , rand = newRand
                    }
          (newFood, newRand) = randomFreePosition (limits w) (rand w) $ snake eaten

playGame :: World -> [Direction] -> [GameState]
playGame iw ds =
    play (scanl advance iw ds)
    where
        play (w:rest)
          | collision $ snake w = [GameOver]
          | any (outside $ limits w) (snake w) = [GameOver]
          | otherwise = Playing w : play rest
        play [] = []
        collision (x:xs) = any (== x) (tail xs)
        outside (maxr, maxc) (r, c) =
            r < 1 || r > maxr || c < 1 || c > maxc


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

drawBorder :: World -> IO ()
drawBorder w = do
    let (r, c) = limits w
    mapM_ (draw '*') [(0, x) | x <- [0..c+1]]
    mapM_ (draw '*') [(r+1, x) | x <- [0..c+1]]
    mapM_ (draw '*') [(x, 0) | x <- [0..r+1]]
    mapM_ (draw '*') [(x, c+1) | x <- [0..r+1]]

renderWorld :: Char -> Char -> World -> IO ()
renderWorld s f w = do
    draw f (food w)
    mapM_ (draw s) (reverse $ snake w)
    cursorBackward 1

drawWorld = renderWorld '@' '#'
clearWorld = renderWorld ' ' ' '

drawUpdate :: (GameState, GameState) -> IO ()
drawUpdate (_, GameOver) = clearScreen >> putStrLn "You died!"
drawUpdate (Playing old, Playing new) = clearWorld old >> drawWorld new

initialWorld = World { snake = [(1, x)| x <- [1..3]]
                     , food = (2, 2)
                     , direction = West
                     , rand = R.mkStdGen 0
                     , limits = (5, 5)
                     }

main = do
    initScreen
    drawBorder initialWorld
    input <- getContents
    let states = playGame initialWorld (parseInput input)
    drawWorld initialWorld
    mapM_ drawUpdate $ zip states (tail states)
