import qualified System.Random as R
import System.IO
import System.Console.ANSI

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import Control.Concurrent.Async
import Pipes
import Pipes.Concurrent
import qualified Pipes.Prelude as P
import Data.Monoid ((<>))

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


-- takeWhile (not . pred), but including the element that fails 'pred'
takeUntilAfter :: Monad m => (a -> Bool) -> Pipe a a m ()
takeUntilAfter p = do
    v <- await
    yield v
    if p v then return () else takeUntilAfter p

--  zip l (tail l), but as a pipe
deltas :: Monad m => Pipe a (a,a) m ()
deltas = do
    first <- await
    P.scan remember (first, first) id
    where
        remember (_, a) b = (a, b)

rateLimit :: Int -> Pipe b b IO ()
rateLimit t = forever $ do
    lift $ threadDelay (t * 1000000)
    await >>= yield


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

toGameState :: World -> GameState
toGameState w
    | collision $ snake w = GameOver
    | any (outside $ limits w) (snake w) = GameOver
    | otherwise = Playing w
    where
        collision (x:xs) = any (== x) (tail xs)
        outside (maxr, maxc) (r, c) =
            r < 1 || r > maxr || c < 1 || c > maxc

transitions game =
    P.scan advance game id
    >-> P.map toGameState
    >-> takeUntilAfter isGameOver
    >-> deltas
    where
        isGameOver GameOver = True
        isGameOver (Playing _) = False


-- User input

parseCommand :: Char -> Maybe Command
parseCommand c = case c of
    'q' -> Just Quit
    'w' -> Just $ Go North
    'a' -> Just $ Go West
    's' -> Just $ Go South
    'd' -> Just $ Go East
    _   -> Nothing

getCommands :: Producer Command IO ()
getCommands = forever $ do
    c <- lift getChar
    case parseCommand c of
        Nothing -> return ()
        Just x -> yield x

removeOpposites :: Monad m => Pipe Direction Direction m r
removeOpposites = do
    first <- await
    yield first
    loop first
    where
        loop prev = do
            next <- await
            if next == opposite prev
                then loop prev
                else yield next  >> loop next

getDirections :: Producer Direction IO ()
getDirections =
    getCommands
    >-> P.takeWhile (/= Quit)
    >-> P.map fromCommand
    >-> removeOpposites
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
drawUpdate (Playing old, Playing new) = clearWorld old >> drawWorld new
drawUpdate (Playing w, GameOver) = do
    let text = "You died!"
        (r, c) = limits w
    clearScreen
    setCursorPosition (r`div`2) ((c - length text)`div`2)
    putStrLn "You died!"
    setCursorPosition r 0

initialWorld = World { snake = [(5, x)| x <- [10..13]]
                     , food = (5, 5)
                     , direction = West
                     , rand = R.mkStdGen 0
                     , limits = (20, 20)
                     }

main = do
    initScreen
    drawBorder initialWorld
    drawWorld initialWorld

    let initialDir = direction initialWorld
        run p = async $ runEffect p >> performGC
        from = fromInput
        to = toOutput

    (mO, mI) <- spawn unbounded
    (dO, dI) <- spawn $ latest initialDir

    inputTask <- run $ getDirections >-> to (mO <> dO)
    delayedTask <- run $ from dI >-> rateLimit 1 >-> to mO
    drawingTask <- run $ for
        (from mI >-> transitions initialWorld)
        (lift . drawUpdate)

    -- inputTask will end if 'q', drawingTask will end if we die
    waitAny [inputTask, drawingTask]
