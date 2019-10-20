module GameOfLife where

import qualified Data.Map.Lazy as Map

data State = Alive | Dead deriving (Eq)
type States = [State]
type Coord = (Int, Int)
type Coords = [Coord]
data Grid = Grid Int Int (Map.Map Coord State)

instance Show Grid where
    show(Grid h w x) = if (Map.size x) == 0 then "" else show(Map.elems (Map.take w x)) ++ "\n" ++ show(Grid h w (Map.drop w x))

instance Show State where
    show(Alive) = "@"
    show(Dead) = " "

buildGrid :: Int -> Int -> Grid
buildGrid h w = (Grid h w (Map.fromList [((i,j), Dead) | i <- [1..h], j <- [1..w]]))

setStates :: Coords -> State -> Grid -> Grid
setStates [] _ grid = grid
setStates (x:xs) state grid = setStates xs state (setState x state grid)

setState :: Coord -> State -> Grid -> Grid
setState coord state (Grid h w x) = (Grid h w (Map.insert coord state x))

getState :: Coord -> Grid -> State
getState (a,b) (Grid h w x) = if a < 1 || a > h || b < 1 || b > w then Dead else x Map.! (a,b)

getStates :: Coords -> Grid -> States
getStates coords grid = customMap getState coords grid

getNeighbours :: Coord -> Coords
getNeighbours (a,b) = [(a, b), (a-1, b-1), (a-1, b), (a-1, b+1), (a, b-1), (a, b+1), (a+1, b-1), (a+1, b), (a+1, b+1)]

checkRules :: States -> State
checkRules states
                | head states == Alive && (length $ filter (==Alive) $ tail states) < 2 = Dead
                | head states == Alive && (length $ filter (==Alive) $ tail states) == 2 = Alive
                | head states == Alive && (length $ filter (==Alive) $ tail states) == 3 = Alive
                | head states == Alive && (length $ filter (==Alive) $ tail states) > 3 = Dead
                | head states == Dead && (length $ filter (==Alive) $ tail states) == 3 = Alive
                | otherwise = Dead

nextGen :: Grid -> Grid
nextGen (Grid h w x) = let keys = (Map.keys x)
                           neighbours = map getNeighbours keys
                           states = customMap getStates neighbours (Grid h w x)
                           newStates = map checkRules states
                           newGrid = Map.fromList (zip keys newStates)
                       in (Grid h w newGrid)

customMap :: (a -> Grid -> b) -> [a] -> Grid -> [b]
customMap f [] grid = []
customMap f (x:sx) grid = (f x grid):customMap f sx grid
