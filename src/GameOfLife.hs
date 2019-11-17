module GameOfLife where

import Data.List (nub)

type Cells = [Cell]
type Cell = (Coord, State)
type Coord = (Int, Int)
type Coords = [Coord]
data State = Alive | Dead deriving (Eq)
type States = [State]
data Grid = Grid Int Int Cells

instance Show Grid where
    show(Grid h w []) = ""
    show(Grid h w x) = let (_, states) = unzip x
                       in show(take w states) ++ "\n" ++ show(Grid h w (drop w x))

instance Show State where
    show Alive = "#"
    show Dead = " "


-- | Deze functie berekent de nieuwe generatie van een grid op basis van een gegeven lijst met levende coördinaten.
nextGen :: Coords -- ^ Een lijst met levende coördinaten als oude generatie.
        -> Coords -- ^ Een lijst met levende coördinaten als nieuwe generatie.
nextGen coords = let cellCoords = nub (concatMap getNeighbours coords)
                     newCellStates = customMap nextGenCell cellCoords coords
                 in takeAliveCells (zip cellCoords newCellStates)

-- | Deze functie berekent de nieuwe generatie van een coördinaat.
nextGenCell :: Coord -- ^ De coördinaat waarvan de nieuwe generatie berekent moet worden.
            -> Coords -- ^ Een lijst met cecoördinaten waarvan we weten dat ze levend zijn.
            -> State -- ^ De nieuwe staat die de gegeven cel moet krijgen in de nieuwe generatie.
nextGenCell coord coords = checkRules (customMap checkState (getNeighbours coord) coords)

-- | Deze functie berekent de coördinaten van de buren van een gegeven cel.
getNeighbours :: Coord -- ^ De coördinaat waarvan de buren berekent moeten worden.
              -> Coords -- ^ De coördinaten van de buren van de gegeven coördinaat.
getNeighbours (y,x) = [(y, x), (y-1, x-1), (y-1, x), (y-1, x+1), (y, x-1), (y, x+1), (y+1, x-1), (y+1, x), (y+1, x+1)]

-- | Deze functie berekent of een coördinaat leven of dood is.
checkState :: Coord -- ^ De coördinaat waarvan je wilt weten welke staat hij heeft
           -> Coords -- ^ De coördinaten waarvan je weet de ze levend zijn.
           -> State -- ^ De berekende staat van de gegeven coördinaat.
checkState coord coords = if coord `elem` coords then Alive else Dead

-- | Deze functie berekent wat de nieuwe staat van een cel is aan de hand van een lijst met staten.
checkRules :: States -- ^ Een lijst met 9 staten waarvan de eerste staat in de lijst de oude staat is en de rest de buren zijn van de cell..
           -> State -- ^ De nieuw berekende staat aan de hand van de buren van de cel
checkRules states
                | head states == Alive && length (filter (==Alive) $ tail states) == 2 = Alive
                | head states == Alive && length (filter (==Alive) $ tail states) == 3 = Alive
                | head states == Dead && length (filter (==Alive) $ tail states) == 3 = Alive
                | otherwise = Dead

-- | Deze functie berekent de welke cellen levend zijn in een lijst van cellen.
takeAliveCells :: Cells -- ^ Een lijst met cellen waarvan berekent word welke levend zijn.
               -> Coords -- ^ Een lijst met coördinaten die de staat levend hebben.
takeAliveCells [] = []
takeAliveCells ((coord,state):xs) = if state == Alive then coord : takeAliveCells xs else takeAliveCells xs

-- | Deze functie bouwt een grid waarbij je kan aangeven hoe groot hij moet zijn, en welke cellen levend moeten zijn.
buildGrid :: Int -- ^ Een getal die de grootte van het grid aangeeft in de hoogte.
          -> Int -- ^ Een getal die de grootte van het grid aangeeft in de breedte.
          -> Coords -- ^ Een lijst met coördinaten die levend gemaakt worden in het grid.
          -> Grid -- ^ Het gecreeërde grid met de aangegeven grootte en welke cellen levend zijn.
buildGrid h w coords = setStatesAlive coords (Grid h w [((y,x), Dead) | y <- [1..h], x <- [1..w]])

-- | Deze functie maakt cellen in een grid levend aan de hand van een lijst met coördinaten.
setStatesAlive :: Coords -- ^ Een lijst met coördinaten die levend gemaakt moeten worden in het grid
               -> Grid -- ^ Het gegeven grid waar cellen levend in gemaakt moeten worden.
               -> Grid -- ^ Het grid waar de aangegeven cellen levend zijn gemaakt.
setStatesAlive coords grid = foldl (flip setStateAlive) grid coords

-- | Deze functie maakt een aangegeven cell levend in een grid.
setStateAlive :: Coord -- ^ De coördinaat die levend gemaakt moet worden in het gegeven grid.
              -> Grid -- ^ Het gegeven grid waar de cel levend in gemaakt moet worden.
              -> Grid -- ^ Het grid waar de aangegeven cel levend gemaakt is.
setStateAlive coordA (Grid h w x) = Grid h w (map (\(coordB, state) -> if coordB == coordA then (coordB, Alive) else (coordB, state)) x)

-- | Dit is een geabstraheerde functie die bijna gelijk is aan de functie map. Het ver schil is dat je bij deze functie een extra argument kan meegeven aan de functie die je wilt mappen over de lijst. 
customMap :: (a -> b -> c) -- ^ De functie die gemapt word over de lijst.
          -> [a] -- ^ De lijst waarover de functie gemapt word.
          -> b -- ^ Het extra argument die nodig is voor de functie.
          -> [c] -- ^ De lijst waarover gemapt is met de functie met het extra argument.
customMap f a b = map (`f` b) a