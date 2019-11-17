module Main where

import GameOfLife

-- | Deze functie stelt een aantal vragen aan de gebruiker die nodig zijn 
main :: IO ()
main = do 
         putStrLn "Give height grid (number e.g. 10): "
         y <- getLine
         putStrLn "Give weight grid (number e.g. 10): "
         x <- getLine
         let h = read y :: Int
         let w = read x :: Int
         putStrLn "Give begin living cells grid (Coords e.g. [(5,5),(5,6),(5,7),(6,5),(6,7),(7,5),(7,6),(7,7)]) : "
         bg <- getLine
         let beginState = read bg :: Coords
         let grid = buildGrid h w beginState
         print grid
         printNextGen beginState h w
         
-- | Deze functie laat een grid zien op het scherm waarbij aangeven is welke cellen levend moeten zijn en hoe groot het grid moet zijn.
printNextGen :: Coords -- ^ De coördinaten de levend in het grid moet zijn.
             -> Int -- ^ De hoogte die het grid moet hebben.
             -> Int -- ^ De breedte die het grid moet hebben.
             -> IO () 
printNextGen coords h w = do
                     putStrLn "Press enter for next generation: "
                     n <- getLine
                     if n == "" then print(buildGrid h w (nextGen coords)) else printNextGen coords h w
                     printNextGen (nextGen coords) h w