module Main where

import GameOfLife

main :: IO ()
main = do 
         putStr "Give size height grid: "
         y <- getLine
         putStr "Give size weight grid: "
         x <- getLine
         let h = (read y :: Int)
         let w = (read x :: Int)
         putStr "Give begin state: " -- possoble option = [(10,10),(10,11),(10,12),(9,12),(8,11)]
         bg <- getLine
         let beginState = (read bg :: Coords)
         let grid = buildGrid h w
         let beginGrid = setStates beginState Alive grid
         print(beginGrid)
         printNextGen beginGrid
         

printNextGen :: Grid -> IO ()
printNextGen grid = do
                     putStr "Press enter for next generation: "
                     n <- getLine
                     if n == "" then print(nextGen grid) else printNextGen grid
                     printNextGen (nextGen grid)