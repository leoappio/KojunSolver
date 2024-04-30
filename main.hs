module Main where

import Types
import KojunSolver(searchForSolution, reduceChoices, choices)

values6x6 :: Table
values6x6 = [[2,0,0,0,1,0], [0,0,0,3,0,0], [0,3,0,0,5,3], [0,0,0,0,0,0], [0,0,3,0,4,2], [0,0,0,0,0,0]]

groups6x6 :: Table
groups6x6 = [[1,1,2,2,2,3], [4,4,4,4,4,3], [5,6,6,6,4,7], [5,5,5,6,7,7], [8,8,10,0,0,0], [9,9,10,10,0,0]]

main :: IO ()
main = do
    putStrLn "Resolving 6x6 board"
    mapM_ print (getSolution values6x6 groups6x6)

getSolution :: Table -> Table -> Table
getSolution values groups = head $ searchForSolution (reduceChoices (choices values groups) groups) groups
