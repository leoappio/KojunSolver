module Main where

import Types
import KojunSolver(searchForSolution, reduceChoices, choices)

--6x6
values :: Table
values = [[2,0,0,0,1,0], [0,0,0,3,0,0], [0,3,0,0,5,3], [0,0,0,0,0,0], [0,0,3,0,4,2], [0,0,0,0,0,0]]

groups :: Table
groups = [[1,1,2,2,2,3], [4,4,4,4,4,3], [5,6,6,6,4,7], [5,5,5,6,7,7], [8,8,10,0,0,0], [9,9,10,10,0,0]]

--8x8
-- values :: Table
-- values = [[0,0,0,6,5,3,0,0],
--           [0,0,6,0,0,0,0,0],
--           [2,4,0,0,3,0,0,3],
--           [1,0,6,0,0,0,4,0],
--           [0,3,0,6,0,0,0,4],
--           [0,2,0,0,0,0,1,3],
--           [0,0,5,0,0,1,0,0],
--           [0,2,0,0,0,0,2,4]]

-- groups :: Table
-- groups = [[1,2,3,4,4,4,4,4],
--           [1,3,3,5,4,6,4,7],
--           [3,3,3,5,8,6,9,9],
--           [3,10,10,8,8,9,9,9],
--           [10,10,11,13,17,17,18,18],
--           [12,10,10,13,13,16,16,18],
--           [12,10,13,13,15,15,18,18],
--           [12,14,14,13,19,19,19,19]]

main :: IO ()
main = do
    putStrLn "Resolving..."
    mapM_ print (getSolution values groups)

-- Recebe dois table e retorna um table
getSolution :: Table -> Table -> Table

-- função 'choices' vai gerar uma matriz de possíveis escolhas para cada célula do tabuleiro com base nos valores e nos grupos.
-- função 'reduceChoices' pega a matriz gerada e tenta reduzir as possibilidades em cada célula,
-- utilizando a lógica para evitar escolhas que já não são possíveis com base nas outras células na mesma coluna ou grupo.
-- funcao 'searchForSolution' vai aplicar o backtracking para verificar se a matriz gerada é uma solução válida, se não
-- gera novas matrizes de forma recursiva e sempre reduzindo as possibilidades.
getSolution values groups = head $ searchForSolution (reduceChoices (choices values groups) groups) groups
