module MatrixHelper(size, rows, cols, matrixByGroup, originalCols) where

import Types
import Data.List

-- modulo que trata das funções que irão servir de base para as operações com as matrizes


-- retorna ordem da matriz
size :: Matrix a -> Int
size = length . head


-- linhas de uma matriz
rows :: Matrix a -> [Row a]
rows = id


-- colunas de uma matriz
cols :: Matrix a -> [Row a]
cols = transpose


-- cria tupla com valor e grupo de determinada posição
-- divide a matriz de valores de acordo com os grupos
-- retorna matriz com blocos
matrixByGroup :: Eq a => Matrix a -> Matrix Int -> Matrix a
matrixByGroup values groups = [filterByGroup g vg | g <- nub (concat groups)]
    where
        vg = concat $ zipWith zip values groups
        filterByGroup g = map fst . filter ((== g) . snd)


-- remonta as colunas originais a partir de uma dividida por blocos
originalCols :: [Row a] -> Int -> [Row a]
originalCols blocks size = map (take size) . takeWhile (not . null) $ iterate (drop size) (concat blocks)
