module KojunSolver(searchForSolution, reduceChoices, choices) where

import Types
import MatrixHelper
import Data.List

-- modulo do solver do Kojun

-- conta quantos elementos estão em um grupo
groupSize :: Eq m => m -> Matrix m -> Int
groupSize _ [] = 0
groupSize id groups = sum [count id p | p <- groups]
  where count x xs = length (filter (==x) xs)


-- id identifica um bloco (grupo)
-- cria lista dos valores que já estão em um bloco
getValuesInGroup :: Eq m => Matrix m -> Table -> Int -> [m]
getValuesInGroup values groups id = map fst $ filter ((==id) . snd) valuesTuple
    where
        valuesTuple = foldl1 (++) (zipWith zip values groups)


minus :: Choices -> Choices -> Choices
xs `minus` ys = if singleElementInList xs then xs else xs \\ ys


-- ve se a linha tem apenas um elemento
singleElementInList :: [a] -> Bool
singleElementInList [_] = True
singleElementInList _ = False


-- cria escolhas possíveis em cada célula
-- células sem valor possuem valores possíveis de 1 até tamanho do grupo
-- remove os valores que já estão naquele grupo (para não haver repetição)
choices :: Table -> Table -> Matrix Choices
choices values groups = map (map choice) (zipWith zip values groups)
    where choice (v, p) = if v == 0 then [1..(groupSize p groups)] `minus` (getValuesInGroup values groups p) else [v]


-- cria as colunas da matriz delimitando pelos grupos
groupsByColumn :: Eq m => Matrix m -> Table -> [Row m]
groupsByColumn values groups = zipWith zip (cols values) (cols groups) >>= map (map fst) . groupBy (\x y -> snd x == snd y)


-- em cada coluna (já dividida por grupos), faz uma redução em lista
-- faz a redução das escolhas dentro de cada lista e retorna a matriz reduzida
reduceChoices :: Matrix Choices -> Table -> Matrix Choices
reduceChoices values groups = cols $ originalCols (map reduceChoicesByList (groupsByColumn values groups)) (size values)


-- em posições que só possui um valor possível, seta o valor na célula
reduceChoicesByList :: Row Choices -> Row Choices
reduceChoicesByList xss = [xs `minus` singles | xs <- xss]
    where singles = concat (filter singleElementInList xss)


-- filtra soluções baseada nas escolhas, percorrendo as células
-- cria uma lista com tabuleiros com possíveis soluções para o tabuleiro
searchForSolution :: Matrix Choices -> Table -> [Table]
searchForSolution values groups
    | notPossible values groups = []  -- caso não tenha solução, retorna lista vazia
    | all (all singleElementInList) values = [map concat values]  -- não precisa de mais reduções
    | otherwise = [g | values' <- expandChoices values, g <- searchForSolution (reduceChoices values' groups) groups]  -- ainda precisa de redução, expandindo a matriz e em seguida reduzindo, continuando a busca


-- ve se tabuleiro não gera solução
notPossible :: Matrix Choices -> Table -> Bool
notPossible values groups = empty values || not (valid values groups)
    where
        empty m = any (any null) m


-- verifica se uma matriz de escolhas é valida
valid :: Matrix Choices -> Table -> Bool
valid values groups = all validNeighbour (cols values) &&
                      all validNeighbour (rows values) &&  -- nenhuma célula tem vizinhos com valor igual (em linha e coluna)
                      all validRow (matrixByGroup values groups) &&  --compara dentro do grupo para ver se não há valor igual
                      all descendingRow (groupsByColumn values groups)  -- deve haver ordenamento descrescente de cima para baixo nas colunas dentro de um grupo


-- vizinhos não podem ter o mesmo valor
validNeighbour :: Eq a => Row [a] -> Bool
validNeighbour [] = True
validNeighbour [a] = True
validNeighbour (a:b:bs)  -- compara primeiro com segundo, se necessário, chama a função usando o segundo valor e o resto da lista 
    | (length a <= 1) && (length b <= 1) = if a == b then False else validNeighbour (b:bs)
    | otherwise = validNeighbour (b:bs)  -- b = segundo valor; bs = restante da Row


-- não pode haver valores iguais em uma linha
validRow :: Eq a => Row [a] -> Bool
validRow [] = True
validRow (x:xs) = if (length x <= 1) then not (elem x xs) && validRow xs else validRow xs


-- ordenamento decrescente nas colunas (não pode ter valor menor sobre maior)
descendingRow :: Ord a => Row [a] -> Bool
descendingRow [] = True
descendingRow [a] = True
descendingRow (a:b:bs)
    | (length a <= 1) && (length b <= 1) = if a < b then False else descendingRow (b:bs)
    | otherwise = descendingRow (b:bs)


-- faz expanção das escolhas
expandChoices :: Matrix Choices -> [Matrix Choices]
expandChoices m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where
        (rows1,row:rows2) = break (not . all singleElementInList) m
        (row1,cs:row2) = break (not . singleElementInList) row
