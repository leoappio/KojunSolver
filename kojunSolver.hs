module KojunSolver(searchForSolution, reduceChoices, choices) where

import Types
import MatrixHelper
import Data.List

-- modulo do solver do Kojun --


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


-- subtrai a lista de escolhas `ys` de `xs`, mas mantém `xs` inalterado se tiver apenas um elemento.
minus :: Choices -> Choices -> Choices
xs `minus` ys = if singleElementInList xs then xs else xs \\ ys


-- ve se a linha tem apenas um elemento
singleElementInList :: [a] -> Bool
singleElementInList [_] = True
singleElementInList _ = False


-- Gera uma matriz de escolhas possíveis para cada célula no tabuleiro.
-- Para células vazias (valor 0), as escolhas são todos os números de 1 até o tamanho do grupo, excluindo números que já estão presentes no grupo.
choices :: Table -> Table -> Matrix Choices
choices values groups = map (map choice) (zipWith zip values groups)
    where choice (v, p) = if v == 0 then [1..(groupSize p groups)] `minus` (getValuesInGroup values groups p) else [v]


-- cria as colunas da matriz delimitando pelos grupos
groupsByColumn :: Eq m => Matrix m -> Table -> [Row m]
groupsByColumn values groups = zipWith zip (cols values) (cols groups) >>= map (map fst) . groupBy (\x y -> snd x == snd y)


-- Reduz as escolhas disponíveis em cada célula com base nas escolhas nas colunas e grupos correspondentes.
-- Aplica uma redução coluna por coluna, ajustando as escolhas disponíveis para evitar conflitos.
-- A ideia é simplificar o espaço de busca antes do backtracking.
reduceChoices :: Matrix Choices -> Table -> Matrix Choices
reduceChoices values groups = cols $ originalCols (map reduceChoicesByList (groupsByColumn values groups)) (size values)


-- em posições que só possui um valor possível, seta o valor na célula
reduceChoicesByList :: Row Choices -> Row Choices
reduceChoicesByList xss = [xs `minus` singles | xs <- xss]
    where singles = concat (filter singleElementInList xss)


-- Função principal de backtracking que busca por soluções válidas.
-- Avalia se o estado atual das escolhas é viável; se não for, retorna uma lista vazia.
-- Se todas as células têm uma única escolha, uma solução completa foi encontrada.
-- Se ainda existem múltiplas escolhas, a função continua a busca recursivamente com novas configurações geradas por `expandChoices`.
searchForSolution :: Matrix Choices -> Table -> [Table]
searchForSolution values groups
    | notPossible values groups = []  -- caso não tenha solução, retorna lista vazia
    | all (all singleElementInList) values = [map concat values]  -- não precisa de mais reduções
    | otherwise = [g | values' <- expandChoices values, g <- searchForSolution (reduceChoices values' groups) groups]  -- ainda precisa de redução.


-- verifica se tabuleiro não gera uma solução
notPossible :: Matrix Choices -> Table -> Bool
notPossible values groups = empty values || not (valid values groups)
    where
        empty m = any (any null) m


-- Verifica se uma matriz de escolhas é válida com base em várias regras.
-- Validações incluem a verificação de vizinhos (nenhum par adjacente pode ter o mesmo valor),
-- valores dentro de um grupo (não devem repetir), e ordenamento nas colunas dentro de um grupo (devem ser descrescentes).
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


-- Expande as escolhas de uma célula que possui várias possibilidades, gerando várias matrizes de escolhas.
-- Cada matriz gerada é uma cópia da original, exceto por uma célula, que tem uma das possíveis escolhas fixadas.
-- Esta função é necessária para o backtracking, permitindo explorar diferentes caminhos para a solução.
expandChoices :: Matrix Choices -> [Matrix Choices]
expandChoices m = [rows1 ++ [row1 ++ [c] : row2] ++ rows2 | c <- cs]
    where
        (rows1,row:rows2) = break (not . all singleElementInList) m
        (row1,cs:row2) = break (not . singleElementInList) row
