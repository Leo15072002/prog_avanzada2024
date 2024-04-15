------------- Trabajo Practico 1 ---------------

factos :: Integer -> Integer 
factos 0 = 1
factos n = n * factos(n-1)

--Ejercicio 5
esNoPar :: [Int] -> Bool
esNoPar xs = head(reverse xs) `mod` 2 == 0

--Ejercicio 6
mulTres :: [Int] -> Bool
mulTres xs = sum xs `mod` 3 == 0

--Ejercicio 7
multDos :: [Int] -> Bool
multDos xs = sum xs `mod` 2 == 0    --adicional para hacer el ejercicio 7

multSeis :: [Int] -> Bool
multSeis xs = multDos xs && mulTres xs 

--Ejercicio 8
numLista :: Int -> [Int] 
numLista n 
    | n < 10 = [n]  --en el caso de que de un numero menor que 10, directamente lo transforme en lista
    | otherwise = numLista (n `div` 10) ++ [n `mod` 10]

--Ejercicio 9
cortar :: Int -> Int -> [Char] -> [Char]
cortar i j w = if j>i then (take (j-(i+1))(drop i w)) else (take (i-(j+1))(drop j w))


--valAbsolute :: Int -> Int
--valAbsolute n = if n<0 then (n*(-1)) else 