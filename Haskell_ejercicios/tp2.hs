------------- Trabajo Practico 2 ---------------
--Ejercicio 2
hd :: [a] -> a              
hd (x:xs) = x

tl :: [a] -> [a]
tl (x:xs) = xs

lst :: [a] -> a
lst [x] = x
lst (x:xs) = lst xs

ini :: [a] -> [a]
ini [x] = []    --(caso base)
ini (x:xs) = x : ini xs     --(caso recursivo)

--Ejercicio 3
maxTree :: Int -> Int -> Int -> Int
maxTree a b c = (max a (max b c))

--Ejercicio 4
concateniar :: [a] -> [a] -> [a]
concateniar [] ys = ys
concateniar xs [] = xs
concateniar (x:xs) ys = x : concateniar xs ys

tomar :: Int ->[a] -> [a]  
tomar n [] = []
tomar 0 (x:xs) = []
tomar n (x:xs) = x: tomar(n-1) xs
tirar :: Int -> [a] -> [a]  
tirar n [] = []
tirar 0 (x:xs) = x:xs
tirar n (x:xs) = tirar(n-1) xs

agregarFinal :: a -> [a] -> [a]
agregarFinal a [] = [a]
agregarFinal a xs =  xs ++ [a]

--Ejercicio 5
valAbs :: Int -> Int
valAbs n
    | n < 0 = -n 
    | otherwise = n

--Ejercicio 6
--contempla los negativos
edad :: (Int,Int,Int) -> (Int,Int,Int) -> Int 
edad (dd,mm,aaaa) (dd1,mm1,aaaa1) | (mm1, dd1)<(mm, dd) = (valAbs aaaa1 - aaaa) - 1
                                  | otherwise = valAbs aaaa1 - aaaa



--no contempla nada jiji
edadClase :: (Int,Int,Int) -> (Int,Int,Int) -> Int 
edadClase (x,y,z) (p,q,r) 
                          |(q<y) || (q==y && p<x) = r-z-1
                          |otherwise = r-z

--Ejercicio 7
xor :: Bool -> Bool -> Bool
xor True True = False 
xor False False = False
xor _ _ = True

--7b)
--manera 1
xor1 :: Bool -> Bool -> Bool
xor1 x y = if (x==y) then False else True

--manera 2
xor2 :: Bool -> Bool -> Bool
xor2 True y = not y
xor2 False y = y 

--Ejercicio 8, 
nuPrim :: Int -> Bool
nuPrim num = length(dividir num) == 2

dividir :: Int -> [Int] -- funcion auxiliar
dividir n = [x | x <- [1..n], n `mod` x == 0] 

-- Ejercicio 9
listNaturales :: Int -> [Int]
listNaturales 0 = []
listNaturales n = if nuPrim(n-1) then listNaturales(n-1) ++ [n] else listNaturales(n-1)

--otra manera
listNat :: Int -> [Int]
listNat n = [x | x <- [1..n-1], nuPrim x]

--Ejercicio 10
reverList :: [a] -> [a]
reverList [] = []
reverList (x:xs) = reverList xs ++ [x]

--Ejercicio 11
listPrim :: [Int] -> [Int]
listPrim xs = [x | x <- xs, nuPrim x]

--Ejercicio 12
palinromo :: (Eq a) => [a] -> Bool
palinromo xs = xs == reverse xs

--Ejercicio 13
cantRaices :: Int -> Int -> Int -> Int
cantRaices a b c 
                | (b^2 - 4*a*c) > 0 = 2
                | (b^2 - 4*a*c) == 0 = 1
                |otherwise = 0




