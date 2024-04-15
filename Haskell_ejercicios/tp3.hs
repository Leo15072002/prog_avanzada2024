------------- Trabajo Practico 3 ---------------

--Ejercicio 1
merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
                    |x <= y = x : merge xs (y:ys)
                    |otherwise =  y: merge (x:xs) ys

--Ejercicio 2
ordNum :: [Int] -> [Int]
ordNum [] = []  -- caso base del vacio
ordNum (x:xs) = ordNum left ++ [x] ++ ordNum right
                where  
                    left = [a | a <- xs, a <= x]
                    right = [b | b <- xs, b > x]

--Ejercicio 3
dosElev :: Int -> Int
dosElev 0 = 1
dosElev n = 2 * (dosElev (n-1))

--Ejercicio 4
numBits :: Int -> [Int]
numBits 0 = []
numBits n = numBits (n `div` 2) ++ [n `mod` 2]

--Ejercicio 5
binaryPar :: [Int] -> Bool  --dado un numero binario (en lista) retorna si es par o no
binaryPar [] = False
binaryPar xs = last xs `mod` 2 == 0

--Ejercicio 6
distanciaH :: Eq a => [a] -> [a] -> Int
distanciaH [] [] = 0    --evalua el primer argumento
distanciaH xs [] = 0
distanciaH [] ys= 0
distanciaH (x:xs) (y:ys)
            |x /= y = 1 + distanciaH xs ys
            |otherwise = distanciaH xs ys 

--Ejercicio 7
cuadPerfect :: Int -> Bool
cuadPerfect n = (length [x | x<-[1..n], (x^2 == n)]) > 0

--Otro metodo
cuadPerfectDos :: Int -> Bool
cuadPerfectDos n = [x | x<-[0..n], x*x == n] /= []

cuadPerfectDosa :: Int -> Bool
cuadPerfectDosa n = null [x | x<-[0..n], x*x == n]  --con el null me retorna el valor logico contrario, arreglar
--Ejercicio 8
repetiDos :: Ord a => a -> Int -> [a]
repetiDos a 0 = []
repetiDos a n = repetiDos a (n-1) ++ [a]

--Ejercicio 9 
nElem :: [Int] -> Int -> Int
nElem [] n = undefined
nElem (x:xs) 0 = x
nElem (x:xs) n = nElem xs (n-1)

--Ejercicio 10 
posicionesC :: [Char] -> Char -> [Int]
posicionesC [] c = []
posicionesC xs c = [i | (x,i) <- zip xs [0..], c == x ]
--zip :: [a] -> [b] -> [(a,b)]


--Ejercicio 11
functcompac :: [Int] -> [Int]
functcompac [] = []
functcompac [x] = [x]  
functcompac (x:xs)
                    | x /= head xs = x : functcompac xs  
                    | otherwise = functcompac xs







