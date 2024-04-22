------------- Trabajo Practico 3 ---------------

--Ejercicio 1

list1 :: [Int]
list1 = 1 : list1

--Ejercicio 2
list2 :: Int -> [Int]
list2 n = n : list2 (n + 1)

--Ejercicio 3 
primerosN :: Int -> [Int]
primerosN n = take n (list2 0)

--Ejercicio 4
primeros5 :: [Int]
primeros5 = take 5 (list2 0)

--Ejercicio 5
cuadrados :: [Int] -> [Int]
cuadrados = map (^2) --map funcion [1,2,3] --> [funcion(1),funcion(2), funcion(3)] (pensar como una distribucion)

--Ejercicio 6 
    --metodo 1 (función lambda)
divisoresPrueba :: Int -> [Int]
divisoresPrueba n = filter (\x -> n `mod` x == 0) [1..n]

    --metodo 2 (función auxiliar)
esDivisor :: Int -> Int -> Bool
esDivisor divisor numero = (divisor `mod` numero) == 0  --

divisores :: Int -> [Int]
divisores n = filter (esDivisor n) [1..n]

    --metodo 3 (where)
divisoress :: Int -> [Int]
divisoress n = filter p [1..n] where p x = mod n x == 0

--Ejercicio 7
numPrim :: Int -> Bool
numPrim n = (n > 1) && null [x | x <- [2..n - 1], n `mod` x == 0]

listaprimos :: [Int] -> [Int]
listaprimos = filter numPrim

--Ejercicio 8
listaNaturales :: [Int] -> Int
listaNaturales xs= sum  (map (^2) xs )

--Ejercicio 9 
listaNaturales2 :: [Int] -> [Int]
listaNaturales2 xs = map (succ) xs

--Ejercicio 10
sumEnteros :: [Int] -> Int
sumEnteros = foldl (+) 0 

--Ejercicio 11
factorial :: Int -> Int
factorial n = foldr (*) 1 [1..n]

--Ejercicio 12
ands :: [Bool] -> Bool
ands = foldr (&&) True

--Ejercicio 13
tam :: [a] -> Int
tam xs = foldl (\acumulador i -> acumulador + 1) 0 xs

    --otra manera
incrementar :: Int -> a -> Int
incrementar acumulador _ = acumulador + 1

tam1 :: [a] -> Int
tam1 xs = foldl incrementar 0 xs

--Ejercicio 14
listaSuccesores :: [Int] -> [Int]
listaSuccesores xs = [x + 1| x <- xs]

--Ejercicio 15
listaCuadrados :: [Int] -> [Int]
listaCuadrados xs = [x^2 | x <-xs]

--Ejercicio 16
listaPares :: [Int] -> [Int]
listaPares xs = [x | x <- xs, x > 10 && even x]

--Ejercicio 17
listDiv :: Int -> [Int]
listDiv num = [x | x <- [1..num], (num `mod` x) == 0  ]

--Ejercicio 18
todosOcurrenEn :: (Eq a) => [a] -> [a] -> Bool    
todosOcurrenEn xs ys = null [x | x<- xs, x `notElem` ys] 

--Ejercicio 19
esPrimo :: Int -> Bool
esPrimo n | n < 2 = False
          | otherwise = all (\x -> n `mod` x /= 0) [2..intSqrt n]
          where intSqrt = floor . sqrt . fromIntegral
--intSqrt = toma un numero entero y devuelve la parte entera de su raiz cuadrada

    --otra manera
numPrimitos :: Int -> [Int]
numPrimitos n = [x | x <- [2..n], esPrimo x ]

--Ejercicio 20
cartesiano :: [Int] -> [Int] -> [(Int,Int)]
cartesiano xs ys = [(x,y)  | x <- xs, y <- ys]

--Ejercicio 21
numOcurrencia :: (Eq a) => [a] -> a -> Int
numOcurrencia ys x = length [ y | y <- ys, y == x ]

--Ejercicio 22
    --modo 2
split2 :: [a] -> [([a], [a])]
    --manera mas corta: split3 xs = [ (take i xs, drop i xs) | i <- [0..length xs] ]
split2 xs = [ (x,y) |n <- [0 ..length(xs)],  x <- [take n (xs)], y <- [drop n (xs)]]

    --funcionamiento:
    --dado [1,2,3]
    --xs = [1,2,3]
    --n <- [0..length(xs)] = n <- [0..length([1,2,3])] = n <- [0..3] {itera con n = 0}

    -- x <- take 0 ([1,2,3]), y <- drop 0 ([1,2,3])
    --      x <- []         ,    y <- [1,2,3]

    -- (x,y) = ([], [1,2,3])

    --y luego con n = 1, n = 2, y n = 3

    -- total: [([],[1,2,3]), ([1],[2,3]), ([1,2],[3]), ([1,2,3], [])]
    --              n = 0  ,   n = 1    ,   n = 2    ,   n = 3

--Ejercicio 23
sumaSeg :: [Int] -> Int
sumaSeg xs = sum [sum (take n xs) | n <- [1..length xs]]

--Ejercicio 24
pares :: [Int]
pares = [2,4..]

