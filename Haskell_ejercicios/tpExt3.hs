--Guia Practica "Cinco soluciones cinco"

--funcion base
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

--primera solucion (uso del Where)
factoriales_1 :: Int -> [Int]
factoriales_1 n = reverse (aux n)
    where aux 0 = [1]  
          aux n = factorial n : aux (n-1)

--segunda solucion (acumuladores)
factoriales_2 :: Int -> [Int]
factoriales_2 n = reverse (aux (n+2) 0 [1])
    where 
        aux n m (x:xs)
            | n == m    = init xs  -- Elimina el último elemento de xs cuando n alcanza m.
            | otherwise = aux (n) (m+1) ((factorial m) : x : xs)


--lista por comprensión
factoriales_3 :: Int -> [Int]
factoriales_3 n = [factorial x | x <- [0..n]]

--lista por orden superior
factoriales_4 :: Int -> [Int]
factoriales_4 n = map factorial [0..n]

--con funcion scanl