alCubo :: Int -> Int
alCubo n = (n*n*n)
sumaCubos :: Int->Int
sumaCubos n
    | (n == 0) = 0
    | (n>0) = (alCubo n) + sumaCubos (n-1)
--Pre : a > b
mod2 :: Int -> Int -> Int
mod2 a b
    | a == b = 0
    | a>b = mod (a-b) b
    | b > a = b-a 
sumaImpares :: Int -> Int
sumaImpares n
    | n == 0 = 0
    | n>0 && (mod2 n 2 == 0) = sumaImpares(n-1)
    | n>0 && not(mod2 n 2 == 0) = n+sumaImpares(n-1)

sumaPrimerosPares :: Int -> Int
sumaPrimerosPares n
    | ( n == 0 ) = 0
    | ( n >= 1 ) = ( 2 * n )+sumaPrimerosPares (n-1)
sumaPrimerosImpares :: Int -> Int
sumaPrimerosImpares n
    | ( n == 0) = 0
    | ( n >= 1 ) = ( ( 2 * n ) - 1 ) +sumaPrimerosImpares (n-1)
pot2 :: Int -> Int
pot2 n 
    | ( n == 0 ) = 1
    | ( n > 0 )  = 2 * (pot2 (n-1))
sum2 :: Int -> Int
sum2 n
    | ( n == 0 ) = 0
    | ( n > 0 ) = pot2 n +sum2 (n-1)

{--
Dem :
Paso base (n = 0):
sumaPrimerosPares n = (sumaPrimerosImpares n) + n
aplico definicion en ambos lados
    0               =  0
Paso Inductivo 
Hi) sumaPrimerosPares h = (sumaPrimerosImpares h) + h
Ti) sumaPrimerosPares (h+1) = (sumaPrimerosImpares (h+1)) + (h+1)
Dem : 
sumaPrimerosPares (h+1)  = (sumaPrimerosImpares (h+1)) + (h+1)
aplico definicion                               aplico def 
( 2 * (h+1) ) + sumaPrimerosPares(h)            ( (2 * (h+1)) -1) + 
aplico hipotesis
(2 * (h+1) ) + ( sumaPrimerosImpares h )+ h

--}

