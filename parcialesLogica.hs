--Parcial ensayo 1

cantidadVeces :: Int -> [Int] -> Int
cantidadVeces n [] = 0
cantidadVeces n ( x:xs )
            | ( n == x ) = 1 + cantidadVeces n xs 
            | not ( n == x ) = cantidadVeces n xs

saltearIguales :: Int -> [Int] -> [Int]
saltearIguales  n [] = []
saltearIguales n (x:xs) 
            | ( n  == ( head xs ) ) = saltearIguales n xs
            | not ( n  == ( head xs ) ) = xs

cantidadVecesLista :: [Int] -> [(Int,Int)]
cantidadVecesLista [] = []
--cantidadVecesLista ( x:xs ) = ( map ( ( x,cantidadVeces x ( x:xs ) ) ) ( saltearIguales xs )  )
 
prefijo :: [Int] -> [Int] -> Bool
prefijo [] []  = True
prefijo [] (x:xs) = True
prefijo ( x : xs ) ( y:ys )
            | ( x == y ) = prefijo xs ys 
            | not ( x == y ) = False

fix :: Int -> [Int] -> [Int]
fix n [] = []
fix n ( x:xs ) = n: ( fix  n xs )

largoLista :: [ Int ] -> Int
largoLista [] = 0
largoLista ( x:xs ) = 1 + largoLista xs 

--Falta la parte de arboles




-- Parcial 2

map2 :: ( a -> b ) -> [a] ->[b]
map2 f [] = []
map2 f ( x:xs ) = ( f x ): map2 f xs

and2 :: [Bool] -> Bool
and2 [] = True
and2 ( x :xs ) 
            | ( x ) = x && and2 xs
            | not ( x ) = False

paraTodos :: ( a -> Bool ) -> [a] -> Bool
paraTodos p [] = True
paraTodos p ( x:xs )
            | p x = and (map ( p ) (x:xs))
            | not ( p x ) = False

posicionDeN :: Int -> [Int] -> Int
posicionDeN n [] = 0
posicionDeN n ( x:xs ) 
            | n == x = 0
            | not ( n == x ) = 1 + posicionDeN n xs

 {--       
data Sum = Num Int | Suma (Num Int) (Num Int)
    deriving (Eq,Show)

eval :: Sum -> Int
eval (Num n) = n
eval Suma ( n ) ( n2 ) = eval n + eval n2 

reemplazar :: Int -> Sum -> Sum
reemplazar n (Num x) = Num n
reemplazar n ( Suma ( t1 ) ( t2 ) ) = Suma (reemplazar n t1) (reemplazar n t2)
--}

-- Parcial 4

eliminar :: Int -> [Int] -> [Int]
eliminar x [] = []
eliminar y ( x:xs ) 
        | y == x = eliminar y xs 
        | not ( y == x ) = x: eliminar y xs
        
eliminar2 :: (Int -> Bool ) -> [Int] ->[Int]
eliminar2 p [] = []
eliminar2 p (x:xs) 
        | p x = eliminar2 p xs
        | not ( p x ) = x:eliminar2 p xs

sumaPrimerosImpares :: Int -> Int
sumaPrimerosImpares 0 = 0
sumaPrimerosImpares n = ( ( n*2 ) - 1 ) + sumaPrimerosImpares(n-1) 

data Arb a = Vacio | Nodo a (Arb a) (Arb a)
                deriving (Eq,Show)

listaAArbol :: [a] -> Arb a
listaAArbol [] = Vacio
listaAArbol (x:xs) = Nodo x ( listaAArbol xs) ( listaAArbol xs)

ramaDerecha :: Arb a -> [a]
ramaDerecha ( Vacio ) = []
ramaDerecha ( Nodo x ( t1 ) ( t2 ) ) = x : (ramaDerecha t2) 

--Parcial 5

listLenght :: [a] -> Int
listLenght [] = 0
listLenght (x:xs) = 1 + listLenght xs

cuales::(a -> Bool) -> [a] -> [a]
cuales p [] = []
cuales p (x:xs) 
        | p x = x:cuales p xs
        | not (p x) = cuales p xs
cuantos :: ( a-> Bool ) -> [a] -> Int
cuantos p [] = 0
cuantos p (x:xs) 
        | p x = 1 +cuantos p xs
        | not ( p x ) = cuantos p xs
-- Pre : el primer numero ingresado debe ser mayor al segundo
sumaCondicionEntre :: Int -> Int -> ( Int -> Bool ) -> Int
sumaCondicionEntre n1 n2 p
        | (n1 < n2 ) = 0
        | ( ( n1 >= n2 ) && ( p n1 )) = n1 + sumaCondicionEntre ( n1 - 1 ) n2 p
        | not ( ( ( n1 >= n2 ) && ( p n1 ) ) ) = sumaCondicionEntre ( n1 - 1 ) n2 p

data Fichero = Imagen String | Sonido String | Carpeta1 Fichero | Carpeta2 Fichero Fichero
        deriving(Eq,Show)

cantSonidos :: Fichero -> Int
cantSonidos ( Imagen s ) = 0
cantSonidos ( Sonido s ) = 1
cantSonidos (Carpeta1 f ) = cantSonidos f
cantSonidos ( Carpeta2 f1 f2) = cantSonidos f1 + cantSonidos f2

listarSonidos :: Fichero -> [String]
listarSonidos ( Imagen s ) = []
listarSonidos ( Sonido s ) = s:[]
listarSonidos ( Carpeta1 f) = listarSonidos f 
listarSonidos (Carpeta2 f1 f2 ) = listarSonidos f1 ++ listarSonidos f2       

--Parcial 6
existe :: (a->Bool)->[a]->Bool
existe p [] = False
existe p (x:xs) 
        | p x = True
        | not ( p x ) = existe p xs
ninguno :: (a->Bool)->[a]->Bool
ninguno p [] = True
ninguno p (x:xs) 
        | p x = False
        | not ( p x ) = ninguno p xs
ninguno2 :: (a->Bool)->[a]->Bool
ninguno2 p l = ninguno p l

takeFrom :: Int ->[Int]->[Int]
takeFrom n [] = []
takeFrom n (x:xs) 
        | n == x = x : xs
        | not ( x == n ) = takeFrom n xs
        
data AndT = B Bool | And (AndT) (AndT) 
        deriving(Eq,Show)

eval2 :: AndT -> Bool
eval2 (B b) = b
eval2 (And t1 t2) = ( eval2 t1 ) && ( eval2 t2 )
aTrue :: AndT -> AndT
aTrue ( B b ) = ( B True )
aTrue ( And ( t1 ) ( t2 ) ) = And (aTrue t1) ( aTrue t2)
espejo :: AndT -> AndT
espejo (B b) = B b 
espejo (And (t1) (t2)) = And (espejo t2) ( espejo t1)
 
par :: Int -> Bool
par 0 = True
par n 
        | mod n 2 == 0 = True
        | not ( mod n 2 == 0 )= False

