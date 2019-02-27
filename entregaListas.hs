-- Recursion en listas--
insert :: Ord a => a -> [a] -> [a]
insert n []  = n:[]
insert n (x:xs)  
    | n > x = x:insert n (xs) 
    | not ( n > x ) = n : (x:xs)
insertSort ::  Ord a => [a] -> [a]
insertSort [] = []
insertSort (x:xs)  = insert x (insertSort xs)
primerElemento :: [a] -> a
primerElemento [] = error " Lista vacia "
primerElemento (x:xs) = x
minElem :: Ord a => [a] -> a
minElem [] = error "Se debe ingresar una lista NO vacia"
minElem (x:xs) = primerElemento (insertSort (x:xs))
borrarPri :: Eq a => a -> [a] -> [a]
borrarPri a [] = error " La lista esta vacia "
borrarPri a (x:xs) 
    | not ( a == x) = x:borrarPri a xs
    | (a == x) =  xs
selectSort :: Ord a => [a] -> [a]
selectSort [] = []
selectSort (x:xs) = (minElem (x:xs)) : selectSort (borrarPri (minElem(x:xs)) (x:xs)) 

-- Induccion en Listas --
duplicar :: [a] -> [a]
duplicar [] = []
duplicar (x:xs) = x:x:duplicar xs