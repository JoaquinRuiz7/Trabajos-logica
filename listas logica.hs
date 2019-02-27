module Enteros where 
import Prelude

productoDeListaDeEnteros :: [Int] -> Int
productoDeListaDeEnteros [] = 1
productoDeListaDeEnteros (x:xs) = x * productoDeListaDeEnteros xs
allTrue :: [Bool] -> Bool
allTrue [] = True
allTrue (x:xs) = x && allTrue xs
someTrue :: [Bool] -> Bool
someTrue [] = False
someTrue (x:xs) = x || someTrue xs
invertir :: [a] -> [a]
invertir [] = []
invertir (x:xs) = invertir xs
invertirLista :: [a] -> [a]
invertirLista [] = []
invertirLista (x:xs) = ultimoElemento (x:xs) : invertirLista (sacarUltimo (x:xs))
ultimoElemento :: [a] -> a
ultimoElemento [] = error "La lista es vacia"
ultimoElemento (x:xs)
    | listLength xs == 0 = x
    | not (listLength xs == 0) = ultimoElemento xs
sacarUltimo :: [a] -> [a]
sacarUltimo [] = []
sacarUltimo (x:xs) 
    | listLength xs == 0 = []
    | not (listLength xs == 0) = x:sacarUltimo xs
listLength :: [a] -> Int
listLength [] = 0
listLength (x:xs) = 1+listLength(xs)
duplicate :: [a] -> [a]
duplicate [] = []
duplicate (x:xs) = x:x:duplicate xs
lenghtSum :: [[a]] -> Int
lenghtSum [] = 0
lenghtSum (x:xs) = (listLength x) + lenghtSum xs
prefijo :: Eq a => [a]-> [a]-> Bool
prefijo [] [] = True
prefijo [] (x:xs) = True
prefijo (x:xs) [] = False
prefijo (x:xs) (y:ys) 
    | ( x == y ) = prefijo xs ys
    | not ( x == y ) = False
primero::[a] -> (a -> Bool) -> Int
primero [] p = 0
primero (x:xs) p 
    | p x = 1
    | not ( p x) = 1 + primero xs p 
{--fold :: (a->b->b)->b->[a]->b 
fold f cb [] = cb
fold f cb (x:xs) = f x (fold f cb xs)--}
{--append :: [a]->[a]->[a]
append [] ys--} 

