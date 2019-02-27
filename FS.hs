module FS where 

import Prelude

-- Nombre y número: Joaquin Ruiz 206164
-- Nombre y número: Francisco Lopez 209003

type Nombre = String 

data FS = A Nombre | Dir Nombre [FS]
  deriving (Eq, Show)

----
-- 1
djazz :: FS
djazz = Dir "jazz" [A "mumbles.mp3"]

drock :: FS
drock = Dir "rock" [A "clones.mp3", A "bajan.mp3", A "clara.mp3"]

dmusica :: FS
dmusica = Dir "musica"[djazz, drock, A "clara.mp3"]

-- Completar el resto de los componentes del FS
dobls :: FS
dobls = Dir "Obls" [A "p2.txt" , A "p2.jar" , A "fc.hs"]

dort :: FS
dort = Dir "ort" [dobls , A "notas.txt"] 

dsys2 :: FS
dsys2 = Dir "sys" []

dsys :: FS
dsys = Dir "sys" [A "sys.txt" , dsys2]


draiz :: FS
draiz = Dir "raiz" [dmusica , A "notas.txt" , dort , dsys]

----
-- 2
nombre :: FS -> Nombre
nombre ( A name ) = name
nombre ( Dir ( n ) ( x:xs ) ) = n
nombre ( Dir ( n ) [] ) = n
----
contenido2 :: [FS] -> [Nombre]
contenido2 [] = []
contenido2 ( x:xs ) = [nombre x] ++ contenido2 xs
-- 3
contenido :: FS-> [Nombre]
contenido ( Dir ( name ) [] ) = [name]
contenido ( Dir ( name ) ( x:xs ) ) = [nombre x] ++ contenido2 xs
contenido ( A name ) = error "no es un directorio"
----
cantA2 :: [FS] -> Int
cantA2 [] = 0
cantA2 ( x:xs ) = cantA x + cantA2 xs
-- 4
cantA :: FS -> Int 
cantA ( A name ) = 1
cantA ( Dir ( name ) [] ) = 0
cantA ( Dir ( name ) ( x:xs ) ) = cantA x + cantA2 xs
----
cantD2 :: [FS] -> Int
cantD2 [] = 0
cantD2 (x:xs) = cantD x + cantD2 xs
-- 5
cantD :: FS -> Int 
cantD ( A name ) = 0
cantD ( Dir ( name ) [] ) = 1
cantD ( Dir ( name ) ( x:xs ) ) = 1 + cantD x + cantD2 xs
----
belongs :: Nombre -> [FS] -> Bool
belongs name [] = False
belongs name ( x:xs )
        | name == ( nombre x ) = True
        | not ( name == ( nombre x ) ) = pertenece name x || belongs ( name ) ( xs )
-- 6
pertenece :: Nombre -> FS -> Bool 
pertenece name ( A name2 ) 
              | name == name2 = True
              | not( name == name2 ) = False
pertenece name ( Dir ( name2 ) [] )
              | name == name2 = True
              | not ( name == name2 ) = False 
pertenece name ( Dir ( name2 ) ( x:xs) )
              | name == name2 = True
              | not ( name == name2 ) = ( pertenece ( name ) ( x ) || belongs ( name ) ( xs ) )
----
-- 7
valido :: FS -> Bool
valido  ( A name ) = True
valido ( Dir ( name ) ( [] ) )= True
valido ( Dir ( name ) ( x:xs ) ) =  not ( pertenece (nombre x) ( Dir ( name ) ( xs ) ) ) 
----
cambiarNomAux :: Nombre -> Nombre -> [FS] -> [FS]
cambiarNomAux name1 name2 [] = []
cambiarNomAux name1 name2 (x:xs) = [cambiarNom name1 name2 x] ++ cambiarNomAux name1 name2 xs
-- 8
cambiarNom :: Nombre -> Nombre -> FS -> FS 
cambiarNom name1 name2 ( A name ) 
          | name1 == name = ( A name2 )
          | not ( name1 == name ) = ( A name )
cambiarNom name1 name2 ( Dir name [] )
          | name1 == name = ( Dir name2 [])
          | not ( name1 == name ) = ( Dir name [])
cambiarNom name1 name2 ( Dir name ( x:xs ) )
          | name1 == name = ( Dir name2 ( cambiarNomAux name1 name2 ( x:xs ) ) )
          | not ( name1 == name ) = Dir name ( cambiarNomAux name1 name2 ( x:xs ) )
----
-- 9 falta corregir
unirFS :: Nombre -> FS -> FS -> FS
unirFS name ( A name2 ) ( A name3 ) = Dir name [A name2, A name3]
unirFS name ( Dir name2 ( l1 ) ) ( Dir name3 ( l2 ) ) = Dir name ( l1 ++ l2 )
unirFs name ( Dir name2 (l1) ) ( A name3 ) = Dir name ( l1 ++ [( A name3 )])
unirFs name ( A name3 ) ( Dir name2 (l1) )  = Dir name ( ( A name3 ) : l1 )  
----


-- 10
nivelesD :: FS -> Int
nivelesD ( Dir name [] ) = 1
nivelesD ( A name ) = 0
nivelesD ( Dir name ( x:xs ) ) = 1 + maximum ( map nivelesD (x:xs) )  
----
-- 11 
borrar :: Nombre -> FS -> FS 
borrar n (Dir name (xs))
	|n == name = Dir name (xs)
	|n /= name = Dir name (borrarAux n (xs))
borrar n (A name) = A name
	
borrarAux :: String -> [FS] -> [FS]
borrarAux n [] = []
borrarAux n ((A name):xs)
	|name == n = borrarAux n xs 
	|name /= n = (A name):borrarAux n xs
borrarAux n ((Dir name xs): is)
	|name == n = borrarAux n is
  |name /= n = (Dir name (borrarAux n xs)) : borrarAux n is

cantdy:: Nombre -> FS -> Int
cantdy n ( A name ) 
  | n == name = 1
  | not ( n == name ) = 0
cantdy n ( Dir name [] ) 
  | n == name = 1
  | not ( n == name ) = 0
  cantdy n ( Dir name (x:xs)) 
      | n == name = 1 + map (cantdy n x) x:xs 
      | not (name == n) = map ( cantdy n x ) x:xs
