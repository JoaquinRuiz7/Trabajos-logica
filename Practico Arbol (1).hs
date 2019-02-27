--Practico listas
--Arbol vacio ( data BinTree a = Empty | node a (BinTree a) (BinTree a))
-- t :: BinTree Int
-- t = Node 10 (Node 3 Empty Empty) (Node 14 (Node 5 Empty Empty) (Empty))
data BinTree a = Empty
				|Node a (BinTree a) (BinTree a)
 deriving (Show)
 
t :: BinTree Int
t = Node 10 (Node 3 Empty Empty) (Node 14 (Node 5 Empty Empty) (Empty))
--Funcion que da el tama;o de un arbol
size :: BinTree a -> Int
size Empty = 0
size (Node x izq der) = 1 + size izq + size der
--Funcion que devuelve la suma de los nodos de un arbol
suma :: BinTree Int -> Int
suma Empty = 0
suma (Node x izq der) = x + suma izq + suma der
--Funcion que te dice si todos los nodos cumplen un predicado
todos :: (a -> Bool) -> BinTree a -> Bool
todos p Empty = True
todos p (Node x izq der)
	|p x = (todos p izq) && (todos p der)
	|not(p x) = False
	
--Funcion que te dice si alguno de los nodos cumple con el predicado
alguno :: (a->Bool) -> BinTree a -> Bool
alguno p Empty = False
alguno p (Node x izq der)
	|p x = (alguno p izq) || (alguno p der)
	|not(p x) = False
--Funcioin que devuelve el largo de un arbol
altura :: BinTree a -> Int
altura Empty = 0
altura (Node x izq der) = 1 + (maxm (altura izq) (altura der))
--Funcion que devuelve el numero maximo entre dos numeros
maxm :: Int -> Int -> Int
maxm 0 0 = 0
maxm m n
	|m >= n = m
	|m < n = n
--Funcion que dado un arbol y un numero devuelve true si el numero se encuentra dento del arbol
pertenece :: BinTree Int -> Int -> Bool
pertenece Empty n = False
pertenece (Node x izq der) n
		|n == x = True
		|n /= x = (pertenece izq n) || (pertenece der n)
--Un arbol ordenado es aquel que dado un nodo 
--a su izquierda tendra numeros menores y a la derecha numeros mayores

--Pertenece pero con un arbol ordenado
pertOrd :: BinTree Int -> Int -> Bool
pertOrd Empty n = False
pertOrd (Node x izq der) n
	|n == x = True
	|n > x = pertOrd der n
	|n < x = pertOrd izq n
--Funcion que une dos listas
unirL :: [a] -> [a] -> [a]
unirL [] ys= ys
unirL (x:xs) ys = x:(unirL xs ys)
	
--Funcion que hace una lineacion de un arbol(transforma un arbol en una lista)
-- x izq der
linea :: BinTree a -> [a]
linea Empty = []
linea (Node x izq der) = x : ( unirL (linea izq) (linea der))
--izq x der
lineab :: BinTree a -> [a]
lineab Empty = []
lineab (Node x izq der) = ( unirL (lineab izq) (x:lineab der))
--izq der x
lineac :: BinTree a -> [a]
lineac Empty = []
lineac (Node x izq der) = unirL ( unirL (lineac izq) (lineac der)) [x]

data OpTree = Num Int
			|Neg OpTree
			|Sum OpTree OpTree
			|Prod OpTree OpTree
 deriving (Show)
 
b :: OpTree
b = Prod(Num 6) (Neg (Sum (Num 11) (Num 2)))

c :: OpTree
c = Prod(Num 6) (Neg(Neg (Sum (Num 11) (Num 2))))

--Funcion que devuelve el numero de constantes de un OpTree
cantConst :: OpTree -> Int
cantConst (Num n)= 1
cantConst (Neg e)= cantConst e
cantConst (Sum e1 e2)= cantConst e1 + cantConst e2
cantConst (Prod e1 e2)= cantConst e1 + cantConst e2

--Funcion que devuelve la cantidad de operaciones de un OpTree
cantOp :: OpTree -> Int
cantOp (Num n) = 0
cantOp (Neg e) = cantOp e
cantOp (Sum e1 e2) = 1+ cantOp e1 + cantOp e2
cantOp (Prod e1 e2) = 1 + cantOp e1 + cantOp e2

--Funcion que resuelve un OpTree
evaluacion :: OpTree -> Int
evaluacion (Num n) = n
evaluacion (Neg e) = - evaluacion e
evaluacion (Sum e1 e2) = evaluacion e1 + evaluacion e2
evaluacion (Prod e1 e2) = evaluacion e1 * evaluacion e2

--Elimina los doble negativos del arbol
dobleNeg :: OpTree -> OpTree
dobleNeg (Num n) = Num n
dobleNeg (Neg (Neg e)) = dobleNeg e
dobleNeg (Neg e) = Neg (dobleNeg e)
dobleNeg (Sum e1 e2) = Sum (dobleNeg e1) (dobleNeg e2)
dobleNeg (Prod e1 e2) = Prod (dobleNeg e1) (dobleNeg e2)

{--Para todo cantNodos t >= altura t
caso base t = Empty
cant nodos empty >= altura empty
=(def cant nodos)
0 >= altura Empty
=(def altura)
0>= 0 caso base cumple
Fin Caso base

paso inductivo
h1) cantNodos izq >= altura izq
h2) cantNodos der >= altura der 
t)cantNodos (Node x izq der) >= altura (Node x izq der)
(def cantNodos)
1+ cantNodos izq + cantNodos der >= altura(Node izq der)
(def altura)
1+ cantNodos izq + cantNodos der >= 1 + max(altura izq) (altura der)
(por h1 y h2)
1+ altura izq + altura der >= 1 + max (altura izq) (altura der)
(a+b >= max a b)

Fin paso inductivo
Fin caso

Ejercicio 1) a- cantVacios, que computa la cantidad de 
arboles vacios contenidos en un arbol dado

b- cantVacios t = 1+ cantNodos t

--}
cantVacios :: BinTree a -> Int
cantVacios Empty = 1
cantVacios (Node x der izq) = cantVacios izq + cantVacios der
{-- 
cantVacios t = 1 + cantNodos t
caso base : t = empty
cantVacios t = 1 + cantNodos t
(def cantVacios)
1 = cantNodos t
(Def CantNodos)
1 = 1 + 0
fin caso base, cumple

Paso Inductivo
h1) cantVacios izq = 1 + cantNodos izq
h2) cantVacios der = 1 + cantNodos der
t) cantVacios (Node x izq der) = 1 + cantNodos (Node x izq der)
(Definicion cantVacios)
cantVacios izq + cantVacios der = 1 + cantNodos (Node x izq der)
(Definicion cantNodos)
cantVacios izq + cantVacios der = 1 + 1 + cantNodos izq + cantNodos der
(por h1 y h2)
1 + cantNodos izq + 1+ cant Nodos der = 1 + 1 + cantNodos izq + cantNodos der
(Por conmutativa de la suma)
1 + 1 + cantNodos izq + cantNodos der = 1 + 1 + cantNodos izq + cantNodos der  
Fin paso inductivo

----------------------------------Metodo de induccion de arboles---------------------------------------------
data OpTree = Num Int  (Caso base)
			  Neg OpTree (1h)         |
			  Sum OpTree Optree (2h)  | 3 Pasos inductivos
			  Prod OpTree OpTree (2h) |
			  
Caso base t = Num n
P(Num N)

Paso inductivo t= Neg e
H) P e
T) P (Neg e)

Paso Inductivo t= Sum e1 e2
H1) Pe1
H2) Pe2
T) P(Sum e1 e2)

Paso Inductivo t= Prod e1 e2
H1) Pe1
H2) Pe2
T)  P(Prod e1 e2)
-----------------------------------------Fin Metodo-------------------------------------------------------------
1- Programar espejo, que computa la imagen espejo de un arbol de tipo OpTree.

2- Programar eval de un arbol de OpTree (Ya lo hice)

3- Demostrar que:
a- espejo(espejo t) = t
b- eval (espejo t) = eval t

--}
opEspejo :: OpTree -> OpTree
opEspejo (Num n) = Num n
opEspejo (Neg e) = Neg (opEspejo e)
opEspejo (Sum e1 e2) = Sum (opEspejo e2) (opEspejo e1)
opEspejo (Prod e1 e2) = Prod (opEspejo e2) (opEspejo e1) 
{--
3- espejo(espejo t) = t

Caso base t = num n

espejo(espejo num n) = num n
(definicion espejo)
espejo(num n) = num n
(Definicion espejo
num n = num n
FIN CASO BASE

Paso inductivo 1 t= Neg e
H)espejo(espejo e) = e
T)espejo (espejo Neg e) = Neg e
(Definicion espejo)
espejo(Neg(espejo e))= Neg e
(Definicion espejo)
Neg(espejo(espejo e) = Neg e
(Aplicamos hipotesis)
Neg e = Neg e

Paso inductivo 2 t= Sum izq der
h1)espejo(espejo izq) = izq
h2)espejo(espejo der) = der
T) espejo(espejo Sum izq der) = Sum izq der
(Definicion espejo)
espejo(Sum (espejo der) (espejo izq)) = Sum izq der
(definicion espejo)
Sum (espejo(espejo izq) espejo(espejo der)) = sum izq der
(Aplicamos hipotesis dos veces(izq der))
Sum izq der = Sum izq der

Paso inductivo 3 t= Prod izq der
h1)espejo(espejo izq) = izq
h2)espejo(espejo der) = der
T) espejo(espejo Prod izq der) = Prod izq der
(Definicion espejo)
espejo(Prod (espejo der) (espejo izq)) = Prod izq  der
(definicion espejo)
Prod (espejo(espejo izq) espejo(espejo der)) = Prod izq der
(Aplicamos hipotesis dos veces(izq der))
Prod izq der = Prod izq der

{--
evaluacion :: OpTree -> Int
evaluacion (Num n) = n
evaluacion (Neg e) = - evaluacion e
evaluacion (Sum e1 e2) = evaluacion e1 + evaluacion e2
evaluacion (Prod e1 e2) = evaluacion e1 * evaluacion e2
--}
3-b) eval(espejo t) = eval t
Caso base t= num n
eval(espejo Num n) = eval (Num n)
(Definicion espejo)
eval(Num n) = eval (Num n)

Paso inductivo 1  t = Neg e
h)eval(espejo e) = eval (e)
T)eval(espejo Neg e) = eval (Neg e)
(Definicion espejo)
eval(Neg(espejo e)) = eval (Neg e)
(definicion eval(doble))
- eval (espejo e) = - eval e
(hipotesis)
- eval e = -eval e
Cumple
FIN PASO INDUCTIVO 1

Paso inductivo 2 t = sum e1 e2
h1)eval(espejo e1)= eval e1
h2)eval (espejo e2) = eval e2
T) eval(espejo(sum e1 e2)) = eval (sum e1 e2)
(aplicamos definicion espejo)
eval(sum (espejo e2) (espejo e1)) = eval(sum e1 e2)
(definicion eval)
eval(espejo e2) + eval(espejo e1) = eval e1 + eval e2
(aplico hipotesis)
eval e2 + eval e1 = eval e1 + eval e2
(conmutativa de la suma)
eval e1 + eval e2 = eval e1 + eval e2

Idem producto(despues escribir)
Paso inductivo 3 t = prod e1 e2
h1)eval(espejo e1)= eval e1
h2)eval (espejo e2) = eval e2
T) eval(espejo(Prod e1 e2)) = eval (prod e1 e2)
(aplicamos definicion espejo)
eval(prod (espejo e2) (espejo e1)) = eval(prod e1 e2)
(definicion eval)
eval(espejo e2) * eval(espejo e1) = eval e1 * eval e2
(aplico hipotesis)
eval e2 * eval e1 = eval e1 * eval e2
(conmutativa del producto)
eval e1 * eval e2 = eval e1 * eval e2

CUMPLE, FIN EJERCICIO


--}