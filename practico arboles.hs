data ExpArit = Num Int
    | Neg ExpArit
    | Sum ExpArit ExpArit
    | Prod ExpArit ExpArit
    deriving (Show)
eval :: ExpArit -> Int
eval (Num n) = n 
eval (Neg x) = - (eval x)
eval (Sum t1 t2) = (eval t1) + (eval t2)
eval (Prod t1 t2) = (eval t1) * (eval t2)

eliminarDN :: ExpArit -> ExpArit
eliminarDN (Num n) = Num n 
eliminarDN (Neg ( Neg t1) ) = eliminarDN t1 
eliminarDN ( Neg t1) = Neg (eliminarDN t1)
eliminarDN (Sum t1 t2 ) = Sum (eliminarDN t1) (eliminarDN t2)
eliminarDN (Prod t1 t2) = Prod (eliminarDN t1) (eliminarDN t2)
data BinTree a = Empty
    | Node a (BinTree a) (BinTree a)
     deriving (Show)
cantVacios :: BinTree a -> Int
cantVacios Empty = 1
cantVacios (Node a t1 t2) = cantVacios t1 + cantVacios t2
espejo :: ExpArit -> ExpArit
espejo (Num n) = Num n 
espejo (Neg( t )) = Neg ( espejo  t )
espejo (Sum t1 t2 ) = Sum (espejo t2) (espejo t1)
espejo (Prod t1 t2 ) = Prod (espejo t2) (espejo t1)


