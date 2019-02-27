{--
1-
Ma (Mi(Num 2)(Ma(Num 4)(Num 3))) (Num 1)
--}
data AB = Num Int | Ma AB AB | Mi AB AB
        deriving (Show) 
valor :: AB -> Int
valor (Num n) = n
valor (Ma t1 t2) = max (valor t1) (valor t2)
valor (Mi t1 t2) = min (valor t1) (valor t2)
fijar :: AB -> Int -> AB
fijar (Num n) x = Num x
fijar (Ma (t1) (t2)) n = Ma (fijar t1 n) (fijar t2 n)
fijar (Mi (t1) (t2)) n = Mi (fijar t1 n) (fijar t2 n)
 ---No me salio la demostracion .