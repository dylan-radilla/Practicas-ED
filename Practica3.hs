--Radilla Maldonado Dylan Emmanuel

module Practica3 where

data Var = A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z deriving (Show, Eq, Ord)

data Formula = Prop Var
             |Neg Formula
             |Formula :&: Formula
             |Formula :|: Formula
             |Formula :=>: Formula
             |Formula :<=>: Formula deriving (Show, Eq, Ord)

infixl 9 :&:
infixl 9 :|:
infixr 7 :=>:
infixl 8 :<=>:

{-Función que recibe una lista y elimina las repeticiones
de elementos-}
aConjunto ::(Eq a) => [a] -> [a]
aConjunto [] = []
aConjunto (x:xs) = [x] ++ aConjunto [y | y <- (xs), y /= x]

varList :: Formula -> [Var]
varList (Prop s) = [s]
varList (Neg fs) = varList fs
varList (fs :&: gs) = aConjunto ((varList fs) ++ (varList gs))
varList (fs :|: gs) = aConjunto ((varList fs) ++ (varList gs))
varList (fs :=>: gs) = aConjunto ((varList fs) ++ (varList gs))
varList (fs :<=>: gs) = aConjunto ((varList fs) ++ (varList gs))

negar :: Formula -> Formula
negar (Prop f) = Neg (Prop f)
negar (Neg fs) = (fs)
negar (fs :&: gs) = (negar (fs) :|: negar (gs))
negar (fs :|: gs) = (negar (fs) :&: negar (gs))
negar (fs :=>: gs) = (negar (Neg fs) :|: negar (gs))
negar (fs :<=>: gs) = (negar (fs :=>: gs) :|: negar (gs :=>: fs))

equivalencia :: Formula -> Formula
equivalencia (Prop f) = (Prop f)
equivalencia (Neg fs) = negar (equivalencia fs) 
equivalencia (fs :&: gs) = (equivalencia(Neg fs) :|: equivalencia(Neg gs))
equivalencia (fs :|: gs) = (equivalencia(Neg fs) :&: equivalencia(Neg gs))
equivalencia (fs :=>: gs) = (equivalencia(Neg fs) :|: equivalencia(gs))
equivalencia (fs :<=>: gs) = (equivalencia(fs :=>: gs) :&: equivalencia(gs :=>: fs))

buscarVar :: Var -> [(Var,Bool)] -> Bool
buscarVar x [] = error ("No están definidas todas las variables")
buscarVar x ((y,b):ys) = if x == y then b
                         else buscarVar x ys

interp :: Formula -> [(Var,Bool)] -> Bool
interp (Prop f) vf = (buscarVar f vf)
interp (Neg fs) vs = not (interp fs vs)
interp (fs :&: gs) vs = (interp fs vs) && (interp gs vs)
interp (fs :|: gs) vs = (interp fs vs) || (interp gs vs)
interp (fs :=>: gs) vs = not (interp fs vs) || (interp gs vs)
interp (fs :<=>: gs) vs = (interp fs vs) == (interp gs vs)

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud (xs)

replicateM :: Monad m => Int -> m a -> m [a] 
replicateM n x = sequence (replicate n x)

combinaciones :: Formula -> [[(Var,Bool)]]
combinaciones fs = let vs = varList fs
                       ps = replicateM (longitud vs) [True, False]
                   in map (zip vs) ps
 
--tablaVerdad :: Formula -> [[(Var,Bool)]]
--tablaVerdad (Prop f) = (Prop f)
--tablaVerdad (Neg fs) = combinaciones (Neg fs)
--tablaVerdad (fs :&: gs) = combinaciones (fs :&: gs)
--tablaVerdad (fs :|: gs) = combinaciones (fs :|: gs)
--tablaVerdad (fs :=>: gs) = combinaciones (fs :=>: gs) 
--tablaVerdad (fs :<=>: gs) = combinaciones (fs :<=>: gs) 