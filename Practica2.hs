--Radilla Maldonado Dylan Emmanuel

--Tablas de verdad
 
 --Función disyunción.
 disyuncion :: Bool -> Bool -> Bool
 disyuncion False False = False
 disyuncion p q = True
 
 --Función conjunción.
 conjuncion :: Bool -> Bool -> Bool
 conjuncion True True = True
 conjuncion p q = False

 --Función implicación.
 implicacion :: Bool -> Bool -> Bool
 implicacion True False = False
 implicacion p q = True

 --Función bicondicional.
 dobleImplica :: Bool -> Bool -> Bool
 dobleImplica True True = True
 dobleImplica False False = True
 dobleImplica p q = False

--Listas

 {-Función que recibe una lista y calcula el numero
 de elementos en la misma.-}
 longitud :: [a] -> Int
 longitud [] = 0
 longitud (x:xs) = 1 + longitud (xs)

 {-Función que recibe una lista de numeros y devuelve
 la suma de ellos-}
 sumaNumeros :: Num a => [a] -> a
 sumaNumeros [] = 0
 sumaNumeros [x] = x
 sumaNumeros (x:xs) = x + sumaNumeros (xs)

 {-Función recibe una lista de valores numericos y devuelve
 el valor máximo.-}
 maximo :: Ord a => [a] -> a
 maximo [] = error "La lista es vacia"
 maximo [x] = x
 maximo (x:xs) = if x > maximo xs then x else maximo xs

 {-Función que recibe como parametros un indice i y una lista,
 la función devuelve el elemento de la lista en la posición i.-}
 indiceDe :: Int -> [a] -> a
 indiceDe n [] = error "La lista es vacia"
 indiceDe 0 (x:xs) = error "No hay elemento 0"
 indiceDe 1 (x:xs) = x
 indiceDe n (x:xs) | n > longitud (x:xs) = error "El elemento que buscas no está en la lista"
                   | n <= longitud (x:xs) = indiceDe (n-1) xs

 {-Función que recibe un elemento, una lista y un booleano,
 posteriormente añade dicho elemento a la lista, si boolean 
 es True lo añade al inicio, si es False al final.-}
 insertarElemento :: a -> [a] -> Bool -> [a]
 insertarElemento y [] b = [y]
 insertarElemento y x b | b == True = y:x
                        | b == False = x ++ [y]

{-Función que al recibir una lista o cadena y te dice si es 
o no un palíndromo-}
 esPalindromo :: [a] -> Bool
 esPalindromo [] = error "La lista es vacia"
 esPalindromo [x] = True
 esPalindromo (x:xs) = False

{-Función que invierte el orden de la lista.-}
 reversa :: [a] -> [a]
 reversa [] = []
 reversa [x] = [x]
 reversa(x:xs) = insertarElemento x (reversa (xs)) False

{-Función que recibe una lista y elimina las repeticiones
de elementos-}
 aConjunto ::(Eq a) => [a] -> [a]
 aConjunto [] = []
 aConjunto (x:xs) = [x] ++ aConjunto [y | y <- (xs), y /= x]

 {-Función que calcula la unión de dos listas.-}
 union :: (Eq a) => [a] -> [a] -> [a]
 union [] [] = []
 union x y = aConjunto (x++y)

 {-Función que calcula la intersección de dos listas.-}
 interseccion ::(Eq a) => [a] -> [a] -> [a]
 interseccion [] [] = []
 interseccion [x] [] = []
 interseccion [x] [y] = if x==y then [x] else []
 interseccion (x:xs) (y:ys) = if x == y then aConjunto ([x] ++ interseccion (xs) (ys)) 
                                else interseccion (xs) (ys)

 {-Función que calcula el producto cruz de dos listas.-}
 productoCruz ::(Num a) => [a] -> [a] -> [a]
 productoCruz [] [] = []
 productoCruz [x] [y] = [x*y]
 productoCruz l (y:ys)  = [x*y | x<-l, y<-(y:ys)]

 {-Función que calcula la diferencia simétrica de
 dos listas.-}
 diferenciaSimetrica :: (Ord a) => [a] -> [a] -> [a]
 diferenciaSimetrica [] [] = []
 diferenciaSimetrica [x] [] = [x]
 diferenciaSimetrica (x:xs) (y:ys) = [x | x <- (x:xs), notElem x (y:ys)] ++ [y | y <- (y:ys), notElem y (x:xs)] 


 --Punto Extra
 {-Función que calcula los divisores de un numero entero.-}
 divisores :: Int -> [Int]
 divisores 0 = []
 divisores x = [y | y <- [1..x], x `mod` y == 0]

 {-Función que recibe una lista y calcula el conjunto potencia.-}
 --conjuntoPotencia :: (Eq a) => [a] -> [a]
 --conjuntoPotencia [] = []
 --conjuntoPotencia (x:xs) = (x:xs)