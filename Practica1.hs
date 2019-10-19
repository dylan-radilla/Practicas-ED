--Radilla Maldonado Dylan Emmanuel

--Función que recibe dos parámetros a, b y devuelve la suma de estos.
suma :: Int -> Int -> Int
suma a b = a + b

--Función que recibe dos valores a, b y devuelve la resta de a menos b.
resta :: Int -> Int -> Int
resta a b = a - b

--Función que recibe dos valores a, b y devuelve la multiplicación de estos.
multiplicacion :: Float -> Float -> Float
multiplicacion a b = a * b

--Función que recibe dos valores a,b y devuelve la división de a entre b.
division ::  Float -> Float -> Float
division a b = a / b

--Función que compara si los parámetros a y b son iguales, a mayor que b
--o b mayor que a
comparador :: Float -> Float -> Int
comparador a b = if a == b then 0
                  else if a > b then 1
                    else -1

--Función que recibe dos parámetros a, b y devuelve el primero elevado a 
--la potencia del segundo
potencia :: Int -> Int -> Int
potencia a b = a^b

--Función que compara los valores de a, b y c para devolver el maximo de estos.
maximo :: Float -> Float -> Float -> Float
maximo a b c = if a > b && a > c then a 
                else if b > c && b > a then b
                    else if c > b && c > a then c
                        else if a < b && b == c then c
                            else if b < a && a==c then a
                                else if c < a && a==b then b
                                    else a

--Función que recibe dos puntos (x1,y1), (x2,y2) y devuelve la distancia entre ellos.
distanciaPuntos :: Float -> Float -> Float -> Float -> Float
distanciaPuntos x1 y1 x2 y2 = sqrt ((x2-x1)^2 + (y2-y1)^2)

--Función que calcula la hipotenusa a partir de la base y altura de un triangulo.
hipotenusa :: Float -> Float -> Float
hipotenusa b h = sqrt(b**2 + h**2 )

--Función que calcula la pendiente de dos puntos (x1,y1), (x2,y2).
pendiente :: Float -> Float -> Float -> Float -> Float
pendiente x1 x2  y1 y2 = (division (y2 - y1) (x2 - x1))

--Función de calcula la chicharronera de una ecuación ax²+bx+c
raicesCuadraticas:: Float -> Float -> Float -> (Float,Float)
raicesCuadraticas a b c = (((-b + sqrt(b**2 - 4*a*c))/2*a),((-b - sqrt(b**2 - 4*a*c))/2*a))

--Función que calcula el volumen de una piramide regular.
--volumenPiramidal :: Float -> Float -> Float -> Int -> Float
--volumenPiramidal l h n = ((1/3)*(l**2)*h)