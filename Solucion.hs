module Solucion where
import Data.Char


--PARTE I
-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula caracter = 97 <= ord(caracter) && ord(caracter) <= 122 
                -- el 97 es la A minuscula, el 122 es la Z minuscula (las minusculas van del 97 al 122)

-- EJ 2
letraANatural :: Char -> Int
letraANatural caracter = ord(caracter) - 97

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar caracter movimiento | esMinuscula caracter == True && (ord(caracter) + movimiento) >=97 && (ord(caracter) + movimiento) <= 122 = chr(ord(caracter) + movimiento)
                              | esMinuscula caracter == True = desplazarAux caracter movimiento
                              | otherwise = caracter

desplazarAux :: Char -> Int -> Char
desplazarAux caracter movimiento | movimiento>0 && (ord(caracter) + movimiento)>122 = desplazarAux caracter (movimiento-26)
                                 | movimiento<0 && (ord(caracter) + movimiento)<97 = desplazarAux caracter (movimiento+26)
                                 | otherwise = chr(ord(caracter) + movimiento)
                                 
-- EJ 4
cifrar :: String -> Int -> String
cifrar "" _ = ""
cifrar (c:cs) movimiento = (desplazar c movimiento) : (cifrar cs movimiento) 

-- EJ 5
descifrar :: String -> Int -> String
descifrar "" _ = ""
descifrar (c:cs) movimiento = (desplazar c (-movimiento)) : (descifrar cs movimiento)

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista [] = []
cifrarLista lista = cifrarListaAux lista 0 
            --el primer elemento tiene posicion cero, por lo tanto, su desplazamiento sera cero

cifrarListaAux :: [String] -> Int -> [String]
cifrarListaAux [] _= []
cifrarListaAux (msj:msjs) movimiento = (cifrar msj movimiento) : cifrarListaAux msjs (movimiento+1) 

-- EJ 7
frecuencia :: String -> [Float]
frecuencia "" = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
frecuencia msj | totalMinusculas msj == 0 = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0]
               | otherwise = frecuenciaAux msj 'a'
               --la primera posicion de la lista resultado corresponde a la letra 'a'

frecuenciaAux :: String -> Char -> [Float]
frecuenciaAux msj letra | ord(letra)>122 = [] --las letras en minuscula van de la 97 a la 122 y empecé por 'a', que es 97, entonces termino en 122
                        | otherwise = frecuenciaLetra : frecuenciaAux msj (chr(ord(letra) + 1)) --hago recursion con el mismo mensaje y la siguiente letra minuscula del abecedario
                        where frecuenciaLetra = fromInteger((aparicionesLetra letra msj)*100) / fromInteger(totalMinusculas msj)

aparicionesLetra :: Char -> String -> Integer --funcion que devuelve cantidad de veces que aparece una letra minuscula en un mensaje
aparicionesLetra _ [] = 0
aparicionesLetra letra (c:cs) | letra==c = 1 + aparicionesLetra letra cs 
                              | otherwise = aparicionesLetra letra cs

totalMinusculas :: String -> Integer --funcion que devuelve la cantidad de minusculas en un mensaje
totalMinusculas [] = 0
totalMinusculas (c:cs) | esMinuscula c = 1 + totalMinusculas cs 
                       | otherwise = totalMinusculas cs

-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente msj movimiento = cifradoMasFrecuenteAux (frecuencia(cifrar msj movimiento)) 97 
                                                           --para empezar con la 'a' usamos el numero 97

cifradoMasFrecuenteAux :: [Float] -> Int -> (Char, Float)
cifradoMasFrecuenteAux [p] n = (chr(n) , p) 
cifradoMasFrecuenteAux (p:ps) n | p >= mayorPorcentaje = (chr(n) , p) 
                                | otherwise = cifradoMasFrecuenteAux ps (n+1)
                                where mayorPorcentaje = snd(cifradoMasFrecuenteAux ps (n+1))
--la p hace referencia a porcentaje y la n al número que representa a la letra
--el 8 funciona bien pero se tarda un poco 

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado msj msjCapazCifrado = esDescifradoAux msj msjCapazCifrado 0

esDescifradoAux :: String -> String -> Int -> Bool
esDescifradoAux _ _ 26 = False -- si dio la vuelta entera en la calesita del abecedario, no es descifrado
esDescifradoAux msj msjCapazCifrado i | cifrar msj i == msjCapazCifrado = True 
                                      | otherwise = esDescifradoAux msj msjCapazCifrado (i+1)

-- EJ 10            
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados lista = todosLosDescifradosAux lista lista

todosLosDescifradosAux :: [String] -> [String] -> [(String, String)]
todosLosDescifradosAux [] _ = []
todosLosDescifradosAux (msj:msjs) lista = (buscarDescifrados msj lista) ++ (todosLosDescifradosAux msjs lista)

buscarDescifrados :: String -> [String] -> [(String, String)]
buscarDescifrados _ [] = []
buscarDescifrados mensaje (msj:msjs) | esDescifradoAux mensaje msj 1 = (mensaje, msj):(buscarDescifrados mensaje msjs)
                                     | otherwise = buscarDescifrados mensaje msjs
--esta funcion arma pares donde el primer componente es el mensaje y el segundo componente son todos los mensajes de la lista que son el resultado de cifrar(mensaje,n) para algun n cuyo mod 26 es distinto de 0

--PARTE II
-- EJ 11
expandirClave :: String -> Int -> String
expandirClave clave longitud = expandirClaveAux clave clave [] longitud

expandirClaveAux :: String -> String -> String -> Int -> String
expandirClaveAux clave [c] claveExpandida longitud | longitud == length(claveExpandida) = claveExpandida
                                                   | otherwise = expandirClaveAux clave clave (claveExpandida++[c]) longitud
expandirClaveAux clave (c:cs) claveExpandida longitud | longitud == length(claveExpandida) = claveExpandida
                                                      | otherwise = expandirClaveAux clave cs (claveExpandida++[c]) longitud

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere msj clave = cifrarVigenereAux msj (expandirClave clave (length(msj)))

cifrarVigenereAux :: String -> String -> String
cifrarVigenereAux "" _ = ""
cifrarVigenereAux (c:cs) (cc:ccs) = (desplazar c (letraANatural cc)) : (cifrarVigenereAux cs ccs)
--c hace referencia a un caracter del mensaje y cc a un caracter de la clave expandida

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere msj clave = descifrarVigenereAux msj (expandirClave clave (length(msj)))

descifrarVigenereAux :: String -> String -> String
descifrarVigenereAux "" _ = ""
descifrarVigenereAux (c:cs) (cc:ccs) = (desplazar c (-(letraANatural cc))) : (descifrarVigenereAux cs ccs)
--c hace referencia a un caracter del mensaje y cc a un caracter de la clave expandida

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ [clave] = clave
peorCifrado mensaje (clave:claves) | distanciaConClave <= distanciaConPeorClave = clave
                                   | otherwise = peorCifrado mensaje claves
                                   where distanciaConClave = distancia (cifrarVigenere mensaje clave) mensaje (length(mensaje))
                                         distanciaConPeorClave = distancia (cifrarVigenere mensaje (peorCifrado mensaje claves)) mensaje (length(mensaje))

distancia :: String -> String -> Int -> Int --funcion que calcula la distancia entre dos listas
distancia _ _ 0 = 0
distancia (cc:ccs) (c:cs) longitud = abs(letraANatural(cc)-letraANatural(c)) + distancia ccs cs (longitud-1)
--c hace referencia a un caracter del mensaje y cc a un caracter de la clave expandida

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere [] _ _ = [] 
combinacionesVigenere (msj:msjs) claves cifrado = (combinacionesVigenereAux msj claves cifrado)++(combinacionesVigenere msjs claves cifrado)

combinacionesVigenereAux :: String -> [String] -> String -> [(String, String)]
combinacionesVigenereAux _ [] _ = []
combinacionesVigenereAux msj (clave:claves) cifrado | (cifrarVigenere msj clave) == cifrado = (msj, clave):(combinacionesVigenereAux msj claves cifrado)
                                                    | otherwise = combinacionesVigenereAux msj claves cifrado

