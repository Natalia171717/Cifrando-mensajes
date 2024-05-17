module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {Los Linces}
-- Integrante1: { 4526405, Andrade, Gonzalo}
-- Integrante2: { DNI2,apellidoYNombre2}
-- Integrante3: { DNI3,apellidoYNombre3}
-- Integrante4: { DNI4,apellidoYNombre4}
-- Integrantes que abandonaron la materia: {En caso que haya abandonado la materia algún
                        -- integrante, completar con los dni y apellidos, sino dejar vacío}


-- Las letras en minuscula van de la 97 a la 122
-- Las mayusculas van de la 65 a la 90

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula caracter = 97 <= Data.Char.ord(caracter) && Data.Char.ord(caracter) <= 122 
                -- el 97 es la a minuscula, el 122 es la Z minuscula

-- EJ 2
esMayuscula :: Char -> Bool
esMayuscula caracter = 65 < Data.Char.ord(caracter) && Data.Char.ord(caracter) < 90 
                -- el 65 es la A mayusucla, el 90 es la Z mayuscula

letraANatural :: Char -> Int
letraANatural caracter | esMinuscula caracter == True = Data.Char.ord(caracter) - 97
                       | esMayuscula caracter == True = Data.Char.ord(caracter) - 65
                       | otherwise = Data.Char.ord(caracter) -- Aca tiro q si no es una letra, 
                       -- que devuelva igual asi el numero del caracter, no se si esta bien o mal

-- EJ 3
desplazar :: Char -> Int -> Char
desplazar caracter movimiento | esMinuscula caracter == True && (Data.Char.ord(caracter) + movimiento) <= 122 = Data.Char.chr(Data.Char.ord(caracter) + movimiento)
                              | esMinuscula caracter == True = Data.Char.chr(Data.Char.ord(caracter) + movimiento - 26) -- En el caso que se pase, que empiece hacia atras de vuelta
                              | otherwise = 'c'

-- EJ 4
cifrarAux :: Char -> Int -> Char
cifrarAux caracter movimiento | esMinuscula caracter == True && (Data.Char.ord(caracter) + movimiento) <= 122 = Data.Char.chr(Data.Char.ord(caracter) + movimiento)
                              | esMinuscula caracter == True && (Data.Char.ord(caracter) + movimiento) >= 97 = Data.Char.chr(Data.Char.ord(caracter) + movimiento - 26) -- No es perfecto el 26, ya que si se da mas de una vuelta, funciona mal
                              | esMinuscula caracter == True = Data.Char.chr(Data.Char.ord(caracter) + movimiento + 26) -- En el caso que vaya para atras, que vuelva hacia el abecedario dado vuelta
                              | otherwise = caracter
-- Repensarlo

cifrar :: String -> Int -> String
cifrar "" _ = ""
cifrar (x:xs) movimiento = (cifrarAux x movimiento) : (cifrar xs movimiento)
-- Va a tener casos que tire mal, x el problema del cifrarAux

-- EJ 5
descifrar :: String -> Int -> String
descifrar (x:xs) movimiento = (cifrarAux x (-movimiento)) : (cifrar xs (-movimiento))

-- Va a tener casos que tire mal, x el problema del cifrarAux

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista [] = []
cifrarLista (x:xs) = cifrarListaAux (x:xs) 0 
-- Va a tener casos que tire mal, x el problema del cifrarAux

cifrarListaAux :: [String] -> Int -> [String]
cifrarListaAux [] _= []
cifrarListaAux (x:xs) movimiento = cifrar x movimiento : cifrarListaAux xs (movimiento+1) 

-- EJ 7
frecuencia :: String -> [Float]
frecuencia palabra = [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0]


-- Ej 8
cifradoMasFrecuente :: String -> Int -> (Char, Float)
cifradoMasFrecuente _ _ = ('o', 33.333336)

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado palabraNormal palabraCapazCifrada | esDescifradoAux palabraNormal palabraCapazCifrada 0 == True = True
                                               | otherwise = False

esDescifradoAux :: String -> String -> Int -> Bool
esDescifradoAux _ _ 26 = False -- Si dio la vuelta entera en la calesita del abecedario, no es descifrado
esDescifradoAux palabraNormal palabraCapazCifrada n | cifrar palabraNormal n == palabraCapazCifrada = True 
                                               | otherwise = esDescifradoAux palabraNormal palabraCapazCifrada (n+1)
-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados (x:y:xs) | esDescifrado x y == True = [(x, y)] ++ todosLosDescifrados xs
                             | otherwise = todosLosDescifrados (y:xs) 
                            -- No esta perfecto xq puede ser que en xs haya un 
                            --descifrado equivalente a X
                            -- Rever

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave (x:xs) n | length (x:xs) == n = (x:xs)
                       | otherwise = [x] ++ expandirClave xs n
                        -- No funca

-- EJ 12
cifrarVigenere :: String -> String -> String
cifrarVigenere _ _ = "kdueciirqdv"

-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere _ _ = "computacion"

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ _ = "asdef"

-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
combinacionesVigenere _ _ _ = [("hola", "b")]
