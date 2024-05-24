import Test.HUnit
import Solucion
import Data.List
-- No está permitido agregar nuevos imports.

runCatedraTests = runTestTT allTests

allTests = test [
    "esMinuscula" ~: testsEjesMinuscula,
    "letraANatural" ~: testsEjletraANatural,
    "desplazar" ~: testsEjdesplazar,
    "cifrar" ~: testsEjcifrar,
    "descifrar" ~: testsEjdescifrar,
    "cifrarLista" ~: testsEjcifrarLista,
    "frecuencia" ~: testsEjfrecuencia,
    "cifradoMasFrecuente" ~: testsEjcifradoMasFrecuente,
    "esDescifrado" ~: testsEjesDescifrado,
    "todosLosDescifrados" ~: testsEjtodosLosDescifrados,
    "expandirClave" ~: testsEjexpandirClave,
    "cifrarVigenere" ~: testsEjcifrarVigenere,
    "descifrarVigenere" ~: testsEjdescifrarVigenere,
    "peorCifrado" ~: testsEjpeorCifrado,
    "combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]


testsEjesMinuscula = test [
    "Minuscula" ~: esMinuscula 'a' ~?= True,
    "Caracter que no es una letra" ~: esMinuscula '/' ~?= False, 
    "Mayuscula" ~: esMinuscula 'A' ~?= False 
    ]

testsEjletraANatural = test [
    "Primer letra minuscula" ~: letraANatural 'a' ~?= 0,
    "Ultima letra minuscula" ~: letraANatural 'z' ~?= 25,
    "Otra letra minuscula" ~: letraANatural 'm' ~?= 12
    ]

testsEjdesplazar = test [
    --movimiento positivo
    "Movimiento corto con minuscula" ~: desplazar 'a' 3 ~?= 'd', 
    "Movimiento corto con un caracter que no es una letra" ~: desplazar '/' 3 ~?= '/', 
    "Movimiento corto con mayuscula" ~: desplazar 'B' 3 ~?= 'B', 
    "Movimiento nulo" ~: desplazar 'g' 0 ~?= 'g',
    "Vuelta completa" ~: desplazar 'a' 26 ~?= 'a',
    "Un poco mas de una Vuelta completa" ~: desplazar 'b' 27 ~?= 'c', 
    "Mas de dos vueltas completas" ~: desplazar 'a' 100 ~?= 'w',
    --movimiento negativo
    "Movimiento corto con minuscula y negativo" ~: desplazar 'n' (-3) ~?= 'k', 
    "Movimiento corto con un caracter que no es una letra y negativo" ~: desplazar '/' (-3) ~?= '/', 
    "Movimiento corto con mayuscula y negativo" ~: desplazar 'B' (-3) ~?= 'B', 
    "Vuelta completa y negativo" ~: desplazar 'a' (-26) ~?= 'a',
    "Un poco menos de una Vuelta completa y negativo" ~: desplazar 'b' (-3) ~?= 'y', 
    "Mas de dos vueltas completas y negativo" ~: desplazar 'a' (-100) ~?= 'e'
    ]

testsEjcifrar = test [
    "Minusculas" ~: cifrar "computacion" 3 ~?= "frpsxwdflrq", 
    "Mayusculas y minusculas" ~: cifrar "aAbBCc" 1 ~?= "bAcBCd", 
    "Vuelta completa" ~: cifrar "zorro" 26 ~?= "zorro", 
    "Minusculas y caracteres que no son letras" ~: cifrar "hola!" 5 ~?= "mtqf!", 
    "Mas de dos vueltas completas" ~: cifrar "abc" 100 ~?= "wxy", 
    "Minusculas, mayusculas y caracteres que no son letras" ~: cifrar "holA!" 5 ~?= "mtqA!"
    ]

testsEjdescifrar = test [
    "Minusculas" ~: descifrar "frpsxwdflrq" 3 ~?= "computacion", 
    "Mayusculas y minusculas" ~: descifrar "Bwgpcu Tctfgu" 2 ~?= "Buenas Tardes",
    "Vuelta completa" ~: descifrar "apssp" 26 ~?= "apssp", 
    "Minusculas y caracteres que no son letras" ~: descifrar "mtqf!" 3 ~?= "jqnc!", 
    "Mas de dos vueltas completas" ~: descifrar "wxy" 100 ~?= "abc", 
    "Minusculas, mayusculas y caracteres que no son letras" ~: descifrar "mtqA!" 5 ~?= "holA!"
    ]

testsEjcifrarLista = test [
    "Lista minusculas" ~: cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"], 
    "Lista no solo minusculas" ~: cifrarLista ["hola", "Chau", "teXto*", ""] ~?= ["hola", "Cibv","vgXvq*", ""],
    "Lista sin minusculas" ~: cifrarLista ["HOLA", "123", "CH4", "!X*"] ~?= ["HOLA", "123", "CH4", "!X*"],
    "Lista vacia" ~: cifrarLista [] ~?= [], 
    "Lista con muchos elementos" ~: cifrarLista ["a", "b", "c","d", "e", "f","g", "h", "i","j", "k", "l","m", "n", "o","p", "q", "r","s", "t", "u","v", "x", "y ","z", "a", "b"] ~?= ["a","c","e","g","i","k","m","o","q","s","u","w","y","a","c","e","g","i","k","m","o","q","t","v ","x","z","b"] 
    ]

testsEjfrecuencia = test [
    "Minusculas" ~: expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0], 
    "Letra a es el 100%" ~: expectlistProximity (frecuencia "aaaa") [100,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0], 
    "Letra a es el 50%" ~: expectlistProximity (frecuencia "aabc") [50,25,25,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0],
    "Sin minusculas" ~: expectlistProximity (frecuencia "AAAA") [0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0],
    "Letra Z es el 100%" ~: expectlistProximity (frecuencia "zzzz") [0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0,0.0,0,0.0,0.0,0.0,0.0,0.0,100],  
    "No solo minusculas" ~: expectlistProximity (frecuencia "a/b/cde") [20,20,20,20,20,0.0,0.0,0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0],
    "Vacio" ~: expectlistProximity (frecuencia "") [0,0,0,0,0,0.0,0.0,0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0] 
    ]

testsEjcifradoMasFrecuente = test [
  "Solo minusculas y una letra con mayor frecuencia" ~: expectAnyTuplaAprox (cifradoMasFrecuente "taller" 3) [('o', 33.33333)], 
  "No solo minusculas y una letra con mayor frecuencia" ~: expectAnyTuplaAprox (cifradoMasFrecuente "AaAa" 3)  [('d', 100)], 
  "Solo minusculas y mas de una letra con mayor frecuencia" ~: expectAnyTuplaAprox (cifradoMasFrecuente "aaller" 26) [('l', 33.333332), ('a', 33.333332)], 
  "No solo minusculas y mas de una letra con mayor frecuencia" ~: expectAnyTuplaAprox (cifradoMasFrecuente "aaBcc" 100) [('w', 50), ('y', 50)] 
  ]

testsEjesDescifrado = test [
  "Solo minusculas y falso" ~: esDescifrado "taller" "compu" ~?= False, 
  "Solo minusculas y verdadero" ~:  esDescifrado "aaaa" "bbbb" ~?= True, 
  "No solo minusculas y vacio" ~: esDescifrado "" "bBbB" ~?= False, 
  "No solo minusculas y verdadero" ~: esDescifrado "aAaA" "bAbA" ~?= True 
  ]

testsEjtodosLosDescifrados = test [
  "No todos aparecen" ~: expectPermutacion (todosLosDescifrados ["compu", "frpsx", "mywza"]) [("compu", "frpsx"), ("frpsx", "compu")],  
  "Dos elementos y todos aparecen" ~: expectPermutacion (todosLosDescifrados ["bbbb", "aaaa"]) [("bbbb","aaaa"),("aaaa","bbbb")], 
  "Mas de dos elementos y todos aparecen" ~: expectPermutacion (todosLosDescifrados ["aaaa", "bbbb", "xxxx"]) [("aaaa","bbbb"),("aaaa","xxxx"),("bbbb","aaaa"),("bbbb","xxxx"),("xxxx","aaaa"),("xxxx","bbbb")], 
  "Ninguno aparece" ~: expectPermutacion (todosLosDescifrados ["bbbc", "aaaa", "mywza"]) [], 
  "No todos minuscula" ~: expectPermutacion (todosLosDescifrados ["bBbB", "aBaB", "mywza"]) [("bBbB","aBaB"),("aBaB","bBbB")] 

  ]

testsEjexpandirClave = test [
  "Alargar simple" ~: expandirClave "compu" 8 ~?= "compucom", 
  "Alargar complejo" ~: expandirClave "clave" 24 ~?= "claveclaveclaveclaveclav", 
  "Recorte" ~: expandirClave "compu" 3 ~?= "com" 
  ]


testsEjcifrarVigenere = test [
  "Clave con dos valores" ~: cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv", 
  "Mismo mensaje" ~: cifrarVigenere "abcd" "a" ~?= "abcd", 
  "Clave con un valor" ~: cifrarVigenere "abcd" "b" ~?= "bcde", 
  "Clave con mas valores" ~: cifrarVigenere "abcd" "don" ~?= "dppg", 
  "Mensaje vacio" ~: cifrarVigenere "" "zzzz" ~?= ""
  ]

testsEjdescifrarVigenere = test [
  "Clave con dos valores" ~: descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion", 
  "Mismo mensaje" ~: descifrarVigenere "abcd" "a" ~?= "abcd", 
  "Clave con un valor" ~: descifrarVigenere "bcde" "b" ~?= "abcd", 
  "Clave con mas valores" ~: descifrarVigenere "dppg" "don" ~?= "abcd",
  "Mensaje vacio" ~: descifrarVigenere "" "zzzz" ~?= "" 
  ]

testsEjpeorCifrado = test [
  "Tres claves" ~: peorCifrado "computacion" ["ip", "asdef", "ksy"] ~?= "asdef", 
  "Clave repetida" ~: peorCifrado "computacion" ["ab", "ab", "abc"] ~?= "ab",
  "Dos peores claves" ~: expectAny(peorCifrado "computacion" ["ab", "ba", "abc"]) ["ab", "ba"],
  "Dos claves" ~: peorCifrado "computacion" ["z", "za"] ~?= "za", 
  "Una claves" ~: peorCifrado "computacion" ["z"] ~?= "z", 
  "Una clave deja al mensaje igual" ~: peorCifrado "computacion" ["z","a","ds"] ~?= "a" 
  ]

testsEjcombinacionesVigenere = test [
  "Combinacion simple" ~: combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb" ~?= [("hola", "b")], 
  "Una sola clave" ~: combinacionesVigenere ["aaaa", "mundo"] ["b"] "bbbb" ~?= [("aaaa", "b")], 
  "Listas vacias" ~: combinacionesVigenere [] [] "aaaa" ~?= [], 
  "cifrado vacio" ~: combinacionesVigenere [] [] "" ~?= [], 
  "Nada es igual a cifrado" ~: combinacionesVigenere ["aaaa", "mundo"] ["b","c"] "aaaa" ~?= [] 
  ]

-- Funciones útiles

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)