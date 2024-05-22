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
    "peorCifrado" ~: testsEjpeorCifrado
    --"combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]


testsEjesMinuscula = test [
    esMinuscula 'a' ~?= True, -- Probamos con una minuscula
    esMinuscula '/' ~?= False, --Probamos con un caracter que no sea una letra
    esMinuscula 'A' ~?= False -- Probamos con una mayuscula
    ]

testsEjletraANatural = test [
    letraANatural 'b' ~?= 1, -- Probando con una minuscula
    letraANatural '/' ~?= -50, -- Probamos con un caracter que no sea una letra del abecedario
    letraANatural 'B' ~?= -31 -- Probamos con una mayuscula
    ]

testsEjdesplazar = test [
    desplazar 'a' 3 ~?= 'd', -- Probamos un movimiento corto en las primeras letras
    desplazar '/' 3 ~?= '/', -- Probamos un movimiento corto con algo que no deberia moverse
    desplazar 'B' 3 ~?= 'B', -- Probamos un movimiento corto con algo que no deberia moverse
    desplazar 'g' 0 ~?= 'g', -- Probamos un movimiento nulo
    desplazar 'a' 26 ~?= 'a', -- Probamos una vuelta completa
    desplazar 'b' 27 ~?= 'c', -- Probamos mas de una vuelta
    desplazar 'a' 100 ~?= 'w', -- Probamos mas de dos vueltas en el abecedario
    desplazar 'f' (-1) ~?= 'e' -- Probamos un movimiento negativo
    ]

testsEjcifrar = test [
    cifrar "computacion" 3 ~?= "frpsxwdflrq", -- Probamos un cifrado simple
    cifrar "aAbBCc" 1 ~?= "bAcBCd", -- Probamos un test con mayusculas y minusculas
    cifrar "zorro" 26 ~?= "zorro", -- Probamos con que de la vuelta entera
    cifrar "luz" (-2) ~?= "jsx", -- Probamos con movimiento negativo
    cifrar "hola!" 5 ~?= "mtqf!", --Probamos con minusculas y simbolos que no deberian de moverse
    cifrar "abc" 100 ~?= "wxy" -- Probamos mas de dos vueltas en el abecedario
    ]

testsEjdescifrar = test [
    descifrar "frpsxwdflrq" 3 ~?= "computacion", -- Probamos con solo minusculas
    descifrar "Bwgpcu Tctfgu" 2 ~?= "Buenas Tardes", -- Probamo con mayusculas y minusculas
    descifrar "apssp" 26 ~?= "apssp", -- Probamos con una vuelta completa
    descifrar "jsx" (-2) ~?= "luz", -- Probamos con movimientos negativos
    descifrar "mtqf!" 3 ~?= "jqnc!", -- Probamos con elementos que no se deberian de mover
    descifrar "wxy" 100 ~?= "abc" -- Probamos mas de dos vueltas en el abecedario
    ]

testsEjcifrarLista = test [
    cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"], -- Probamos con una lista de todo minuscula
    cifrarLista ["hola", "Chau", "teXto"] ~?= ["hola","Cibv","vgXvq"], -- Probamos con una lista con masyusculas y minusculas
    cifrarLista ["yo", "", "vos"] ~?= ["yo","","xqu"], -- Probamos con una lista vacia
    cifrarLista ["a", "b", "c","d", "e", "f","g", "h", "i","j", "k", "l","m", "n", "o","p", "q", "r","s", "t", "u","v", "x", "y ","z", "a", "b"] ~?= ["a","c","e","g","i","k","m","o","q","s","u","w","y","a","c","e","g","i","k","m","o","q","t","v ","x","z","b"] -- Probamos con una lista de mas de26 elementos
    ]

testsEjfrecuencia = test [
    expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0], -- Un ejemplo con todas minusculas
    expectlistProximity (frecuencia "aaaa") [100,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0], -- Un ejemplo con 100%
    expectlistProximity (frecuencia "aabc") [50,25,25,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0], -- Un ejemplo con 50%
    expectlistProximity (frecuencia "AAAA") [0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0], -- Un ejemplo con 0% en totalidad
    expectlistProximity (frecuencia "zzzz") [0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0,0.0,0,0.0,0.0,0.0,0.0,0.0,100],  --Un ejemplo con 100%
    expectlistProximity (frecuencia "a/a/") [100,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0,0,0.0,0.0,0.0,0.0,0.0,0,0.0,0,0.0,0.0,0.0,0.0,0.0,0.0] -- Un ejemplo con 100% debido a que hay minusculas y elementos q no cuentan
    ]

testsEjcifradoMasFrecuente = test [
  cifradoMasFrecuente "taller" 3 ~?= ('o', 33.333332), -- Probamos un test de solo minusculas y que hayan 2 iguales
  cifradoMasFrecuente "AaAa" 3 ~?= ('d', 100), -- Probamos un test con minusculas y mayusculas 
  cifradoMasFrecuente "taller" 26 ~?= ('l', 33.333332), -- Probamos un test con una vuelta entera
  cifradoMasFrecuente "aabc" 100 ~?= ('w', 50) -- Probamos un test con mas de una vuelta
  ]

testsEjesDescifrado = test [
  esDescifrado "taller" "compu" ~?= False, -- Probamos con un ejemplo simple y falso
  esDescifrado "aaaa" "bbbb" ~?= True, -- Probamos con un ejemplo simple y verdadero
  esDescifrado "aaaa" "bBbB" ~?= False, -- Probamos con un ejemplo de mayusculas falso
  esDescifrado "aAaA" "bAbA" ~?= True -- Ṕrobamos con un ejemplo de mayusculasy minsculas verdadero
  ]

testsEjtodosLosDescifrados = test [todosLosDescifrados ["compu", "frpsx", "mywza"] ~?= [("compu", "frpsx"), ("frpsx", "compu")],  -- Probamos en un caso donde son todos minusculas
  todosLosDescifrados ["bbbb", "aaaa", "mywza"] ~?= [("bbbb","aaaa"),("aaaa","bbbb")], -- Probamos en un caso donde son todos minusculas
  todosLosDescifrados ["aaaa", "bbbb", "xxxx"] ~?= [("aaaa","bbbb"),("aaaa","xxxx"),("bbbb","aaaa"),("bbbb","xxxx"),("xxxx","aaaa"),("xxxx","bbbb")], -- Probamos en un caso donde hay mas de dos iguales
  todosLosDescifrados ["bBbB", "aAaA", "mywza"] ~?= [], -- Probamos en un caso donde sean todos diferentes
  todosLosDescifrados ["bBbB", "aBaB", "mywza"] ~?= [("bBbB","aBaB"),("aBaB","bBbB")] -- Probamos un caso donde hay minusculasy mayusculas

  ]

testsEjexpandirClave = test [
  expandirClave "compu" 8 ~?= "compucom", --Probamos con un caso de todos minusculas
  expandirClave "clave" 7 ~?= "clavecl", -- Probamos con un caso de todos minusculas
  expandirClave "compu" 3 ~?= "com" -- Probamos con cortar la palabra a algo mas corto 
  ]


testsEjcifrarVigenere = test [
  cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv", --Probamos en un caso que que seacon dos valores diferentes
  cifrarVigenere "abcd" "a" ~?= "abcd", -- Probamos un caso donde no se mueve
  cifrarVigenere "abcd" "b" ~?= "bcde", -- Probamos un caso donde da un paso
  cifrarVigenere "abcd" "ab" ~?= "acce", -- Probamos un caso donde se mueve con dos valores diferentes
  cifrarVigenere "" "zzzz" ~?= "" -- Probamos un caso donde el mensaje a cambiar es vacio
  ]

testsEjdescifrarVigenere = test [
  descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion", --Probamos en un caso que que seacon dos valores diferentes
  descifrarVigenere "abcd" "a" ~?= "abcd", -- Probamos un caso donde no se mueve
  descifrarVigenere "bcde" "b" ~?= "abcd", -- Probamos un caso donde da un paso
  cifrarVigenere "" "zzzz" ~?= "" -- Probamos un caso donde el mensaje a cambiar es vacio
  ]

testsEjpeorCifrado = test [
  peorCifrado "computacion" ["ip", "asdef", "ksy"] ~?= "asdef", -- Probamos en un caso que sea con 3 valores diferentes
  peorCifrado "computacion" ["a", "ab", "abc"] ~?= "a", -- Probamos con casos mas simples,dondeuno de los casos no cambia nada
  peorCifrado "computacion" ["ab", "ab", "abc"] ~?= "ab", -- Probamos cuando esta repetido
  peorCifrado "computacion" ["z", "za"] ~?= "za", -- Probamos un caso especifico
  peorCifrado "computacion" ["z"] ~?= "z", -- Probamos cuando hay un solo elemento
  peorCifrado "computacion" ["z","a","ds"] ~?= "a" -- Hicimosun ultimo ejercicio simple
  ]

testsEjcombinacionesVigenere = test [
  combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb" ~?= [("hola", "b")]]

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