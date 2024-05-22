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
    --"expandirClave" ~: testsEjexpandirClave,
    --"cifrarVigenere" ~: testsEjcifrarVigenere,
    --"descifrarVigenere" ~: testsEjdescifrarVigenere,
    --"peorCifrado" ~: testsEjpeorCifrado,
    --"combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]


testsEjesMinuscula = test [
    esMinuscula 'a' ~?= True
    esMinuscula '/' ~?= False
    esMinuscula 'A' ~?= False
    ]

testsEjletraANatural = test [
    letraANatural 'b' ~?= 1
    letraANatural '/' ~?= '/'
    letraANatural 'B' ~?= B
    ]

testsEjdesplazar = test [
    desplazar 'a' 3 ~?= 'd'
    desplazar '/' 3 ~?= '/'
    desplazar 'B' 3 ~?= 'B'
    desplazar 'g' 0 ~?= 'g'
    desplazar 'a' 26 ~?= 'a'
    desplazar 'b' 27 ~?= 'c'
    desplazar 'f' (-1) ~?= 'e'
    ]

testsEjcifrar = test [
    cifrar "computacion" 3 ~?= "frpsxwdflrq"
    cifrar "Nada" 3 ~?= "Ndgd"
    cifrar "Buenas Tardes" 2 ~?= "Bwgpcu Tctfgu"
    cifrar "zorro" 1 ~?= "apssp"
    cifrar "luz" (-2) ~?= "jsx"
    cifrar "hola!" 5 ~?= "mtqf!"
    ]

testsEjdescifrar = test [
    descifrar "frpsxwdflrq" 3 ~?= "computacion"
    descifrar "Ndgd" 3 ~?= "Nada"
    descifrar "Bwgpcu Tctfgu" 2 ~?= "Buenas Tardes"
    descifrar "apssp" 1 ~?= "zorro"
    descifrar "jsx" (-2) ~?= "luz"
    descifrar "mtqf!" 3 ~?= "hola!"
    ]

testsEjcifrarLista = test [
    cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"]
    cifrarLista ["hola", "Chau", "texto"] ~?= ["hola","Cibv","vgzvq"]
    cifrarLista ["yo", "", "vos"] ~?= ["yo","","xqu"]
    ]

testsEjfrecuencia = test [
    expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0]
    ]

--testsEjcifradoMasFrecuente = test [
  --  cifradoMasFrecuente "taller" 3 ~?= ('o', 33.333336)]

--testsEjesDescifrado = test [
  --  esDescifrado "taller" "compu" ~?= False]

--testsEjtodosLosDescifrados = test [todosLosDescifrados ["compu", "frpsx", "mywza"] ~?= [("compu", "frpsx"), ("frpsx", "compu")]]

--testsEjexpandirClave = test [
  --  expandirClave "compu" 8 ~?= "compucom"]

--testsEjcifrarVigenere = test [
  --  cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv"]

--testsEjdescifrarVigenere = test [
  --  descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion"]

--testsEjpeorCifrado = test [
  --  peorCifrado "computacion" ["ip", "asdef", "ksy"] ~?= "asdef"]

--testsEjcombinacionesVigenere = test [
  --  combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb" ~?= [("hola", "b")]]

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