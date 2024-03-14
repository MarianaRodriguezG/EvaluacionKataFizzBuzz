module FizzBuzz where

fizzbuzz :: Int -> String
fizzbuzz n
  | esPrimo n = "FizzBuzz!"
  | otherwise = esFizzBuzz n

esPrimo :: Int -> Bool
esPrimo n
  | n <= 1 = False
  | otherwise = not $ any (\x -> n `mod` x == 0) [2 .. esRaiz n]

esFizzBuzz :: Int -> String
esFizzBuzz n
  | n == 0 = "cero"
  | n < 20 = nrosBasicos !! (n-1)
  | n < 30 = if n `mod` 10 == 0 then decenas !! 0 else "veinti" ++ esFizzBuzz (n `mod` 10)
  | n < 100 = if n `mod` 10 == 0 then decenas !! (n `div` 10 - 2) else decenas !! (n `div` 10 - 2) ++ " y " ++ esFizzBuzz (n `mod` 10)
  | n == 100 = "cien"
  | n < 200 = "ciento " ++ esFizzBuzz (n `mod` 100)
  | n `mod` 100 == 0 = cientos !! ((n `div` 100) - 1)
  | n == 1000 = "mil"
  | n < 1000 = cientos !! ((n `div` 100) - 1) ++ " " ++ esFizzBuzz (n `mod` 100)
  | n < 2000 = "mil " ++ esFizzBuzz (n `mod` 1000)
  | n `mod` 1000 == 0 = esFizzBuzz (n `div` 1000) ++ " mil"
  | n == 100000 = "cien mil"
  | n == 1000000 = "un millon"
  | n < 1000000 = esFizzBuzz (n `div` 1000) ++ " mil " ++ esFizzBuzz (n `mod` 1000)
  | otherwise = error "Número no válido"

esRaiz :: Int -> Int
esRaiz = floor . sqrt . fromIntegral

nrosBasicos :: [String]
nrosBasicos = ["uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez", "once", "doce", "trece", "catorce", "quince", "dieciseis", "diecisiete", "dieciocho", "diecinueve"]

-- diecis :: [String]
-- diecis = ["dieciséis", "diecisiete", "dieciocho", "diecinueve"]

decenas :: [String]
decenas = ["veinte", "treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta", "noventa"]

cientos :: [String]
cientos = ["cien", "doscientos", "trescientos", "cuatrocientos", "quinientos", "seiscientos", "setecientos", "ochocientos", "novecientos"]
