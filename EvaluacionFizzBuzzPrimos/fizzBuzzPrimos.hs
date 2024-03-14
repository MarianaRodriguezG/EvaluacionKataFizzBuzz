module FizzBuzz where

fizzbuzz :: Int -> String
fizzbuzz n
  | isPrime n = "FizzBuzz!"
  | otherwise = numeroATexto n

isPrime :: Int -> Bool
isPrime n
  | n <= 1 = False
  | otherwise = not $ any (\x -> n `mod` x == 0) [2..isqrt n]
numeroATexto :: Int -> String
numeroATexto n
  | n == 0 = "cero"
  | n < 20 = basicNumbers !! (n - 1)
  | n < 30 = if n `mod` 10 == 0 then tens !! 0 else "veinti" ++ numeroATexto (n `mod` 10)
  | n < 100 = if n `mod` 10 == 0 then tens !! (n `div` 10 - 2) else tens !! (n `div` 10 - 2) ++ " y " ++ numeroATexto (n `mod` 10)
  | n == 1000 = "mil"
  | n < 1000 = if n `mod` 100 == 0 then hundreds !! (n `div` 100 - 1) else hundreds !! (n `div` 100 - 1) ++ " " ++ (if n `mod` 100 == 0 then "" else numeroATexto (n `mod` 100))
  | n == 1000000 = "un millón"
  | otherwise = numeroATexto (n `div` 1000) ++ " mil " ++ (if n `mod` 1000 == 0 then "" else numeroATexto (n `mod` 1000))

isqrt :: Int -> Int
isqrt = floor . sqrt . fromIntegral

basicNumbers :: [String]
basicNumbers = ["uno", "dos", "tres", "cuatro", "cinco", "seis", "siete", "ocho", "nueve", "diez", "once", "doce", "trece", "catorce", "quince"]

teens :: [String]
teens = ["dieciséis", "diecisiete", "dieciocho", "diecinueve"]

tens :: [String]
tens = ["veinte", "treinta", "cuarenta", "cincuenta", "sesenta", "setenta", "ochenta", "noventa"]

hundreds :: [String]
hundreds = ["cien", "doscientos", "trescientos", "cuatrocientos", "quinientos", "seiscientos", "setecientos", "ochocientos", "novecientos"]
