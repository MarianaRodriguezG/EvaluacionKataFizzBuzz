module FizzBuzz where

ifThenElse :: Bool -> a -> a -> a
ifThenElse cond thenVal elseVal =
  case cond of
    True -> thenVal
    False -> elseVal

fizzbuzz :: Int -> String
-- fizzbuzz n = "One!"
-- fizzbuzz n = if n==1 then "One!" else "Two!"
-- fizzbuzz n = ifThenElse (n==1) "One!" "Two!"
fizzbuzz n = menoresA20 (n)
fizzbuzz _ = "othewise"

menoresA20 :: Int -> String
menoresA20 n
  | n > 0 && n < 20 =
      let respuestas =
            words
              ( "uno dos tres cuatro cinco seis siete ocho nueve diez "
                  ++ "once doce trece catorce quince dieciseis "
                  ++ "diecisiete dieciocho diecinueve"
              )
       in respuestas !! (n - 1)

dieces :: Int -> String
dieces n
  | n >= 2 && n <= 9 =
      respuestas !! (n - 2)
  where
    respuestas = words "veinte treinta cuarenta cincuenta sesenta setenta ochenta noventa"

veintes :: Int -> String
veintes n
    | n > 20 && n < 30 =
        let respuestas =
             words 
                ("veintiuno veintidos veintitres veinticuatro "
                ++ "veinticinco veintiseis veintisiete veintiocho veintinueve")
        in respuestas !! (n-2)         
              

number :: Int -> String
number n
  | n >= 1 && n < 20 = menoresA20 (n)
  | n `mod` 10 == 0 && n < 100 = dieces (n `div` 10)
  |n < 100 = veintes (n `div` 20) ++ " " ++veintes (n `mod` 20)
  | n < 100 = dieces (n `div` 10) ++ " " ++ menoresA20 (n `mod` 10) 
  | n == 100 = "cien"

-- fizzbuzz n |  (n==1) = "One!"
--            | (n/=1) = "Two!"
