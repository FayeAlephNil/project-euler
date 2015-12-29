module Problem9 where

primitives = [(m^2 - n^2, 2 * m * n, m^2 + n^2) | m <- [1..], n <- [1..(m - 1)], (m - n) `mod` 2 == 1, gcd m n == 1]

pythagTriplesOfSum n = [let
  x =  n `div` (a + b + c)
  in (x * a, x * b, x * c) | (a, b, c) <- primitives, n `mod` (a + b + c) == 0]

tripleProduct :: (Int, Int, Int) -> Int
tripleProduct (a, b, c) = a * b * c

problem9 :: IO ()
problem9 = let
  tuple = head $ pythagTriplesOfSum 1000
  in print $ show tuple ++ ": Product = " ++ show (tripleProduct tuple)
