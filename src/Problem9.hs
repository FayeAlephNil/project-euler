module Problem9 where

pythagTriplesOfSum n = filter (\(a, b, c) -> a^2 + b^2 == c^2) [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..n], a + b + c == n]

tripleProduct :: (Int, Int, Int) -> Int
tripleProduct (a, b, c) = a * b * c

problem9 :: IO ()
problem9 = let
  tuple = head $ pythagTriplesOfSum 1000
  in print $ show tuple ++ ": Product = " ++ show (tripleProduct tuple)
