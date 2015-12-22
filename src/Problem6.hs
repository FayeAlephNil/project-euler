module Problem6 where

to100 = [1..100]
to100Squares = map (\x -> x * x) to100

problem6 :: IO ()
problem6 = print $ let
  sumTo100 = sum to100
  in sumTo100 * sumTo100 - sum to100Squares
