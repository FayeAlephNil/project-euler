module Problem5 where
import Problem1

smallestMultipleOfAll :: [Int] -> Int
smallestMultipleOfAll = foldl
  (\ acc x -> acc * (if acc `multipleOf` x then 1 else x `div` gcd acc x))
  1

problem5 :: IO ()
problem5 = print $ smallestMultipleOfAll [1..20]
