module Problem10 where
import Problem3

sumOfPrimesBelow:: Integer -> Integer
sumOfPrimesBelow n = sum $ takeWhile (< n) primes

problem10 :: IO ()
problem10 = print $ sumOfPrimesBelow 2000000
