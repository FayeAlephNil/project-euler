module Problem3 where

import Data.List
--- Ripped from my learning project

--- Factors

-- Checks if a number is a factor of another number
factorof :: (Integral a)
	=> a -- Original Number
	-> a -- Possible Factor
	-> Bool -- true if m is a factor of n
factorof m n = (m `mod` n) == 0

-- Gets the factors of the number passed in
factors :: (Integral a) =>
	a -- Number to get factors of
	-> [a] -- Factors of the number
factors n = nub $ sort (below ++ map (\ x -> abs n `quot` x) below)
	where below = [x | x <- [1..(floor $ sqrt $ fromIntegral $ abs n)], n `mod` x == 0]

-- Gets the Prime factors of a number
primeFactors :: (Integral a)
	=> a -- Number to get prime factors of
	-> [a] -- Prime factors of the number
primeFactors = filter prime . factors

-- Checks if a number is prime
prime :: (Integral a)
	=> a -- Number to check
	-> Bool -- True if the number is prime, else False
prime n = factors n == [1, abs n]

problem3 :: IO ()
problem3 = print $ last $ primeFactors 600851475143
