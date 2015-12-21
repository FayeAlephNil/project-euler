module Problem2 where

fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

sumWhile :: (Int -> Bool) -> [Int] -> Int
sumWhile p xs = sum (takeWhile p xs)

problem2 :: IO ()
problem2 = print $ sumWhile (< 4000000) (filter even fibs)
