module Problem1 where

import Data.List

multipleOf :: Int -> Int -> Bool
n `multipleOf` m = n `mod` m == 0

filterMultiplesOf :: [Int] -> Int -> [Int]
xs `filterMultiplesOf` n = filter (`multipleOf` n) xs

filterMultiplesOfAll :: [Int] -> [Int] -> [Int]
xs `filterMultiplesOfAll` [] = []
xs `filterMultiplesOfAll` (y:ys) = nub $ xs `filterMultiplesOf` y ++ xs `filterMultiplesOfAll` ys

sumMultiplesOfAll :: [Int] -> [Int] -> Int
xs `sumMultiplesOfAll` ys = sum (xs `filterMultiplesOfAll` ys)

problemSolution :: IO ()
problemSolution = print $ [1..999] `sumMultiplesOfAll` [3, 5]
