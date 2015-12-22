module Problem4 where
import Data.List

digit = [100..999]
products = [a * b | a <- digit, b <- digit]

palindrome :: Int -> Bool
palindrome x = let
  str = show x
  in str == reverse str

problem4 :: IO ()
problem4 = print $ maximum $ filter palindrome products
