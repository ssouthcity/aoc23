module Main (main) where

main :: IO ()
main = do
  input <- lines <$> readFile "inputs/day01.txt"
  print (sum (map calibrationValue input))

isDigit :: Char -> Bool
isDigit x = elem x "123456789"

firstAndLast :: [Char] -> [Char]
firstAndLast [x] = [x, x]
firstAndLast (x : xs) = [x, last xs]

calibrationValue :: [Char] -> Int
calibrationValue x = read (firstAndLast (filter isDigit x))
