module Main (main) where

import Data.List (isPrefixOf, tails)
import Data.Maybe (mapMaybe)

main :: IO ()
main = do
  input <- lines <$> readFile "inputs/day01.txt"
  print (part1 input)
  print (part2 input)

part1 :: [[Char]] -> Int
part1 = sum . map toInt . map firstAndLast . map findLiteralNumbers

part2 :: [[Char]] -> Int
part2 = sum . map toInt . map firstAndLast . map findNumbers

toInt :: [Char] -> Int
toInt = read

findLiteralNumbers :: [Char] -> [Char]
findLiteralNumbers = mapMaybe literalDigit . tails

findNumbers :: [Char] -> [Char]
findNumbers = mapMaybe (applyUntilJust [spelledDigits, literalDigit]) . tails

isDigit :: Char -> Bool
isDigit x = elem x "123456789"

firstAndLast :: [Char] -> [Char]
firstAndLast [] = error "Empty lists are not allowed"
firstAndLast [x] = [x, x]
firstAndLast (x : xs) = [x, last xs]

applyUntilJust :: [a -> Maybe b] -> a -> Maybe b
applyUntilJust [] _ = Nothing
applyUntilJust (f : fs) x = case f x of
  Just result -> Just result
  Nothing -> applyUntilJust fs x

literalDigit :: [Char] -> Maybe Char
literalDigit [] = Nothing
literalDigit (x : _)
  | isDigit x = Just x
  | otherwise = Nothing

spelledDigits :: [Char] -> Maybe Char
spelledDigits x
  | "one" `isPrefixOf` x = Just '1'
  | "two" `isPrefixOf` x = Just '2'
  | "three" `isPrefixOf` x = Just '3'
  | "four" `isPrefixOf` x = Just '4'
  | "five" `isPrefixOf` x = Just '5'
  | "six" `isPrefixOf` x = Just '6'
  | "seven" `isPrefixOf` x = Just '7'
  | "eight" `isPrefixOf` x = Just '8'
  | "nine" `isPrefixOf` x = Just '9'
  | otherwise = Nothing
