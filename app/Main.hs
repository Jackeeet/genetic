{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main(main) where

import GeneticLib (newPopulation, scores)
import System.Random

main :: IO ()
main = do
    pop <- newPopulation solutionRange populationSize
    print pop
   --  print $ randomPairs pop 2

f :: Int -> Int
f x = a + b * x + c * (x ^ 2) + d * (x ^ 3)
    where a = 67
          b = 3
          c = -66
          d = 1

solutionRange :: (Int, Int)
solutionRange = (-10, 53)

populationSize :: Int
populationSize = 4

crossoverCount :: Int
crossoverCount = 2

mutationCount :: Int
mutationCount = 1

randomPair :: [a] -> IO (a, a)
randomPair xs = do
    i1 <- randomRIO (0, length xs - 1)
    i2 <- randomRIO (0, length xs - 1)
    return (xs !! i1, xs !! i2)

randomPairs :: (Eq a) => [a] -> Int -> IO [(a, a)]
randomPairs xs n = do
    pair <- randomPair xs
    if n > 0 then
        if uncurry (==) pair then 
            randomPairs xs n
        else do
            pairs <- randomPairs xs (n - 1)
            return $ pair : pairs
    else return []
