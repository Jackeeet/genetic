{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main(main) where

import           GeneticLib    (newPopulation)

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


