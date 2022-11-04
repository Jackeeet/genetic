{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main(main) where

import           Data.List  (sort)
import           GeneticLib (getFitness, newPopulation, nextGeneration)

main :: IO ()
main = do
    let offset = if solutionFrom < 0 then (-1) * solutionFrom else 0
    initial <- newPopulation (solutionFrom + offset, solutionTo + offset) populationSize
    print initial
    nextGen <- nextGeneration initial crossoverCount
    print nextGen
    let scores = getFitness ((-1) * offset) f nextGen
    print scores


f :: Int -> Int
f x = a + b * x + c * (x ^ 2) + d * (x ^ 3)
    where a = 67
          b = 3
          c = -66
          d = 1

solutionFrom :: Int
solutionFrom = -10

solutionTo :: Int
solutionTo = 53

populationSize :: Int
populationSize = 4

crossoverCount :: Int
crossoverCount = 2

