{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main(main) where

import           GeneticLib (Genotype, Score, getFitness, newPopulation,
                             reproduce, select)

main :: IO ()
main = do
    let minMode = True
    let normalize = if minMode then normalizeMin else normalizeMax
    let offset = if solutionFrom < 0 then (-1) * solutionFrom else 0 :: Integer
    let iterations = 5

    initial <- newPopulation (solutionFrom + offset, solutionTo + offset) populationSize
    putStrLn $ "Initial population: " ++ show initial
    putStrLn $ "Initial scores: " ++ show (getFitness ((-1) * offset) f initial)
    runGenerations ((-1) * offset) normalize initial iterations iterations


runGenerations :: Integer -> ([Score] -> [Score]) -> [Genotype] -> Int -> Int -> IO ()
runGenerations _ _ _ _ 0           = return ()
runGenerations ofs norm pop maxN n = do
    let generation = show (maxN - n)
    putStrLn $ "-------------" ++ generation ++ "-------------"
    nextGen <- reproduce pop crossoverCount
    putStrLn $ show nextGen
    let scores = norm $ getFitness ofs f nextGen
    selected <- select [(gt, sc) | gt <- nextGen, sc <- scores ] populationSize
    putStrLn $ "GEN " ++ generation ++ ": "  ++ show selected
    let values = getFitness ofs f selected
    putStrLn $ show values
    runGenerations ofs norm selected maxN (n - 1)


f :: Integer -> Integer
f x = a + b * x + c * (x ^ 2) + d * (x ^ 3)
    where a = 67
          b = 3
          c = -66
          d = 1

solutionFrom :: Integer
solutionFrom = -10

solutionTo :: Integer
solutionTo = 53

populationSize :: Int
populationSize = 4

crossoverCount :: Int
crossoverCount = 2

normalizeMax :: [Integer] -> [Integer]
normalizeMax scores = let minItem = minimum scores in
    if minItem < 0
        then map (+ abs minItem) scores
        else scores

normalizeMin :: [Integer] -> [Integer]
normalizeMin scores = let minItem = minimum inverted in
    if minimum inverted < 0
        then map (+ abs minItem) inverted
        else inverted
    where inverted = map (* (-1)) scores
