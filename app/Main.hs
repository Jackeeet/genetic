module Main(main) where

import           System.Random

main :: IO ()
main = do
    pop <- newPopulation populationSize
    print pop

solutionRange :: (Int, Int)
solutionRange = (-10, 53)

populationSize :: Int
populationSize = 4

crossoverCount :: Int
crossoverCount = 2

mutationCount :: Int
mutationCount = 1

newPopulation :: Int -> IO [String]
newPopulation size = fmap (map intToGene) (take size . randomRs solutionRange <$> newStdGen)

geneLength :: Int
geneLength = 7

intToGene :: Int -> String
intToGene 0 = "0"
intToGene n | n > 0 = '0' : binary n
            | otherwise = '1' : binary (-1 * n)
    where binary num = padZero (geneLength - 1) (reverse $ helper num)
          helper 0 = ""
          helper n' | n' `mod` 2 == 1 = '1' : helper m
                    | otherwise = '0' : helper m
                        where m = n' `div` 2

padZero :: Int -> String -> String
padZero maxLength str = replicate (maxLength - length str) '0' ++ str
