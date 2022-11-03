module GeneticLib (newPopulation, scores) where

import           Data.Char     (digitToInt)
import           Data.List     (foldl')
import           System.Random

type Genotype = String
type Gene = Char
type FitFunc = (Int -> Int)

geneLength :: Int
geneLength = 7

scores :: FitFunc -> [Genotype] -> [Int]
scores fitFunc = map $ fitFunc . toInt

newPopulation :: (Int, Int) -> Int -> IO [Genotype]
newPopulation range size = fmap (map toGenotype) (take size . randomRs range <$> newStdGen)

crossover :: Genotype -> Genotype -> Int -> Genotype
crossover first second point = take point first ++ drop point second

mutate :: Genotype -> Int -> Genotype
mutate gt point = take (point - 1) gt ++ [invert $ gt !! (point - 1)] ++ drop point gt  
    where invert '0' = '1'
          invert '1' = '0'
          invert _ = undefined


toGenotype :: Int -> Genotype
toGenotype 0 = "0"
toGenotype n | n > 0 = '0' : binary n
            | otherwise = '1' : binary (-1 * n)
    where binary num = padZero (geneLength - 1) (reverse $ helper num)
          helper 0 = ""
          helper n' | n' `mod` 2 == 1 = '1' : helper m
                    | otherwise = '0' : helper m
                        where m = n' `div` 2

toInt :: Genotype -> Int
toInt [] = undefined -- to make the compiler stop complaining about non-exhaustive pattern matching
toInt (g:gs) | g == '0' = binToInt gs
                 | g == '1' = (-1) * binToInt gs
                 | otherwise = undefined -- same here
    where binToInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0

padZero :: Int -> String -> String
padZero maxLength str = replicate (maxLength - length str) '0' ++ str
