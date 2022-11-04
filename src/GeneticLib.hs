module GeneticLib (newPopulation, nextGeneration, getFitness, select) where

import           Data.Char     (digitToInt)
import           Data.Functor  ((<&>))
import           Data.List     (foldl')
import           Support       (randomPairs)
import           System.Random

type Genotype = String
type FitFunc = (Int -> Int)

geneLength :: Int
geneLength = 6

mutProbability :: Double
mutProbability = 0.3

getFitness :: Int -> FitFunc -> [Genotype] -> [Int]
getFitness offset fitFunc = map $ fitFunc . (+offset) . toInt

newPopulation :: (Int, Int) -> Int -> IO [Genotype]
newPopulation range size = fmap (map toGenotype) (take size . randomRs range <$> newStdGen)

nextGeneration :: [Genotype] -> Int -> IO [Genotype]
nextGeneration pop crossCount = do
    pairs <- randomPairs pop crossCount
    nextPop <- generateAll pairs
    let npCount = length nextPop
    mps <- take npCount . randomRs (0.0 :: Double, 1.0 :: Double) <$> newStdGen
    points <- take npCount . randomRs (1, geneLength) <$> newStdGen
    return $ pop ++ [mutateCond gt point (prob <= mutProbability) | (gt, prob, point) <- zip3 nextPop mps points]

select :: [Genotype] -> [Genotype]
select pop = undefined

mutateCond :: Genotype -> Int -> Bool -> Genotype
mutateCond gt point apply = if apply then mutate gt point else gt

mutate :: Genotype -> Int -> Genotype
mutate gt point = take (point - 1) gt ++ [invert $ gt !! (point - 1)] ++ drop point gt
    where invert '0' = '1'
          invert '1' = '0'
          invert _   = undefined

generateAll :: [(Genotype, Genotype)] -> IO [Genotype]
generateAll pairs = mapM generate pairs <&> concat
--generateAll pairs = mapM generate pairs >>= (return . concat)

generate :: (Genotype, Genotype) -> IO [Genotype]
generate (g1, g2) = do
    crossPoint <- randomRIO (1, geneLength)
    return [crossover g1 g2 crossPoint, crossover g2 g1 crossPoint]

crossover :: Genotype -> Genotype -> Int -> Genotype
crossover first second point = take point first ++ drop point second

toGenotype :: Int -> Genotype
toGenotype 0 = "0"
toGenotype n = binary n
    where binary num = padZero geneLength $ reverse $ helper num
          padZero maxLength str = replicate (maxLength - length str) '0' ++ str
          helper 0 = ""
          helper n' | n' `mod` 2 == 1 = '1' : helper m
                    | otherwise = '0' : helper m
                        where m = n' `div` 2

toInt :: Genotype -> Int
toInt = foldl' (\acc x -> acc * 2 + digitToInt x) 0
