module GeneticLib (Genotype, Phenotype, Score, newPopulation, reproduce, getFitness, select, toPhenotype) where

import           Data.Char     (digitToInt)
import           Data.Functor  ((<&>))
import           Data.List     (foldl', maximumBy)
import           Data.Ord      (comparing)
import           Support       (randomDoubles, randomList, randomPairs)
import           System.Random

type Genotype = String
type Phenotype = Integer
type Score = Integer
type Probability = Double

gtypeLength :: Int
gtypeLength = 6

mutationP :: Probability
mutationP = 0.45

getFitness :: Integer -> (Phenotype -> Score) -> [Genotype] -> [Score]
getFitness offset fitFunc = map $ fitFunc . (+ offset) . toPhenotype

newPopulation :: (Phenotype, Phenotype) -> Int -> IO [Genotype]
newPopulation range size = map toGenotype <$> randomList range size

reproduce :: [Genotype] -> Int -> IO [Genotype]
reproduce pop crossCount = do
    pairs <- randomPairs pop crossCount
    nextPop <- generateAll pairs
    let npCount = length nextPop
    mps <- randomDoubles npCount
    pts <- randomList (1, gtypeLength) npCount
    return $ pop ++ [mutateCond gt pt (mp <= mutationP) | (gt, mp, pt) <- zip3 nextPop mps pts]

select :: [(Genotype, Score)] -> Int -> IO [Genotype]
select gs count = do
    let best = fst $ maximumBy (comparing snd) gs
    let r = roulette 0 $ getPs gs
    picks <- randomDoubles (count - 1)
    return $ best : [ gt | (gt, lo, hi) <- r, p <- picks, p >= lo && p < hi ]

roulette :: Probability -> [(Genotype, Probability)] -> [(Genotype, Probability, Probability)]
roulette _ []         = []
roulette acc (gp:gps) = (fst gp, acc, acc + snd gp) : roulette (acc + snd gp) gps

getPs :: [(Genotype, Score)] -> [(Genotype, Probability)]
getPs gs = [(g, fromIntegral s / total) | (g, s) <- gs]
    where total = fromIntegral $ sum [ snd gs' | gs' <- gs ]

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
    crossPoint <- randomRIO (1, gtypeLength)
    return [crossover g1 g2 crossPoint, crossover g2 g1 crossPoint]

crossover :: Genotype -> Genotype -> Int -> Genotype
crossover first second point = take point first ++ drop point second

toGenotype :: Phenotype -> Genotype
toGenotype n = padZero gtypeLength $ reverse $ helper n
    where padZero maxLength str = replicate (maxLength - length str) '0' ++ str
          helper 0 = ""
          helper n' | n' `mod` 2 == 1 = '1' : helper m
                    | otherwise       = '0' : helper m
                        where m = n' `div` 2

toPhenotype :: Genotype -> Phenotype
toPhenotype = foldl' (\acc x -> acc * 2 + (toInteger . digitToInt) x) 0
