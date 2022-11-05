{-# OPTIONS_GHC -Wno-type-defaults #-}
module Main(main) where

import           Data.Foldable (maximumBy, minimumBy)
import           Data.Ord      (comparing)
import           GeneticLib
import           System.IO

f :: Integer -> Integer -> Integer -> Integer -> Integer -> Integer
f a b c d x = a + b * x + c * (x ^ 2) + d * (x ^ 3)

rangeFrom :: Integer
rangeFrom = -10

rangeTo :: Integer
rangeTo = 53

main :: IO ()
main = do
    putStrLn "Генетический алгоритм поиска значений функции a + bx + cx^2 + dx^3"
    fn <- readFunc
    let offset = if rangeFrom < 0 then (-1) * rangeFrom else 0 :: Integer
    let iOffset = (-1) * offset
    populationSize <- readInt "Число особей в популяции: "
    crossCount <- readInt "Число скрещиваний в популяции: "
    maxIters <- readInt "Максимальное количество итераций: "
    minMode <- readMode
    let runParams = (iOffset, minMode, fn, crossCount)

    putStrLn "-----------"
    initial <- newPopulation (rangeFrom + offset, rangeTo + offset) populationSize
    let values = getFitness iOffset fn initial
    let best = getBest minMode initial values
    printGen offset initial values best 0
    putStrLn "-----------"
    runGenerations runParams initial maxIters maxIters

type RunParams = (Integer, Bool, Integer -> Integer, Int)

runGenerations :: RunParams -> [Genotype] -> Int -> Int -> IO ()
runGenerations _ _ _ 0                    = return ()
runGenerations rp@(o, mm, fn, cc) pop m n = do
    let normalize = if mm then normalizeMin else normalizeMax
    let generation =  m - n + 1

    nextGen <- reproduce pop cc
    let scores = normalize $ getFitness o fn nextGen
    selected <- select (zip nextGen scores) (length pop)
    let values = getFitness o fn selected
    let best = getBest mm selected values

    printGen o selected values best generation
    putStrLn "-----------"

    if allSame selected
        then return ()
        else runGenerations rp selected m (n - 1)

getBest :: Bool -> [Genotype] -> [Score] -> (Genotype, Score)
getBest minMode pop scores = bestIf (comparing snd) (zip pop scores)
    where bestIf = if minMode then minimumBy else maximumBy

allSame :: (Eq a) => [a] -> Bool
allSame xs = all (== head xs) (tail xs)

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

---- IO helpers ----

printGen :: Integer -> [Genotype] -> [Score] -> (Genotype, Score) -> Int -> IO ()
printGen offset pop values best gen = do
    putStrLn $ "Поколение " ++ show gen ++ ":      " ++ show pop
    putStrLn $ "Значения функции: " ++ show values
    putStrLn $ "Лучшее решение  : x = " ++ show (offset + toPhenotype (fst best)) ++ " (генотип: " ++ show (fst best) ++ ")"
    putStrLn $ "Лучшее значение : " ++ show (snd best)

readFunc :: IO (Integer -> Integer)
readFunc = do
    putStrLn "Укажите параметры целевой функции:"
    iA <- prompt "a = "
    iB <- prompt "b = "
    iC <- prompt "c = "
    iD <- prompt "d = "
    let a = read iA
    let b = read iB
    let c = read iC
    let d = read iD
    let func = f a b c d
    putStrLn $ "f x = " ++ show a ++ " + " ++ show b ++ "x + "
                ++ show c ++ "(x ^ 2) + " ++ show d ++ "(x ^ 3)"
    return func

readMode :: IO Bool
readMode = do
    mode <- prompt "Введите 0, чтобы минимизировать функцию, 1 - чтобы максимизировать: "
    return $ read mode == 0

readInt :: String -> IO Int
readInt msg = do
    num <- prompt msg
    return $ read num

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine
