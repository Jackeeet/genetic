module Support(randomPairs) where

import           System.Random

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
