#!/usr/bin/env runghc

-- ACM-ICPC 2022 Domestic Qualifier
-- Problem A: Counting Peaks of Infection

import Control.Monad (when)
import Control.Applicative ((<$>))

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    when (n /= 0) $ do
        vs <- map read . words <$> getLine :: IO [Int]
        print $ numPeak vs
        main

numPeak :: [Int] -> Int
numPeak vs =
    length . filter (\(a,b,c) -> a < b && b > c)
        $ zip3 vs (tail vs) (tail $ tail vs)
