#!/usr/bin/env runghc

-- ACM-ICPC 2022 Domestic Qualifier
-- Problem B: Leave No One Behind

import Control.Monad (when, replicateM)
import Data.List (sort)

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    when (n /= 0) $ do
        hs <- replicateM n (map read . words <$> getLine) :: IO [[Int]]
        print $ solve hs
        main

type Hand = [Int]
type Position = [Hand]

solve :: Position -> Int
solve hs =
    length . takeWhile (not . null) $ iterate play iniths
    where iniths = filter (not . null) $ map discardDup hs

play :: Position -> Position
play [] = []
play (h1 : h2 : hs) = filter (not . null) ((h2' : hs) ++ [h1'])
    where (c1 : h1') = sort h1
          h2'  = if c1 `elem` h2 then filter (/= c1) h2 else c1 : h2

discardDup :: Hand -> Hand
discardDup h@[c1, c2]
    | c1 == c2  = []
    | otherwise = h
