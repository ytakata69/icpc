#!/usr/bin/env runghc

-- ACM-ICPC 2022 Domestic Qualifier
-- Problem C: Training Schedule for ICPC

import Control.Monad (when)

main :: IO ()
main = do
    [n, m] <- map read . words <$> getLine :: IO [Int]
    when ((n, m) /= (0, 0)) $ do
        print $ solve n m
        main

solve :: Int -> Int -> Int
solve n 0 = n ^ 2
solve n m = snd . last . takeWhile (uncurry (<=)) $ zip (head gs : gs) gs
    where gs = map (splitRepose n m) [1..min m (n+1)]

-- split reposes into s segments
splitRepose :: Int -> Int -> Int -> Int
splitRepose n m s = gain - lose
    where s' = max (s - 1) 1  -- split trainings into s' segments
          (m1, r) = m `divMod` s
          lose = (m1 ^ 2) * (s - r) + ((m1 + 1) ^ 2) * r
          gain = (n - (s' - 1)) ^ 2 + (s' - 1)
