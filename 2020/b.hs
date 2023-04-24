#!/usr/bin/env runghc

-- ACM-ICPC 2020 Domestic Qualifier
-- Problem B: Contact Tracer

import Control.Monad (when, replicateM)
import Control.Applicative ((<$>))
import qualified Data.Set as Set

main :: IO ()
main = do
    [m, n, p] <- map read . words <$> getLine :: IO [Int]
    when ((m, n, p) /= (0, 0, 0)) $ do
        ab <- replicateM n (map read . words <$> getLine) :: IO [[Int]]
        print $ solve p ab
        main

solve :: Int -> [[Int]] -> Int
solve p = Set.size . foldl updater (Set.singleton p)
    where updater set [a, b] =
              if Set.member a set then Set.insert b set else
              if Set.member b set then Set.insert a set else set
