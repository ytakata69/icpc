#!/usr/bin/env runghc

-- ACM-ICPC 2021 Domestic Qualifier
-- Problem D: Handling out Ballons

import Control.Monad (when)
import Control.Applicative ((<$>))
import qualified Data.Set as Set

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    when (n /= 0) $ do
        bs <- map read . words <$> getLine :: IO [Int]
        print $ solve bs
        main

solve :: [Int] -> Int
solve bs = Set.findMax $ Set.map score dp
    where
        updater dp b = let n1 = Set.map (normpair . addleft b) dp
                           n2 = Set.map (addright b) dp
                       in  Set.unions [dp, n1, n2]
        dp = foldl updater (Set.singleton (0, 0)) bs
        total = sum bs
        score (b1, b2) = min b1 (total - b1 - b2)

-- utilities for pairs

normpair (a, b) = if a <= b then (a, b) else (b, a)
addleft  c (a, b) = (a + c, b)
addright c (a, b) = (a, b + c)
