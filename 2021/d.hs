#!/usr/bin/env runghc

-- ACM-ICPC 2021 Domestic Qualifier
-- Problem D: Handling out Ballons

import Control.Monad (when)
import Control.Applicative ((<$>))
import qualified Data.Set as Set
import Data.Function (on)

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    when (n /= 0) $ do
        bs <- map read . words <$> getLine :: IO [Int]
        print $ solve bs
        main

solve :: [Int] -> Int
solve bs = maximum . map score $ Set.toList dp
    where
        total = sum bs
        onethird = total `div` 3
        updater dp b = let ls = Set.toList dp
                           n1 = map (normpair . addleft b)
                                                 $ filter (ltleft  onethird) ls
                           n2 = map (addright b) $ filter (ltright onethird) ls
                       in  Set.union dp $ on Set.union Set.fromList n1 n2
        dp = foldl updater (Set.singleton (0, 0)) bs
        score (b1, b2) = min b1 (total - b1 - b2)

-- utilities for pairs

normpair (a, b) = if a <= b then (a, b) else (b, a)
addleft  c (a, b) = (a + c, b)
addright c (a, b) = (a, b + c)
ltleft  c = (< c) . fst
ltright c = (< c) . snd
