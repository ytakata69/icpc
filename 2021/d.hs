#!/usr/bin/env runghc

-- ACM-ICPC 2021 Domestic Qualifier
-- Problem D: Handling out Ballons

import Control.Monad (when)
import Control.Applicative ((<$>))
import qualified Data.Set as Set
import Data.Function (on)
import Data.List (sort)

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    when (n /= 0) $ do
        bs <- map read . words <$> getLine :: IO [Int]
        print $ solve bs
        main

solve :: [Int] -> Int
solve bs = maximum . map (score . unpair) $ Set.toList dp
    where
        total = sum bs
        onethird = total `div` 3
        updater b dp = let ls = Set.toList dp
                           n1 = map (normpair . addleft b)
                                                 $ filter (ltleft  onethird) ls
                           n2 = map (addright b) $ filter (ltright onethird) ls
                       in  Set.union dp $ on Set.union Set.fromList n1 n2
        dp = foldr updater (Set.singleton $ pair (0, 0)) $ sort bs
        score (b1, b2) = min b1 (total - b1 - b2)

-- utilities for pairs

base = 4096
pair (a, b) = a * base + b
unpair ab = (ab `div` base, ab `mod` base)

addleft  c ab = ab + (c * base)
addright c ab = ab + c
normpair = pair . normpair' . unpair
normpair' (a, b) = if a <= b then (a, b) else (b, a)

ltleft  c ab = ab < c * base
ltright c ab = ab `mod` base < c
