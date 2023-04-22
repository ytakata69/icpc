#!/usr/bin/env runghc

-- ACM-ICPC 2021 Domestic Qualifier
-- Problem D: Handling out Ballons

import Control.Monad (when)
import Control.Applicative ((<$>))
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List (sort)

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    when (n /= 0) $ do
        bs <- map read . words <$> getLine :: IO [Int]
        print $ solve bs
        main

-- solve :: [Int] -> Int
solve bs = Map.fold max 0 $ Map.mapWithKey score dp
    where
        total = sum bs
        onethird = total `div` 3
        updater b dp = let n1 = Map.mapKeys (+ b) $
                                Map.filterWithKey (const . (< onethird)) dp
                           n2 = Map.mapWithKey
                                (\k -> Set.map (+b) . Set.filter (<= k)) dp
                       in  foldr (Map.unionWith Set.union) dp [n1, n2]
        dp = foldr updater (Map.singleton 0 (Set.singleton 0)) $ sort bs
        score  b1 bs = maximum . map (score' b1) $ Set.toList bs
        score' b1 b2 = minimum [b1, b2, total - b1 - b2]
