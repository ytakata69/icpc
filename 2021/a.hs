#!/usr/bin/env runghc

-- ACM-ICPC 2021 Domestic Qualifier
-- Problem A: Marbles Tell Your Lucky Number

import Control.Monad (when)
import Data.List (sort)

main :: IO ()
main = do
    cups <- map read . words <$> getLine :: IO [Int]
    when (cups /= [0, 0, 0, 0]) $ do
        print $ luckey cups
        main

luckey :: [Int] -> Int
luckey cups = luckey' . filter ((>0) . fst) $ zip cups [0..]

luckey' :: [(Int, Int)] -> Int
luckey' cs =
    let ((m, i) : cs') = sort cs in
        if null cs' then m
        else luckey' $ (m, i) : [(m' - m, j) | (m', j) <- cs', m' > m]
