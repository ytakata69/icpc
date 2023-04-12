#!/usr/bin/env runghc

-- ACM-ICPC 2021 Domestic Qualifier
-- Problem A: Marbles Tell Your Lucky Number

import Control.Monad (when)
import Control.Applicative ((<$>))
import Data.List (sort)

main :: IO ()
main = do
    cups <- map read . words <$> getLine :: IO [Int]
    when (cups /= [0, 0, 0, 0]) $ do
        print $ lucky cups
        main

lucky :: [Int] -> Int
lucky cups = lucky' . filter ((>0) . fst) $ zip cups [0..]

lucky' :: [(Int, Int)] -> Int
lucky' cs =
    let ((m, i) : cs') = sort cs in
        if null cs' then m
        else lucky' $ (m, i) : [(m' - m, j) | (m', j) <- cs', m' > m]
