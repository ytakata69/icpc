#!/usr/bin/env runghc

-- ACM-ICPC 2020 Domestic Qualifier
-- Problem A: Count Down Up 2020

import Control.Monad (when)
import Control.Applicative ((<$>))

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    when (n /= 0) $ do
        ds <- map read . words <$> getLine :: IO [Int]
        print $ solve ds
        main

solve :: [Int] -> Int
solve (2:0:2:0:ds) = 1 + solve (2:0:ds)
solve (a:ds) = solve ds
solve _ = 0
