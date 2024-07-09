#!/usr/bin/env runghc

-- ICPC Domestic Qualifier
-- Problem A - Snacks within 300 Yen

import Control.Monad (when)
import Control.Applicative ((<$>))

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    when (n /= 0) $ do
        as <- map read . words <$> getLine :: IO [Int]
        print $ shopping as
        main

shopping :: [Int] -> Int
shopping = foldl (\s a -> if s + a <= 300 then s + a else s) 0
