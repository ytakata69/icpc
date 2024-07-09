#!/usr/bin/env runghc

-- ICPC Domestic Qualifier
-- Problem B - Overtaking

import Control.Monad (when)
import Control.Applicative ((<$>))

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    when (n /= 0) $ do
        as <- map read . words <$> getLine :: IO [Int]
        bs <- map read . words <$> getLine :: IO [Int]
        print $ overtaking as bs
        main

overtaking :: [Int] -> [Int] -> Int
overtaking as bs = length ovrs
    where
        acc_as = scanl (+) 0 as  -- 累積和
        acc_bs = scanl (+) 0 bs
        sgns = [compare a b | (a,b) <- zip acc_as acc_bs, a /= b]
        ovrs = [1 | (a,b) <- zip sgns (tail sgns), a /= b] -- 符号変化位置
