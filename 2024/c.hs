#!/usr/bin/env runghc

-- ICPC Domestic Qualifier
-- Problem C - Honeycomb Distance

import Control.Monad (replicateM_)
import Control.Applicative ((<$>))

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    replicateM_ n $ do
        [x,y] <- map read . words <$> getLine :: IO [Int]
        print $ honeycomb x y

honeycomb :: Int -> Int -> Int
honeycomb x y | x <  0     = honeycomb (-x) (-y)
              | y >= 0     = x + y
              | x + y >= 0 = x
              | otherwise  = -y
