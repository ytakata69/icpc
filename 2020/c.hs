#!/usr/bin/env runghc

-- ACM-ICPC 2020 Domestic Qualifier
-- Problem C: Luggage

import Control.Monad (when)
import Control.Applicative ((<$>))

main :: IO ()
main = do
    p <- read <$> getLine :: IO Int
    when (p /= 0) $ do
        print $ solve p
        main

solve :: Int -> Int
solve p = minimum $ map cost ws
    where ws = filter (p `divisibleBy`) $ takeWhile (\w -> w ^ 3 <= p) [1..]
          h w = let hd = p `div` w
                    hmax = truncate . sqrt $ fromIntegral hd
                in  head . filter (hd `divisibleBy`) $ map (hmax -) [0..]
          cost w = let h' = h w ; d = (p `div` w) `div` h' in d + h' + w

divisibleBy :: Int -> Int -> Bool
divisibleBy a b = a `mod` b == 0
