#!/usr/bin/env runghc

-- ACM-ICPC 2022 Domestic Qualifier
-- Problem D: Audience Queue

import Control.Monad (when, replicateM)
import Control.Applicative ((<$>))
import qualified Data.Map as Map
import Data.Maybe (fromJust)
import Data.Function (on)

main :: IO ()
main = do
    [n, k] <- map read . words <$> getLine :: IO [Int]
    when ((n, k) /= (0, 0)) $ do
        [ss, ts] <- replicateM 2 (map read . words <$> getLine) :: IO [[Int]]
        print $ solve n k ss ts
        main

solve :: Int -> Int -> [Int] -> [Int] -> Integer
solve n k ss ts
    | nmustcut + 1 > k = 0
    | conflict         = 0
    | otherwise        = on sumComb fromIntegral m l
    where
        -- mapping from ticket to entering order
        entrankmap = Map.fromList $ zip ts [0..]
        entrank t = fromJust $ Map.lookup t entrankmap

        -- bool list indicating the positions in the queue to cut
        mustcut = zipWith (on (>) entrank) (head ss : ss) ss

        -- mapping from ticket to queuing group
        groupmap = Map.fromList . zip ss . scanl1 (+) $ map fromEnum mustcut
        group s = fromJust $ Map.lookup s groupmap

        -- prefix-max list & bool list indicating the ticket not to cut 
        premax = scanl max 0 ts
        notcut = zipWith (>) premax ts
        conflict = or . zipWith (&&) notcut $ zipWith (on (/=) group) premax ts

        -- the number of positions
        nmustcut = sum $ map fromEnum mustcut
        nnotcut  = sum $ map fromEnum notcut
        m = n - 1 - nmustcut - nnotcut  -- num of candidates for cutting
        l = k - 1 - nmustcut            -- num of remaining gates

-- sumComb m l = sum_{i=0}^{l} comb(m, i)
sumComb :: Integer -> Integer -> Integer
sumComb m l = foldl mplus 0 $ scanl nextComb 1 [1..min m l]
    where nextComb comb i = (comb * (m - i + 1)) `mdiv` i

-- modular arithmetics

modulus = 998244353

mplus :: Integer -> Integer -> Integer
mplus x y = (x + y) `mod` modulus

mpow :: Integral p => Integer -> p -> Integer
mpow x 0 = 1
mpow x 1 = x `mod` modulus
mpow x p
    | even p    = mpow sqx (p `div` 2) `mod` modulus
    | otherwise = (xm * mpow sqx ((p-1) `div` 2)) `mod` modulus
    where xm  = x `mod` modulus
          sqx = xm * xm

mdiv :: Integer -> Integer -> Integer
mdiv n d = (n * mpow d (modulus-2)) `mod` modulus
