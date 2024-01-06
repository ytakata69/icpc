#!/usr/bin/env runghc

-- ACM-ICPC 2022 Domestic Qualifier
-- Problem B: Leave No One Behind

import Control.Monad (when, replicateM)
import Control.Applicative ((<$>))
import Data.List (sort)

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    when (n /= 0) $ do
        hs <- replicateM n (map read . words <$> getLine) :: IO [[Int]]
        print $ solve n hs
        main

type Hand = [Int]
type HandQueue = ([Hand], [Hand])  -- (queue, back_queue)

solve :: Int -> [Hand] -> Int
solve n hs =
    length . takeWhile (not . nullQueue) $ iterate play (hs', [])
    where dupHand [c1, c2] = c1 == c2
          nullQueue (q1, q2) = null q1 && null q2
          hs' = filter (not .dupHand) hs

play :: HandQueue -> HandQueue
play ([],  rest) = play (reverse rest, [])
play ([h], rest) = play (h : reverse rest, [])
play (h1 : h2 : rest1, rest2) = (q1, q2)
      where c1 : h1' = sort h1
            h2' = if c1 `elem` h2 then filter (/= c1) h2 else c1 : h2
            q1  = if null h2' then rest1 else h2' : rest1
            q2  = if null h1' then rest2 else h1' : rest2
