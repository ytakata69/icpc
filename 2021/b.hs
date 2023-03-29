#!/usr/bin/env runghc

-- ACM-ICPC 2021 Domestic Qualifier
-- Problem B: Hundred-Cell Calculation Puzzles

import Control.Monad (when, replicateM)
import Data.Tuple (swap)
import qualified Data.Set as Set

main :: IO ()
main = do
    [w, h] <- map read . words <$> getLine :: IO [Int]
    when ((w, h) /= (0, 0)) $ do
        let k = w + h - 1
        xyn <- replicateM k (map read . words <$> getLine) :: IO [[Int]]
        putStrLn $ if solve w h xyn then "YES" else "NO"
        main

solve :: Int -> Int -> [[Int]] -> Bool
solve w h xyn = dfs Set.empty [1]
    where xy = [(x, y + w) | [x, y, _] <- xyn]
          adj = xy ++ map swap xy  -- bidirectional adjacency list
          dfs visited [] = Set.size visited == w + h  -- has visited all?
          dfs visited (x : stack) =
              let ys = filter (not.(`Set.member` visited)) $ lookupAll x adj
              in  dfs (Set.insert x visited) (ys ++ stack)

lookupAll :: Eq k => k -> [(k,v)] -> [v]
lookupAll k = map snd . filter ((== k) . fst)
