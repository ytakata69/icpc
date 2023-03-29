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
solve w h xyn =
    w + h == (Set.size $ dfs Set.empty 1)  -- has visited all?
    where xy = [(x, -y) | [x, y, _] <- xyn]  -- edges among w+h vertices
          adj = xy ++ map swap xy  -- make the edges bidirectional
          dfs visited x =
              let ys = filter (not.(`Set.member` visited)) $ lookupAll x adj
              in  foldl dfs (Set.insert x visited) ys

lookupAll :: Eq k => k -> [(k,v)] -> [v]
lookupAll k = map snd . filter ((== k) . fst)
