#!/usr/bin/env runghc

-- ACM-ICPC 2022 Domestic Qualifier
-- Problem B: Leave No One Behind

import Control.Monad (when, replicateM)
import Control.Applicative ((<$>))
import Data.List (sort)
import qualified Data.Map as Map

main :: IO ()
main = do
    n <- read <$> getLine :: IO Int
    when (n /= 0) $ do
        hs <- replicateM n (map read . words <$> getLine) :: IO [[Int]]
        let hs' = Map.fromList $ zip [0..] hs
        print $ solve n (0, hs')
        main

type Hand = [Int]
type HandMap = Map.Map Int Hand
type Position = (Int, HandMap)

solve :: Int -> Position -> Int
solve n (p, hs) =
    length . takeWhile ((> 0) . Map.size . snd) $ iterate (play n) (p, iniths)
    where iniths = Map.filter (not . null) $ Map.map discardDup hs

play :: Int -> Position -> Position
play n (p, hs)
    | Map.size hs == 0 = (p, hs)
    | not (Map.member p hs) = play n (nextPlayer n p hs, hs)
    | otherwise = (nextPlayer n p newhs, newhs)
      where q = nextPlayer n p hs
            Just h1 = Map.lookup p hs
            Just h2 = Map.lookup q hs
            (c1 : h1') = sort h1
            h2' = if c1 `elem` h2 then filter (/= c1) h2 else c1 : h2
            newhs = insertOrDelete q h2' . insertOrDelete p h1' $ hs

nextPlayer :: Int -> Int -> HandMap -> Int
nextPlayer n p hs = if Map.member q hs then q else nextPlayer n q hs
    where q = (p + 1) `mod` n

discardDup :: Hand -> Hand
discardDup h@[c1, c2]
    | c1 == c2  = []
    | otherwise = h

insertOrDelete :: Ord k => k -> [a] -> Map.Map k [a] -> Map.Map k [a]
insertOrDelete k v = if null v then Map.delete k else Map.insert k v
