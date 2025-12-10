#!/usr/bin/env runghc

-- ICPC Domestic Qualifier
-- Problem A - Snacks within 300 Yen

-- 入力を受け取って結果をprintするだけのmain関数
main :: IO ()
main = do n <- read <$> getLine
          if n == 0
             then return ()
             else do prices <- map read . words <$> getLine
                     print $ solve prices
                     main

-- アルゴリズム本体
solve :: [Int] -> Int
solve = foldl buyGreedy 0
  where
    buyGreedy bought price
      | bought + price <= 300 = bought + price
      | otherwise             = bought
