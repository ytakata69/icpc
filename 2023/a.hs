#!/usr/bin/env runghc

-- ICPC 2023 domestic qualifier
-- Problem A - Which Team Should Receive the Sponsor Prize?

-- 入力を受け取って結果をprintするだけのmain関数
main :: IO ()
main = do n <- read <$> getLine
          if n == 0
             then return ()
             else do as <- map read . words <$> getLine
                     print $ solve as
                     main

-- アルゴリズム本体
solve :: [Int] -> Int
solve as = snd . minimum $ zip [abs (a - 2023) | a <- as] [1..]
