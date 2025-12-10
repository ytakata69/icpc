#!/usr/bin/env runghc

-- ICPC Domestic Qualifier
-- Problem B - Overtaking

-- 入力を受け取って結果をprintするだけのmain関数
main :: IO ()
main = do n <- read <$> getLine
          if n == 0
             then return ()
             else do as <- map read . words <$> getLine
                     bs <- map read . words <$> getLine
                     print $ solve as bs
                     main

-- アルゴリズム本体
solve :: [Int] -> [Int] -> Int
solve as bs =
    -- 符号が変化する位置の個数
    length [1 | (s1, s2) <- zip signs (tail signs), s1 /= s2]
  where
    acc_a = scanl (+) 0 as  -- 累積和のリスト
    acc_b = scanl (+) 0 bs  -- 〃
    -- 差の符号のリスト（ただし0を除去）
    signs = [signum (a - b) | (a, b) <- zip acc_a acc_b, a /= b]
