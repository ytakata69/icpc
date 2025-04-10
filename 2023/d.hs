#!/usr/bin/env runghc

-- ICPC 2023 domestic qualifier
-- Problem D - Efficient Problem Set

-- Usage:
--   $ runghc d.hs < D1 | diff - D1.ans
-- or
--   $ ghc -O d.hs -o d
--   $ ./d < D1 | diff - D1.ans
--
-- ghc -O だと審判データ 1ファイルに約5秒. runghc だと約20秒.
-- (iMac-2017, 2.3GHz Dual-core Intel Core i5, 16GB RAM,
-- macOS Ventura 13.7.5, GHC 9.4.8)

import qualified Data.Set as Set  -- 集合を表すデータ構造
import Data.Maybe (fromJust)      -- Justを取り除く関数

-- 入力を受け取って結果をprintするだけのmain関数
main :: IO ()
main = do n <- read <$> getLine
          if n == 0
             then return ()
             else do s <- getLine
                     print $ solve n s
                     main

-- ♾️ の代わりに使う定数
inf = maxBound :: Int

-- アルゴリズム本体
solve :: Int -> String -> Int
solve n s =
    if null n_elems then max_n_elem else head n_elems
  where
    -- o,x の列を位置 (= 作るべき数) の列に変換
    is = map snd $ filter (\(c, i) -> c == 'o') (zip s [0..])
    -- iを作れるならn - iも作れるはずなので, それも作るべき数として加える
    js = map (n -) is
    -- Set に変換 (= 作るべき数の集合)
    set = Set.union (Set.fromList is) (Set.fromList js)

    -- 作るべき数をすべて作るのに必要な最小要素数 (要素k個で最大2^k個作れる)
    min_n_elem = ceilLog2 (Set.size set)
    -- nの2進表現のビット数. 2の累乗をmax_n_elem個持てば0〜nをすべて作れる
    max_n_elem = ceilLog2 (n + 1)

    -- 0の次に小さい作るべき数. これ以下の要素が必ず必要
    max_min = fromJust (Set.lookupGT 0 set)

    -- 小さいn_elemから順に, 要素数n_elemでsetをカバーできるか試す.
    -- n_elem == max_n_elemのときはカバーできることが明らかなので,
    -- max_n_elem - 1までしか試さない.
    n_elems = filter (canCover set n 1 max_min) [min_n_elem .. max_n_elem - 1]

-- 合計がn, 最小要素がmin_min以上max_min以下, 要素数 n_elem の数の列のうち,
-- 集合setの要素をすべて作れるものがあるか否か
canCover :: Set.Set Int -> Int -> Int -> Int -> Int -> Bool
canCover set n min_min max_min n_elem =
    not . null . filter (set `Set.isSubsetOf`) $
    map subsetSums (permut n min_min max_min n_elem [])

-- 合計がn, 最小要素がmin_min以上max_min以下, 要素数 n_elem の数の列すべての列.
-- n_elemを1ずつ減らしながら再帰. accに作成中の列を保存する.
permut :: Int -> Int -> Int -> Int -> [Int] -> [[Int]]
permut n min_min max_min n_elem acc
  | n_elem <= 1 = [n : acc]
  | otherwise   = concat $  -- 各iに対する列を結合
      -- min_min以上max_min'以下のiを1個選び, それを最小要素として残りを生成
      map (\i -> permut (n - i) i inf (n_elem - 1) (i : acc))
          [max_min', max_min' - 1 .. min_min]
  where
      max_min' = min max_min (n `div` n_elem)  -- 最小要素は必ず平均以下

-- xsのすべての部分和の集合
subsetSums :: [Int] -> Set.Set Int
subsetSums xs =
    -- 集合{0}から始めて, 現在の集合の要素にxsの要素xを加算して集合に加える
    foldr (\x set -> Set.union set (Set.map (+x) set)) (Set.singleton 0) xs

-- 2 ^ (i - 1) < n <= 2 ^ i である i
ceilLog2 :: Int -> Int
ceilLog2 n =
    -- *2 を繰り返し, n以上に初めてなったときの繰り返し回数を答える
    fst . head . dropWhile ((< n).snd) $ iterate (\(i,p) -> (i+1, p*2)) (0, 1)
