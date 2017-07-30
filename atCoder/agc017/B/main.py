# coding: utf-8

# AtCoder Grand Contest 017
# B - Moderate Differences

# This code is compatible with both Python 2 and 3;
# however, in Python 2, the int type must be 64 bits,
# due to the range of the parameters of this problem.

import sys, re

def solve(N, A, B, C, D):
  for i in range(N):
    upper = i * D - (N - 1 - i) * C
    lower = i * C - (N - 1 - i) * D
    if lower <= B - A and B - A <= upper:
      return True
  return False

for line in sys.stdin:
  words = re.split(r'\s+', line.strip())
  N, A, B, C, D = [int(w) for w in words]  # convert to int
  result = solve(N, A, B, C, D)
  print('YES' if result else 'NO')


# 以降, コメント:
#
# 左から第0マス, 第1マス, ..., 第N-1マスと呼ぶ.
# 第0マスに0, 第N-1マスにB-Aを書いても結果 (YES or NO) は変わらない
# ので, 以降, 青橋君は第0マスに0を書くものと仮定する.
#
# 第kマス (0 <= k < N) に書くことのできる数は,
#   (i * C - (k - i) * D)以上 (i * D - (k - i) * C)以下
# (ただし 0 <= i <= k) の整数である
# (これらの数は書くことができる, かつこれら以外は書くことができない).
# このことはkに関する数学的帰納法で示すことができる.
#
# 従って, 0以上N-1以下のある i について
# B-A が (i * C - (N-1-i) * D)以上 (i * D - (N-1-i) * C)以下のとき,
# かつそのときのみ, 青橋君は第N-1マスにB-Aを書くことができる.
