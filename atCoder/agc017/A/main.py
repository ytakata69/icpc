# coding: utf-8

# AtCoder Grand Contest 017
# A - Biscuits

import sys, re

def solve(N, P, A):
  m = len([a for a in A if a % 2 != 0])  # the num of odd numbers in A
  if m > 0:
    return 2 ** (N-1)
  else:
    return 2 ** N if P == 0 else 0

def splitIntoNums(line):
  words = re.split(r'\s+', line.strip())
  return [int(w) for w in words]  # convert to int

while True:
  l1 = sys.stdin.readline()
  l2 = sys.stdin.readline()
  if not l1: break
  N, P = splitIntoNums(l1)
  A    = splitIntoNums(l2)
  if len(A) != N: raise
  print(solve(N, P, A))


# 以降, コメント:
#
# 奇数枚のビスケットが入っている袋数を m とする
# (偶数枚のビスケットが入っている袋数は N-m).
#
# 補題:
# m > 0 とする. m個の物のうち偶数個を選ぶ選び方は 2 ** (m-1) 通り,
# 奇数個を選ぶ選び方も 2 ** (m-1) 通りである.
# (mに関する数学的帰納法で示せる.)
#
# m > 0 の場合:
#   P == 0 のとき:
#     奇数枚のビスケットが入っている袋を偶数個選ぶ必要がある.
#     偶数枚のビスケットが入っている袋は何個選んでもよい.
#     m個の袋から偶数個選ぶ選び方は 2 ** (m-1) 通り.
#     N-m個の袋から任意の個数選ぶ選び方は 2 ** (N-m) 通り.
#     従って, 選び方は全部で 2 ** (N-1) 通り.
#   P == 1 のときも同様に, 選び方は全部で 2 ** (N-1) 通り.
# m == 0 の場合:
#   どのように選んでもビスケットは偶数枚になる. 従って,
#    P == 0 ならば 2 ** N 通り, P == 1 ならば 0 通り.
