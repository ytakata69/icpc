#!/usr/bin/python
# coding: utf-8

# ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
# A: Taro's shopping

import sys

# @param n  商品数
# @param m  金額
# @param a  各商品の価格
def solve(n, m, a):
  max = 0;
  # すべての組(i,j)を調べる
  for i in range(n):
    for j in range(i + 1, n):
      p = a[i] + a[j]
      if p <= m and max < p:
        max = p
  print("%d" % max if max > 0 else "NONE")


def splitIntoNums(line):
  words = line.strip().split()            # split into words
  return [int(w) for w in words]          # convert to integers

while True:
  n, m = splitIntoNums(sys.stdin.readline())
  if n == 0 and m == 0: break
  a = splitIntoNums(sys.stdin.readline())
  solve(n, m, a)
