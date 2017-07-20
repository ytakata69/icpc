#!/usr/bin/python
# coding: utf-8

# ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
# Problem F: Folding a Ribbon

# Runnable on both Python 2 and 3,
# though the bit length of int type must be >= 64 in Python 2.

import sys, re

# 折りたたみ系列を返す。
# @param n 折りたたみ回数
# @param y 折りたたんだとき上から何枚目か (1..2**n)
# @param x 開いたとき左から何番目か (1..2**n)

def solve(n, y, x):
  # 展開しながら，その時点で印をつけた部分が上から何枚目かを記憶する
  # (左に開いても右に開いても，上から何枚目かは同じ)
  ys = [y]
  for i in range(n):
    half = 1 << (n - i - 1)  # 厚みの半分
    y = half - y + 1 if y <= half else y - half  # 半分より上か下かで変わる
    ys.insert(0, y)  # 先頭に追加

  if y != 1: raise  # 全部展開したら一番上になっているはず

  # 印をつけた部分が上から何枚目になるかをysの要素と一致させながら
  # 折りたたんでいく
  sequence = ''
  for i in range(n):
    thick = 1 << i            # 現在の厚み
    halfX = 1 << (n - i - 1)  # 現在の横幅の半分
    inRight = x > halfX       # 印をつけた部分は右半分にある
    if inRight: x -= halfX    # 右半分にある場合は中心からの距離に変える

    yTumble = thick - y + 1   # 印をつけた部分が動く場合
    yStay   = thick + y       # 印をつけた部分が動かない場合
    y = ys[i + 1]
    tumble = y == yTumble     # 印をつけた部分を動かすか否か
    sequence += 'R' if inRight == tumble else 'L'
    if tumble: x = halfX - x + 1
  return sequence

for line in sys.stdin:
  words = re.split(r'\s+', line.strip())  # 単語に分解
  n, i, j = [int(w) for w in words]       # intに変換
  if n == 0 and i == 0 and j == 0: break
  print(solve(n, i, j))
