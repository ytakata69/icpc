#!/usr/bin/python
# coding: utf-8

# ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
# C: A Garden with Ponds

import sys, re

def solve(d, w, e):
  m = 0;
  for dd in range(3, d + 1):
    for y in range(0, d - dd + 1):
      for ww in range(3, w + 1):
        for x in range(0, w - ww + 1):
          f = fence(e, x, y, ww, dd)
          p = pond (e, x, y, ww, dd, f)
          m = max(m, p)
  return m

def fence(e, x, y, w, d):
  m = e[y][x]
  m = min(m, min(e[y][x: x + w]))
  m = min(m, min(e[y + d - 1][x: x + w]))
  for i in range(y, y + d):
    m = min(m, e[i][x])
    m = min(m, e[i][x + w - 1])
  return m

def pond(e, x, y, w, d, f):
  s = 0
  for i in range(y + 1, y + d - 1):
    ee = e[i][x + 1: x + w - 1]
    if max(ee) >= f: return 0
    s += f * len(ee) - sum(ee)
  return s

def splitIntoNums(line):
  words = re.split(r'\s+', line.strip())  # split into words
  return [int(w) for w in words]          # convert to integers

while True:
  d, w = splitIntoNums(sys.stdin.readline())
  if d == 0 and w == 0: break
  e = []
  for y in range(d):
    e.append(splitIntoNums(sys.stdin.readline()))
  print(solve(d, w, e))
