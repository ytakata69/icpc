#!/usr/bin/env python3

# ACM-ICPC 2017 Tsukuba, Japan Online First-Round Contest
# C: A Garden with Ponds

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
  '''the list of the cells that form the fence'''
  ff = e[y][x : x + w] + e[y + d - 1][x : x + w]
  for i in range(y + 1, y + d - 1):
    ff.append(e[i][x])
    ff.append(e[i][x + w - 1])
  return min(ff)

def pond(e, x, y, w, d, f):
  '''the list of the cells that form the pond'''
  ee = []
  for i in range(y + 1, y + d - 1):
    ee.extend(e[i][x + 1 : x + w - 1])
  return f * len(ee) - sum(ee) if max(ee) < f else 0

while True:
  d, w = map(int, input().split())
  if d == 0 and w == 0:
    break
  e = [list(map(int, input().split())) for y in range(d)]
  print(solve(d, w, e))
