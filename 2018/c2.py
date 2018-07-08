#!/usr/bin/env python3
# -*- coding: utf-8 -*-

from math import sqrt, floor

# sum(l..h) = (h * (h+1) - l * (l-1)) // 2
#           = ((h + l) * (h - l + 1)) // 2
# (h+l) * (h-l+1) == 2 * b の整数解(h,l)のうちh-l+1が最大のものを探せばよい.
# 2 * b == m * n となる2整数m, n (m >= n) を n の大きい順に調べる.
# h+l == m, h-l+1 == n.
# h = (m+n-1)/2, l = (m-n+1)/2 が整数になるものが解.

def solve(b):
    for n in range(floor(sqrt(2 * b)), 0, -1):
        if (2 * b) % n == 0:
            m = (2 * b) // n
            if (m - n + 1) % 2 == 0:
                return (m - n + 1) // 2, n
    raise

while True:
    b = int(input())
    if b == 0: break
    lo, n = solve(b)
    print(lo, n)
