#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def solve(b):
    S = [0]
    lo = 0
    hi = 1
    while True:
        S.append(S[-1] + hi)
        while S[hi] - S[lo] > b:
            lo += 1
        if S[hi] - S[lo] == b:
            break
        hi += 1
    return lo + 1, hi - lo

while True:
    b = int(input())
    if b == 0: break
    lo, n = solve(b)
    print(lo, n)
