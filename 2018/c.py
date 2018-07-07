#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def solve(b):
    s  = 0
    lo = 1
    for hi in range(1, b+1):
        s += hi
        while s > b:
            s  -= lo
            lo += 1
        if s == b:
            return lo, hi - lo + 1
    raise

while True:
    b = int(input())
    if b == 0: break
    lo, n = solve(b)
    print(lo, n)
