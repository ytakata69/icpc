#!/usr/bin/env pypy3

# Problem C - Training Schedule for ICPC

def solve(n, m):
    # mを分割しない場合
    best = n * n - m * m
    # mをd個に分割 (nをd-1個に分割)
    for d in range(2, min(n + 1, m) + 1):
        md = m // d
        r  = m % d
        lose = (md ** 2) * (d - r) + ((md + 1) ** 2) * r
        gain = 1 * (d - 2) + (n - (d - 2)) ** 2
        best = max(best, gain - lose)
    return best

while True:
    n, m = map(int, input().split())
    if n == m == 0:
        break
    print(solve(n, m))
