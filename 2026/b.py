#!/usr/bin/env python3

# ICPC Asia Japan 2026 Online First-Round Contest
# Problem B: Vending machines

def solve(xs, d):
    n = len(xs)
    nv = 0
    v = -d - 1  # 番兵: 最後の自動販売機
    for i in range(n):
        # 最後の自動販売機でカバーできない
        if v + d < xs[i]:
            nv += 1
            v = min(xs[i] + d, 10 ** 8)
    return nv

while True:
    n, d = map(int, input().split())
    if n == 0:
        break
    xs = list(map(int, input().split()))
    assert len(xs) == n
    print(solve(xs, d))
