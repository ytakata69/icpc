#!/usr/bin/env python3

# ICPC Asia Japan 2026 Online First-Round Contest
# Problem C: Water remaining

def solve(ds):
    n = len(ds)

    # 満水時の水の高さ = 各区画の深さ
    ls = ds[:]

    # 左端から水を抜く
    level = ds[0]   # 左端の低さ
    for i in range(n):
        level = min(level, ds[i])
        ls[i] -= level
    
    # 右端から水を抜く
    level = ds[-1]
    for i in range(n - 1, -1, -1):
        level = min(level, ds[i])
        ls[i] = min(ls[i], ds[i] - level)
    
    return sum(ls)

while True:
    n = int(input())
    if n == 0:
        break
    ds = list(map(int, input().split()))
    assert len(ds) == n
    print(solve(ds))
