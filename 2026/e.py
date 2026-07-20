#!/usr/bin/env python3

# ICPC Asia Japan 2026 Online First-Round Contest
# Problem E: Shopping master

def solve(wine):
    n = len(wine)
    ngem = sum(g for _, g in wine)

    # 安い方から n - ngem 個買えばよい.
    # ただし, その中にgemがなければ, gem付きの最安のものに変える.
    tobuy = max(0, n - ngem)
    wine.sort()
    cost, gem = 0, 0
    for i in range(tobuy):
        c, g = wine[i]
        cost += c
        gem  += g
    if gem == 0:
        mc = wine[tobuy - 1][0] if tobuy > 0 else 0  # 最後のワインの値段
        for i in range(tobuy, n):
            c, g = wine[i]
            if g > 0:
                cost += c - mc
                break
    return cost

while True:
    n = int(input())
    if n == 0:
        break
    wine = [tuple(map(int, input().split())) for _ in range(n)]
    assert all(len(w) == 2 for w in wine)
    print(solve(wine))
