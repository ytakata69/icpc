#!/usr/bin/env python3

# Problem E - Tampered Records

def update(dic, key, val):
    if key not in dic or dic[key] > val:
        dic[key] = val

def solve(n, xy):
    # 直前のチームの成績(x, y) -> 書き換え数
    dp = {(n - 1, n - 1): 0}

    for x, y in xy:
        newdp = {}
        for px, py in dp:
            c = dp[(px, py)]

            if (py, px) >= (y, x):
                update(newdp, (x, y), c)

            # 両方書き換え
            if py != y and px != x:
                update(newdp, (px, py), c + 2)

            # xのみ書き換え
            if py >= y and px != x:
                update(newdp, (px, y), c + 1)
            if py > y:
                update(newdp, (n - 1, y), c + 1)

            # yのみ書き換え
            if py != y and px >= x:
                update(newdp, (x, py), c + 1)
            if py > 0 and px < x:
                update(newdp, (x, py - 1), c + 1)
        dp = newdp

    return min(dp.values())

while True:
    n = int(input())
    if n == 0:
        break
    xy = [tuple(map(int, input().split())) for _ in range(n)]
    assert all(len(t) == 2 for t in xy)
    print(solve(n, xy))
