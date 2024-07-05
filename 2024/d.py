#!/usr/bin/env python3

# ICPC Domestic Qualifier
# Problem D - A Bug That's Not a Pill Bug

import bisect

def approach(f, t, d):
    """fからtへ距離d以下だけ移動したときの移動先と残り距離"""
    rd = min(d, abs(f - t))
    return f + rd * (1 if t > f else -1), d - rd

def solve(a, b, d, xy):
    obstx = {}  # 同じX座標の障害物のリスト
    obsty = {}  # 同じY座標の障害物のリスト
    for x, y in xy:
        if x not in obstx:
            obstx[x] = []
        if y not in obsty:
            obsty[y] = []
        obstx[x].append(y)
        obsty[y].append(x)
    for x in obstx:
        obstx[x].sort()  # 昇順に整列
    for y in obsty:
        obsty[y].sort()  # 昇順に整列

    visited = {}
    dir = 0  # 0=east, 1=north, 2=west, 3=south
    while d > 0:
        # 同じ座標・方向に再度到達
        if (a, b, dir) in visited:
            # ループの長さ
            delta = visited[(a, b, dir)] - d
            d %= delta  # 必要なだけループした後の残りの距離
            visited = {}
        else:
            visited[(a, b, dir)] = d

        # 障害物なし
        if dir % 2 == 0 and b not in obsty:
            a += d * (1 if dir == 0 else -1)
            d = 0
        elif dir % 2 == 1 and a not in obstx:
            b += d * (1 if dir == 1 else -1)
            d = 0

        # 障害物あり
        elif dir % 2 == 0:  # 東西
            i = bisect.bisect(obsty[b], a)
            # 進行方向に障害物なし
            if (dir == 0 and i >= len(obsty[b])) or \
               (dir == 2 and i <= 0):
                a += d * (1 if dir == 0 else -1)
                d = 0
            else:
                ta = obsty[b][i] - 1 if dir == 0 else obsty[b][i - 1] + 1
                a, d = approach(a, ta, d)
        else:  # 南北
            i = bisect.bisect(obstx[a], b)
            # 進行方向に障害物なし
            if (dir == 1 and i >= len(obstx[a])) or \
               (dir == 3 and i <= 0):
                b += d * (1 if dir == 1 else -1)
                d = 0
            else:
                tb = obstx[a][i] - 1 if dir == 1 else obstx[a][i - 1] + 1
                b, d = approach(b, tb, d)
        dir = (dir + 1) % 4
    return (a, b)

while True:
    n = int(input())
    if n == 0:
        break
    a, b, d = list(map(int, input().split()))
    xy = [tuple(map(int, input().split())) for _ in range(n)]
    assert all(len(xyi) == 2 for xyi in xy)
    x, y = solve(a, b, d, xy)
    print(x, y)
