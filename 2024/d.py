#!/usr/bin/env python3

# ICPC Domestic Qualifier
# Problem D - A Bug That's Not a Pill Bug

import bisect

def approach(f, t, d):
    """（XまたはY座標のみ）fからtへ距離d以下だけ移動した移動先と残り距離"""
    rd = min(d, abs(f - t))
    return f + rd * (1 if t > f else -1), d - rd

def create_obst_list(xy, idx):
    """同じXまたはY座標の障害物のリストの辞書"""
    obst = {}
    for xyi in xy:
        x, y = xyi[idx], xyi[1 - idx]
        if x not in obst:
            obst[x] = []
        obst[x].append(y)
    for x in obst:
        obst[x].sort()  # 昇順に整列
    return obst

def move(obst, x, y, d, vec):
    """(x,y)からX方向に向きvecに距離dだけ移動"""
    # 同じY座標に障害物なし
    if y not in obst:
        return x + d * vec, 0

    # 二分探索: obst[y][i-1] <= x < obst[y][i] となるiを返す
    i = bisect.bisect(obst[y], x)
    if (vec > 0 and i >= len(obst[y])) or (vec < 0 and i <= 0):
        # 進行方向に障害物なし
        return x + d * vec, 0
    else:
        tx = obst[y][i] - 1 if vec > 0 else obst[y][i - 1] + 1
        return approach(x, tx, d)

def solve(a, b, d, xy):
    obstx = create_obst_list(xy, 0)  # 同じX座標の障害物のリスト
    obsty = create_obst_list(xy, 1)  # 同じY座標〃

    dir = 0  # 0=east, 1=north, 2=west, 3=south
    visited = {}
    while d > 0:
        # 同じ座標・方向に再度到達
        if (a, b, dir) in visited:
            # ループの長さ
            delta = visited[(a, b, dir)] - d
            d %= delta  # 必要なだけループした後の残りの距離
            visited = {}
        else:
            visited[(a, b, dir)] = d

        # 進行方向
        vec = 1 if dir < 2 else -1

        if dir % 2 == 0:  # 東西
            a, d = move(obsty, a, b, d, vec)
        else:  # 南北
            b, d = move(obstx, b, a, d, vec)

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
