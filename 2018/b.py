#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def solve(n, m, Fold, Punch):
    # n: 幅, m: 高さ
    Thick = [[1] * n for i in range(m)]  # 紙の厚さ

    # d: 左=1 or 下=2, c: 左端または下端からの距離
    for d, c in Fold:
        m = len(Thick)
        n = len(Thick[0])
        if d == 1:
            for y in range(m):
                if n < 2 * c:
                    Thick[y] += [0] * (2 * c - n)
                for x in range(c):
                    Thick[y][x + c] += Thick[y][x]
                Thick[y] = Thick[y][c:]
        else:
            if m < 2 * c:
                Thick += [[0] * n for i in range(2 * c - m)]
            for y in range(c):
                for x in range(n):
                    Thick[y + c][x] += Thick[y][x]
            Thick = Thick[c:]

    for x, y in Punch:
        print(Thick[y][x])


while True:
    # n: 幅, m: 高さ
    n, m, t, p = [int(t) for t in input().split()]
    if n == 0 and m == 0 and t == 0 and p == 0: break
    Fold = []
    for i in range(t):
        # d: 左=1 or 下=2, c: 左端または下端からの距離
        d, c = [int(t) for t in input().split()]
        Fold.append((d, c))
    Punch = []
    for i in range(p):
        x, y = [int(t) for t in input().split()]
        Punch.append((x, y))

    solve(n, m, Fold, Punch)
