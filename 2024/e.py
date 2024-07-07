#!/usr/bin/env python3

# ICPC Domestic Qualifier
# Problem E - Colorful Residential Area

def solve(c):
    n = len(c)

    # c[0] == c[r]となる最も右のr, c[-1] == c[l]となる最も左のl
    r = max(i for i in range(n - 1, -1, -1) if c[0] == c[i])
    l = min(i for i in range(0, n) if c[-1] == c[i])
    if l > r:
        return None  # 彩色不可能

    ans = [[0] * n for _ in range(n)]
    px, py = 0, n - 1
    vx, vy = 1, 0
    for dir in range(4):
        hx, hy = vy, -vx  # 法線ベクトル
        for i in range(n):
            x, y = px + vx * i, py + vy * i
            if i < l:
                x += hx * (n - 1 - r)
                y += hy * (n - 1 - r)
            elif r < i:
                x += hx * l
                y += hy * l
            ans[y][x] = c[i]
        px += vx * (n - 1)
        py += vy * (n - 1)
        vx, vy = hx, hy
    return ans

while True:
    n = int(input())
    if n == 0:
        break
    c = list(map(int, input().split()))
    assert len(c) == n
    a = solve(c)
    if a is None:
        print("No")
    else:
        print("Yes")
        for ai in a:
            print(' '.join(map(str, ai)))
