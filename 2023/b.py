#!/usr/bin/env python3

# Problem B - Amidakuji

def update(p, h):
    return p - 1 if h == p - 1 else (p + 1 if h == p else p)

def result(x, p, vpos = -1, hpos = 0):
    for i, xi in enumerate(x):
        if i == vpos:
            p = update(p, hpos)
        p = update(p, xi)

    if len(x) == vpos:
        p = update(p, hpos)
    return p

def solve(n, x, p, q):
    if result(x, p) == q:
        return "OK"
    for vpos in range(len(x) + 1):  # 0..m
        for hpos in range(1, n):    # 1..n-1
            if result(x, p, vpos, hpos) == q:
                return f"{hpos} {vpos}"
    return "NG"

while True:
    n, m, p, q = map(int, input().split())
    if n == m == p == q == 0:
        break
    x = list(map(int, input().split()))
    assert len(x) == m
    print(solve(n, x, p, q))
