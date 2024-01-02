#!/usr/bin/env python3

# Problem B - Amidakuji
# O(m)

def update(p, h):
    """横線hによる水平位置pの次の位置"""
    return p - 1 if p - 1 == h else (p + 1 if p == h else p)

def trace(p, x):
    """水平位置pから横線の列xに沿って進んだときの軌跡"""
    tr = [p]
    for xi in x:
        tr.append(update(tr[-1], xi))
    return tr

def solve(n, x, p, q):
    tp = trace(p, x)  # Pから下に進んだときの軌跡

    if tp[-1] == q:
        return "OK"

    x.reverse()
    tq = trace(q, x)  # Qから上に進んだときの軌跡
    tq.reverse()

    # Pの軌跡とQの軌跡が距離1になる位置を探す
    for i, h in enumerate(zip(tp, tq)):
        hp, hq = h
        if abs(hp - hq) <= 1:
            return f"{min(hp, hq)} {i}"
    return "NG"

while True:
    n, m, p, q = map(int, input().split())
    if n == m == p == q == 0:
        break
    x = list(map(int, input().split()))
    assert len(x) == m
    print(solve(n, x, p, q))
