#!/usr/bin/env pypy3

def solve(m, A, B):
    # カウンタ毎の必要な操作回数
    C = [b - a for a, b in zip(A, B)]
    C = [c if c >= 0 else c + m for c in C]

    c0 =  0  # 直前のカウンタの操作回数
    cs = [0] # 直前のカウンタの操作回数がc0, c0+m, c0+2m, ...のときの操作回数
    for i, c in enumerate(C):
        cs1 = []
        for j in range(i + 1):
            j0 = min(j, len(cs) - 1)
            j1 = min(j + 1, len(cs) - 1) if c >= c0 else max(0, j - 1)
            cnt2 = min(cs[j0] + max(0, c - c0 + (j - j0) * m),
                       cs[j1] + max(0, c - c0 + (j - j1) * m))
            cs1.append(cnt2)
        c0 = c
        cs = cs1
    return min(cs)

while True:
    n, m = map(int, input().split())
    if n == m == 0: break

    A = list(map(int, input().split()))
    B = list(map(int, input().split()))
    assert len(A) == len(B) == n

    print(solve(m, A, B))
