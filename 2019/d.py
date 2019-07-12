#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def solve(m, A, B):
    assert len(A) == len(B) <= 1000
    assert m <= 10000

    diff = [(b - a + m) % m for a, b in zip(A, B)]
    return score(diff, m)

def score(diff, m):
    if len(diff) == 0: return 0
    if len(diff) == 1: return diff[0]
    D = list(zip(diff, range(len(diff))))
    D.sort()
    S = float('inf')
    for d, t in D:
        s = score(diff[:t], m) + score(diff[t + 1:], m)
        if 0 < t < len(diff) - 1:
            s -= diff[t]
        S = min(S, s)
        diff[t] += m
    return S

while True:
    n, m = map(int, input().split())
    if n == m == 0: break

    A = list(map(int, input().split()))
    B = list(map(int, input().split()))
    assert len(A) == len(B) == n

    print(solve(m, A, B))
