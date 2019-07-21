#!/usr/bin/env python3
# -*- coding: utf-8 -*-

def triseq(l):
    if l == 0:
        yield []
    else:
        for s in triseq(l - 1):
            for d in (-1, 0, 1):
                yield [d] + s

def innerprod(A, B):
    return sum(a * b for a, b in zip(A, B))

def solve(A, W):
    assert len(A) <= 100
    assert len(W) <= 10

    C = set(innerprod(t, W) for t in triseq(len(W)))  # |C| <= 3 ** m

    D = [a for a in A if a not in C]   # D = A - C
    if len(D) == 0:
        return 0

    # candidates of the new weight
    Wx = [abs(D[0] - c) for c in C]    # D[0] - c or c - D[0]
    Wx.sort()
    for w in Wx:
        if all((d + w) in C or (d - w) in C for d in D):
            return w
    return -1

while True:
    n, m = map(int, input().split())
    if n == m == 0: break

    A = list(map(int, input().split()))
    W = list(map(int, input().split()))
    assert len(A) == n and len(W) == m

    print(solve(A, W))

