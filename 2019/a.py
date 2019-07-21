#!/usr/bin/env python3
# -*- coding: utf-8 -*-

while True:
    n, m = map(int, input().split())
    if n == m == 0: break
    S = [0] * n
    for i in range(m):
        P = map(int, input().split())
        S = [s + p for s, p in zip(S, P)]
    print(max(S))
