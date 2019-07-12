#!/usr/bin/env python3
# -*- coding: utf-8 -*-

while True:
    n, m = map(int, input().split())
    if n == m == 0: break
    s = [0] * n
    for i in range(m):
        P = map(int, input().split())
        for j, p in enumerate(P):
            s[j] += p
    print(max(s))
