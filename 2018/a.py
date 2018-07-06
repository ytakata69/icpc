#!/usr/bin/env python3
# -*- coding: utf-8 -*-

while True:
    n = int(input())
    if n == 0: break
    A = [int(t) for t in input().split()]
    assert len(A) == n

    ave = sum(A) / n
    print(sum(1 for a in A if a <= ave))
