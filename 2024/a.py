#!/usr/bin/env python3

# ICPC Domestic Qualifier
# Problem A - Snacks within 300 Yen

def solve(a):
    s = 0
    for ai in a:
        if s + ai <= 300:
            s += ai
    return s

while True:
    n = int(input())
    if n == 0:
        break
    a = list(map(int, input().split()))
    assert len(a) == n
    print(solve(a))
