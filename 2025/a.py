#!/usr/bin/env python3

def solve(n):
    s = 0
    for a in range(1, n + 1):
        for b in range(1, n + 1):
            s += a * b
    return s

while True:
    n = int(input())
    if n == 0:
        break
    s = solve(n)
    print(s)
