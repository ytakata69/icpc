#!/usr/bin/env python3

# Problem A - Counting Peaks of Infection

def solve(v):
    return sum(1 for a, b, c in zip(v, v[1:], v[2:]) if a < b and c < b)

while True:
    n = int(input())
    if n == 0:
        break
    v = list(map(int, input().split()))
    assert len(v) == n
    print(solve(v))
