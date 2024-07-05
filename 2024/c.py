#!/usr/bin/env python3

# ICPC Domestic Qualifier
# Problem C - Honeycomb Distance

def solve(x, y):
    # normalize
    if x < 0:
        x, y = -x, -y
    if y >= 0:
        return x + y
    if x + y >= 0:
        return x
    return abs(y)

n = int(input())
for t in range(n):
    x, y = map(int, input().split())
    print(solve(x, y))
