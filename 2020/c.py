#!/usr/bin/env python3

# Problem C - Luggage

from math import sqrt, floor

def solve(p):
    for w in range(1, p + 1):
        if w ** 3 > p:
            break
        if p % w != 0:
            continue
        for h in range(floor(sqrt(p // w)), 0, -1):
            if (p // w) % h == 0:
                yield w + h + (p // w // h)
                break

while True:
    p = int(input())
    if p == 0:
        exit()
    print(min(solve(p)))
