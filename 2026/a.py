#!/usr/bin/env python3

# ICPC Asia Japan 2026 Online First-Round Contest
# Problem A: Find the strongest card

while True:
    n = int(input())
    if n == 0:
        break
    cs = list(map(int, input().split()))
    assert len(cs) == n
    m = max(cs, key=lambda c: (c + 13) % 16)
    print(m)
