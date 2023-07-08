#!/usr/bin/env python3

# Problem A - Which Team Should Receive the Sponsor Prize?

def solve(a):
    _, i = min((abs(ai - 2023), i) for i, ai in enumerate(a))
    return i + 1

while True:
    n = int(input())
    if n == 0:
        break
    a = list(map(int, input().split()))
    assert len(a) == n
    print(solve(a))
