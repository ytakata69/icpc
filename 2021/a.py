#!/usr/bin/env python3

# Problem A - Marbles Tell Your Lucky Number

def solve(a):
    while sum(1 for x in a if x > 0) > 1:
        m, j = min((x, i) for i, x in enumerate(a) if x > 0)
        a = [x - m if i != j and x > 0 else x for i, x in enumerate(a)]
    return sum(a)

while True:
    a = list(map(int, input().split()))
    assert len(a) == 4
    if sum(a) == 0:
        break
    print(solve(a))
