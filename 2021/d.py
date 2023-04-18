#!/usr/bin/env pypy3

# Problem D - Handing out Baloons

def normpair(a, b):
    return (a, b) if a <= b else (b, a)

def solve(bs):
    total = sum(bs)
    dp = {(0, 0)}  # the numbers of baloons processed by two workers
    for b in bs:
        nextdp = set()
        for b1, b2 in dp:
            nextdp.add(normpair(b1 + b, b2))
            nextdp.add(normpair(b1, b2 + b))
        dp.update(nextdp)

    return max(min(b1, b2, total - b1 - b2) for b1, b2 in each(dp))

while True:
    n = int(input())
    if n == 0:
        break
    bs = list(map(int, input().split()))
    assert len(bs) == n
    print(solve(bs))
