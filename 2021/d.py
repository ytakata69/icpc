#!/usr/bin/env pypy3

# Problem D - Handing out Baloons

def normpair(a, b):
    return (a, b) if a <= b else (b, a)

def add(dic, t):
    k, v = t
    if k not in dic:
        dic[k] = set()
    dic[k].add(v)

def update(dic1, dic2):
    for k in dic2:
        if k not in dic1:
            dic1[k] = set()
        dic1[k].update(dic2[k])

def each(dic):
    for k in dic:
        for v in dic[k]:
            yield (k, v)

def solve(bs):
    total = sum(bs)
    onethird = total // 3
    dp = {}
    add(dp, (0, 0)) # the numbers of baloons processed by two workers
    for b in bs:
        nextdp = {}
        for b1 in dp:
            for b2 in dp[b1]:
                if b1 < onethird:
                    add(nextdp, normpair(b1 + b, b2))
                if b2 < onethird:
                    add(nextdp, normpair(b1, b2 + b))
        update(dp, nextdp)

    return max(min(b1, b2, total - b1 - b2) for b1, b2 in each(dp))

while True:
    n = int(input())
    if n == 0:
        break
    bs = list(map(int, input().split()))
    assert len(bs) == n
    print(solve(bs))
