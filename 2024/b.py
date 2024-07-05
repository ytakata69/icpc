#!/usr/bin/env python3

# ICPC Domestic Qualifier
# Problem B - Overtaking

def accum(a):
    ac = [a[0]]
    for i in range(1, len(a)):
        ac.append(ac[-1] + a[i])
    return ac

def sign(ab):
    ai, bi = ab
    return -1 if ai < bi else 1

def solve(a, b):
    acc_a = accum(a)
    acc_b = accum(b)
    pairs = [(ai, bi) for ai, bi in zip(acc_a, acc_b) if ai != bi]
    ovtks = []
    for ai, bi in pairs:
        if ovtks == [] or sign(ovtks[-1]) != sign((ai, bi)):
            ovtks.append((ai, bi))
    return len(ovtks) - 1

while True:
    n = int(input())
    if n == 0:
        break
    a = list(map(int, input().split()))
    b = list(map(int, input().split()))
    assert len(a) == len(b) == n
    print(solve(a, b))
