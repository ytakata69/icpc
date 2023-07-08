#!/usr/bin/env python3

# Problem D - Efficient Problem Set

def included(a, b):
    """a, b are bit-vectors."""
    return (a & ~b) == 0

def notcovered(a, b):
    nc = a & ~b
    i = 0
    while nc != 0:
        if nc & 1:
            return i
        nc >>= 1
        i += 1
    raise

def values(a):
    i = 0
    while a != 0:
        if a & 1:
            yield i
        a >>= 1
        i += 1

def add(a, v):
    r = a
    for w in values(a):
        r |= 1 << (w + v)
    return r

def solve(n, s):
    # 集合をビットベクトルで表す
    s = s.translate(str.maketrans('xo', '01'))  # xo -> 01
    s = int(''.join(reversed(s)), 2)  # to binary int

    frontier = {1}  # ビットベクトルの集合 (1 means {0})
    over = 1 << (n + 1)  # nより大きい数を含む集合はだめ

    count = 0  # 問題数
    while True:
        newfront = set()
        for f in frontier:
            if included(s, f):   # 必要な得点がすべてカバーされた
                return count
            v = notcovered(s, f) # まだカバーされていない最小の得点
            for w in values(f):  # カバー済みの各値
                if w >= v:
                    break
                nf = add(f, v - w)  # (v - w)点の問題を加えた集合
                if nf < over and nf not in frontier:
                    newfront.add(nf)
        frontier = newfront
        count += 1
        #print(','.join(map(lambda v: format(v, 'b'), frontier)))


while True:
    n = int(input())
    if n == 0:
        break
    s = input()
    assert len(s) == n + 1
    print(solve(n, s))
