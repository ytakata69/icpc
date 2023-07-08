#!/usr/bin/env python3

# Problem D - Efficient Problem Set

def solve(n, s):
    s = set(i for i, c in enumerate(s) if c == 'o') # setに変換

    frontier = [{0}]
    count = 0  # 問題数
    while True:
        newfront = []
        for f in frontier:
            if s <= f:        # 必要な得点がすべてカバーされた
                return count
            v = min(s - f)    # まだカバーされていない最小の得点
            rest = n - max(f) # 残り得点
            for w in f:       # カバー済みの各値
                if 0 < v - w <= rest:
                    # fに(v - w)点の問題を加えた集合
                    nf = f | set(e + v - w for e in f)
                    newfront.append(nf)
        frontier = newfront
        count += 1


while True:
    n = int(input())
    if n == 0:
        break
    s = input()
    assert len(s) == n + 1
    print(solve(n, s))
